{-# LANGUAGE QuasiQuotes #-}

import Control.Exception
import Data.Foldable
import qualified Data.Map as M
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.Time.Format
import Data.Time.Clock
import System.Process
import Types
import Options.Applicative

connInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }

type BranchName = String

args :: Parser (FilePath, [BranchName])
args =
    (,)
      <$> option str (short 'd' <> long "directory" <> help "GHC repository path")
      <*> branches
  where
    branches = some (argument str (metavar "REF" <> help "name of branch to import")) <|> pure ["master"]

main :: IO ()
main = do
    (repoPath, branches) <- execParser $ info (helper <*> args) mempty
    conn <- connect connInfo
    importCommits conn repoPath
    mapM_ (importBranch conn repoPath) branches

getCommitInfo :: FilePath -> Commit -> IO (UTCTime, String)
getCommitInfo repo commit = do
    let args = ["-C", repo, "show", "--no-patch", "--pretty=%H\t%ct\n%s", commit]
    parse <$> readProcess "git" args ""
  where
    parse output
      | [l1, title] <- lines output
      = let [commit, date] = words l1
            Just date' = parseTimeM True defaultTimeLocale "%s" date
        in (date', title)
      | otherwise
      = error $ "getCommitInfo: Parse error: "++output

importCommits :: Connection -> FilePath -> IO ()
importCommits conn repo = do
    commits <- query_ conn [sql| SELECT commit_id, commit_sha FROM commits WHERE commit_date IS NULL |]
            :: IO [(Int, Commit)]
    let printExc :: Commit -> SomeException -> IO ()
        printExc commitSha exc = putStrLn $ commitSha++": "++show exc
    forM_ commits $ \(commitId, commitSha) -> handle (printExc commitSha) $ do
        (commitDate, commitTitle) <- getCommitInfo repo commitSha
        execute conn [sql| UPDATE commits
                           SET commit_date = ?,
                               commit_title = ?
                           WHERE commit_id = ? |]
                    (commitDate, commitTitle, commitId)
        return ()

importBranch :: Connection -> FilePath -> String -> IO ()
importBranch conn repo branchName = do
    let args = ["-C", repo, "rev-list", "--topo-order", branchName]
    commits <- lines <$> readProcess "git" args ""

    execute conn
        [sql| INSERT INTO branches (branch_name)
              VALUES (?)
              ON CONFLICT DO NOTHING |]
        (Only branchName)

    [Only branchId] <- query conn
        [sql| SELECT branch_id
              FROM branches
              WHERE branch_name = ? |]
        (Only branchName)
        :: IO [Only Int]

    executeMany conn
        [sql| INSERT INTO commits (commit_sha)
              VALUES (?)
              ON CONFLICT DO NOTHING
            |]
        (map Only commits)

    commitIds <- M.fromList <$> query conn
                                      [sql|
                                          SELECT commit_sha, commit_id
                                          FROM commits
                                          WHERE commit_sha IN ?
                                          |]
                                      (Only $ In commits)
              :: IO (M.Map Commit Int)

    execute conn [sql|
                     DELETE FROM branch_commits
                     WHERE branch_id = ?
                     |]
                 (Only branchId)
    n <- executeMany conn
                      [sql|
                          INSERT INTO branch_commits (branch_id, commit_id, sequence_n)
                          VALUES (?,?,?)
                          |]
                      [ (branchId, commitId, negate n)
                      | (n, commit) <- zip [0::Int ..] commits
                      , Just commitId <- pure $ M.lookup commit commitIds
                      ]
    putStrLn $ "Imported "++show n++" commits on branch "++show branchName
    return ()
