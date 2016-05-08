{-# LANGUAGE QuasiQuotes #-}

import Data.List.Split
import Data.Foldable
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.Time.Format
import Data.Time.Clock
import System.Process
import qualified Data.Map as M
import Types

connInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }

main :: IO ()
main = importGit connInfo "ghc"

getCommits :: FilePath -> [Commit] -> IO (M.Map Commit UTCTime)
getCommits repo commits = do
    let args = ["-C", repo, "show", "--no-patch", "--pretty='%H\t%ct'"]++commits
    out <- readProcess "git" args ""
    return $ M.fromList $ map parseLine $ lines out
  where
    parseLine l = let [commit, date] = words l
                      Just date' = parseTimeM True defaultTimeLocale "%s" date
                  in (commit, date')

importGit :: ConnectInfo -> FilePath -> IO ()
importGit ci repo = do
    conn <- connect ci
    commits <- query_ conn [sql| SELECT commit_id, commit_sha FROM commits WHERE commit_date IS NULL |]
            :: IO [(Int, Commit)]
    commitInfo <- foldMap (getCommits repo) (chunksOf 100 $ map snd commits)
    forM_ commits $ \(commitId, commitSha) -> do
        case M.lookup commitSha commitInfo of
          Just commitDate -> do
            execute conn [sql| UPDATE commits
                              SET commit_date = ?
                              WHERE commit_id = ? |]
                        (commitId, commitDate)
            return ()
          Nothing -> return ()
