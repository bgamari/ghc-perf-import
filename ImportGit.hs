{-# LANGUAGE QuasiQuotes #-}

import Control.Exception
import Data.Foldable
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.Time.Format
import Data.Time.Clock
import System.Process
import Types

connInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }

main :: IO ()
main = importGit connInfo "ghc"

getCommit :: FilePath -> Commit -> IO UTCTime
getCommit repo commit = do
    let args = ["-C", repo, "show", "--no-patch", "--pretty='%H\t%ct'", commit]
    out <- readProcess "git" args ""
    return $ parseLine out
  where
    parseLine l = let [commit, date] = words l
                      Just date' = parseTimeM True defaultTimeLocale "%s" date
                  in date'

importGit :: ConnectInfo -> FilePath -> IO ()
importGit ci repo = do
    conn <- connect ci
    commits <- query_ conn [sql| SELECT commit_id, commit_sha FROM commits WHERE commit_date IS NULL |]
            :: IO [(Int, Commit)]
    let printExc :: Commit -> SomeException -> IO ()
        printExc commitSha exc = putStrLn $ commitSha++": "++show exc
    forM_ commits $ \(commitId, commitSha) -> handle (printExc commitSha) $ do
        commitDate <- getCommit repo commitSha
        execute conn [sql| UPDATE commits
                          SET commit_date = ?
                          WHERE commit_id = ? |]
                    (commitId, commitDate)
        return ()
