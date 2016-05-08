{-# LANGUAGE QuasiQuotes #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.Map as M
import Data.Int
import Data.DList (DList, singleton)
import Data.Foldable
import Options.Applicative
import System.FilePath

import Slurp
import SummarizeResults

type Commit = String
type TestEnvName = String
type TestName = String

ingest :: ConnectInfo
       -> Commit -> TestEnvName -> M.Map TestName Double -> IO ()
ingest ci commit testEnv tests = do
    conn <- connect ci
    ingest' conn commit testEnv tests
    close conn

ingest' :: Connection
        -> Commit -> TestEnvName -> M.Map TestName Double -> IO Int64
ingest' conn commit testEnv tests = withTransaction conn $ do
    executeMany conn
        [sql| INSERT INTO tests (test_name)
              VALUES (?)
              ON CONFLICT (test_name) DO NOTHING|]
        (map Only $ M.keys tests)

    testIds <- M.fromList <$> query conn
        [sql| SELECT test_name, test_id
              FROM tests
              WHERE test_name IN ? |]
        (Only $ In $ M.keys tests)

    execute conn
        [sql| INSERT INTO commits (commit_sha)
              VALUES (?) |]
        (Only commit)

    [Only commitId] <- query conn
        [sql| SELECT commit_id
              FROM commits
              WHERE commit_sha = ? |]
        (Only commit)

    [Only testEnvId] <- query conn
        [sql| SELECT test_env_id
              FROM test_envs
              WHERE test_env_name = ? |]
        (Only testEnv)

    let results :: [(Int, Int, Int, Double)]
        results = [ (commitId, testEnvId, testId, realToFrac value)
                  | (testName, value) <- M.toList tests
                  , Just testId <- pure $ M.lookup testName testIds
                  ]
    executeMany conn
        [sql| INSERT INTO results (commit_id, test_env_id, test_id, result_value)
              VALUES (?,?,?,?) |]
        results

connInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }


args :: Parser (TestEnvName, [FilePath])
args =
    (,)
      <$> option str (short 'e' <> long "env" <> help "test environment name" <> metavar "ENV")
      <*> some (argument str $ help "log files" <> metavar "FILE")

main :: IO ()
main = do
    (testEnv, files) <- execParser $ info (helper <*> args) mempty
    forM_ files $ \fname -> do
        let commit = takeBaseName fname
        results <- parseResults fname
        ingest connInfo commit testEnv (M.fromList results)