{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Int
import Control.Monad (void)
import Data.DList (DList, singleton)
import Data.Foldable
import Options.Applicative
import System.FilePath
import Control.Exception

import GhcPerf.Import.Types
import GhcPerf.Import.Utils
import Slurp
import SummarizeResults

args :: Parser (TestEnvName, String, S.Set FilePath)
args =
    (,,)
      <$> option (TestEnvName <$> str) (short 'e' <> long "env" <> help "test environment name" <> metavar "ENV")
      <*> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> fmap S.fromList (some $ argument str $ help "log files" <> metavar "FILE")

getFileCommit :: FilePath -> Commit
getFileCommit path
    -- abcd123.log/validate.xz
  | takeFileName path == "validate.xz" = Commit $ takeBaseName $ takeDirectory path
    -- just plain abcd123.log
  | otherwise = Commit $ dropExtensions $ takeFileName path

findMissingCommits :: Connection -> TestEnvName -> S.Set Commit -> IO (S.Set Commit)
findMissingCommits conn (TestEnvName testEnv) commits =
    S.fromList . map (\(Only x) -> x) <$> query conn
        [sql| SELECT x.column1
              FROM ? as x
              WHERE NOT EXISTS (
                  SELECT commit_sha
                  FROM results
                  JOIN test_envs ON (results.test_env_id = test_envs.test_env_id)
                  JOIN commits ON (results.commit_id = commits.commit_id)
                  WHERE test_envs.test_env_name = ?
                    AND commits.commit_sha = x.column1
              )
            |]
        (Values ["text"] (map (Only . getCommit) $ S.toList commits), testEnv)

main :: IO ()
main = do
    (testEnv, connString, files) <- execParser $ info (helper <*> args) mempty
    let printExc :: String -> SomeException -> IO ()
        printExc fname exc = putStrLn $ fname++": "++show exc

    conn <- connectPostgreSQL $ BS.pack connString
    let commitFiles :: M.Map Commit FilePath
        commitFiles = foldMap (\fname -> M.singleton (getFileCommit fname) fname) files
    missing <- findMissingCommits conn testEnv (M.keysSet commitFiles)
    let toImport :: M.Map Commit FilePath
        toImport = commitFiles `M.intersection` M.fromSet (const ()) missing

    putStrLn $ "Going to import "++show (M.size toImport)++" of "++show (S.size files)++" commits"
    forM_ toImport $ \fname -> handle (printExc fname) $ do
        let commit = getFileCommit fname
        results <- M.fromList <$> parseResults fname
        putStrLn $ getCommit commit++": "++show (M.size results)++" results"
        void $ addMetrics conn commit testEnv results

    close conn
