{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bifunctor
import Data.Int
import Data.Word
import Control.Monad (void)
import Data.DList (DList, singleton)
import Data.Foldable
import Options.Applicative
import System.FilePath
import Control.Exception

import GhcPerf.Import.Types
import GhcPerf.Import.Utils
import qualified Data.Csv as Csv

readFileTsv :: FilePath -> IO [(String, Double)]
readFileTsv path = either err toList . Csv.decodeWith opts Csv.NoHeader <$> BSL.readFile path
  where
    err e = error $ path ++ ": failed to read TSV: " ++ show e
    opts :: Csv.DecodeOptions
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = tabChar }

tabChar :: Word8
tabChar = 9

args :: Parser (TestEnvName, Commit, String, S.Set FilePath)
args =
    (,,,)
      <$> option (TestEnvName <$> str) (short 'e' <> long "env" <> help "test environment name" <> metavar "ENV")
      <*> option (Commit <$> str) (short 'C' <> long "commit" <> help "Commit")
      <*> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> fmap S.fromList (some $ argument str $ help "log files" <> metavar "FILE")

main :: IO ()
main = do
    (testEnv, commit, connString, files) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString

    let printExc :: String -> SomeException -> IO ()
        printExc fname exc = putStrLn $ fname++": "++show exc
    forM_ files $ \fname -> handle (printExc fname) $ do
        results <- M.fromList . map (first MetricName) <$> readFileTsv fname
        putStrLn $ getCommit commit++": "++show (M.size results)++" results"
        void $ addMetrics conn commit testEnv results

    close conn
