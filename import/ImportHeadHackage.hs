{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Database.PostgreSQL.Simple
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import GhcPerf.Import.HeadHackage
import GhcPerf.Import.Utils
import GhcPerf.Import.Types

testEnv :: TestEnvName
testEnv = "head-hackage"

args :: Parser (String, Commit, [FilePath])
args =
    (,,)
      <$> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option str (short 'C' <> long "commit" <> help "which commit these logs were computed from")
      <*> some (argument str (help "log files"))


importLog :: Connection -> Commit -> FilePath -> IO ()
importLog conn commit logPath = do
    contents <- TIO.readFile logPath
    let pkg = PackageName $ T.pack $ takeBaseName logPath
        parsed :: Measurements
        parsed = parseLog pkg contents
    print parsed
    void $ addMetrics conn commit testEnv $ toMetrics parsed

main :: IO ()
main = do
    (connString, commit, logFiles) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    ensureTestEnvExists conn testEnv
    mapM_ (importLog conn commit) logFiles

