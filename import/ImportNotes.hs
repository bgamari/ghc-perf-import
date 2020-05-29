{-# LANGUAGE QuasiQuotes #-}

import Data.Semigroup ((<>))
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import System.Process
import System.Exit
import Options.Applicative

import GhcPerf.Import.Types
import GhcPerf.Import.Utils
import GhcPerf.Import.Notes

args :: Parser (FilePath, String, NotesRef)
args =
    (,,)
      <$> option str (short 'd' <> long "directory" <> help "GHC repository path")
      <*> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option str (short 'R' <> long "notes-ref" <> help "Git ref where performance notes can be found")

main :: IO ()
main = do
    (repoPath, connString, notesRef) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    importNotes conn repoPath notesRef

importNotes :: Connection -> FilePath -> NotesRef -> IO ()
importNotes conn repo notesRef = do
    commits <- listCommitsWithNotes repo notesRef
    mapM_ ingestCommit commits
  where
    ingestCommit commit = do
      notes <- readNotes repo notesRef commit 
      let testEnvs = toMetrics notes
      mapM_ (\(testEnvName, metrics) -> addMetrics conn commit testEnvName metrics) (M.toList testEnvs)
