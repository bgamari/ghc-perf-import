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

args :: Parser (FilePath, String, NotesRef, Maybe [Commit])
args =
    (,,,)
      <$> option str (short 'd' <> long "directory" <> help "GHC repository path")
      <*> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option str (short 'R' <> long "notes-ref" <> help "Git ref where performance notes can be found")
      <*> (fmap Just commits <|> pure Nothing)
  where
    commits =
      some $ option (Commit <$> str) (short 'C' <> long "commit" <> help "Commits to import (all if omitted")

main :: IO ()
main = do
    (repoPath, connString, notesRef, commits) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    commits' <- case commits of
      Nothing -> listCommitsWithNotes repoPath notesRef
      Just commits' -> pure commits'

    mapM_ (ingestCommit conn repoPath notesRef) commits'

ingestCommit :: Connection -> FilePath -> NotesRef -> Commit -> IO ()
ingestCommit conn repo notesRef commit = do
    notes <- readNotes repo notesRef commit
    let testEnvs = toMetrics notes
    putStrLn $ unlines $ map show notes
    putStrLn $ unlines $ map show $ M.toList testEnvs
    mapM_ (\(testEnvName, metrics) -> addMetrics conn commit testEnvName metrics) (M.toList testEnvs)
