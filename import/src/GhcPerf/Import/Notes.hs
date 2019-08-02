{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}

module GhcPerf.Import.Notes (importNotes, NotesRef) where

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

import GhcPerf.Import.Types
import GhcPerf.Import.Utils

listCommitsWithNotes :: FilePath -> NotesRef -> IO [Commit]
listCommitsWithNotes repo notesRef = do
    let args = ["-C", repo, "notes", "--ref", notesRef, "list"]
    mapMaybe (parse . words) . lines <$> readProcess "git" args ""
  where
    parse [_noteCommit, commit] = Just commit
    parse _ = Nothing

type NotesRef = String
type Way = String
type Metric = String

parseNotes :: T.Text -> [(TestEnvName, TestName, Way, Metric, Double)]
parseNotes = mapMaybe (f . T.words) . T.lines
  where
    f [testEnv, testName, way, metric, value]
      | ("", value') : _ <- read $ T.unpack value =
        Just (T.unpack testEnv, T.unpack testName, T.unpack way, T.unpack metric, value')
      | otherwise =
        Nothing

readNotes :: FilePath -> NotesRef -> Commit -> IO [(TestEnvName, TestName, Way, Metric, Double)]
readNotes repo notesRef commit = do
    let cp = (proc "git" [ "-C", repo, "notes", "--ref", notesRef, "show", commit ]) { std_out = CreatePipe }
    (_, Just hdl, _, p) <- createProcess cp
    notes <- parseNotes <$> T.IO.hGetContents hdl
    ExitSuccess <- waitForProcess p
    return notes

importNotes :: Connection -> FilePath -> NotesRef -> IO ()
importNotes conn repo notesRef = do
    commits <- listCommitsWithNotes repo notesRef
    mapM_ ingestCommit commits
  where
    ingestCommit commit = do
      notes <- readNotes repo notesRef commit 
      let testEnvs = M.unionsWith (<>) 
            [ M.singleton testEnv (M.singleton (metric <> "/" <> testName) value)
            | (testEnv, testName, "normal", metric, value) <- notes
            ]

      mapM_ (\(testEnvName, metrics) -> addMetrics conn commit testEnvName metrics) (M.toList testEnvs)

