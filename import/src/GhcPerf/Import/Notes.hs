{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module GhcPerf.Import.Notes (listCommitsWithNotes, readNotes, NotesRef, toMetrics) where

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
    parse [_noteCommit, commit] = Just $ Commit commit
    parse _ = Nothing

type NotesRef = String
type Way = T.Text
type Metric = T.Text
newtype TestName = TestName T.Text
                 deriving (Eq, Ord, Show)


parseNotes :: T.Text -> [(TestEnvName, TestName, Way, Metric, Double)]
parseNotes = mapMaybe (f . T.splitOn "\t") . T.lines
  where
    f :: [T.Text] -> Maybe (TestEnvName, TestName, Way, Metric, Double)
    f [testEnv, testName, way, metric, value]
      | (value', "") : _ <- reads $ T.unpack value =
        Just (TestEnvName $ T.unpack testEnv, TestName testName, way, metric, value')
    f _ = Nothing

readNotes :: FilePath -> NotesRef -> Commit -> IO [(TestEnvName, TestName, Way, Metric, Double)]
readNotes repo notesRef (Commit commit) = do
    let cp = (proc "git" [ "-C", repo, "notes", "--ref", notesRef, "show", commit ]) { std_out = CreatePipe }
    (_, Just hdl, _, p) <- createProcess cp
    notes <- parseNotes <$> T.IO.hGetContents hdl
    ExitSuccess <- waitForProcess p
    return notes

toMetrics :: [(TestEnvName, TestName, Way, Metric, Double)] -> M.Map TestEnvName (M.Map MetricName Double)
toMetrics notes =
    M.unionsWith (<>) 
    [ M.singleton testEnv (M.singleton (MetricName $ T.unpack $ metric <> "/" <> testName <> "/" <> way) value)
    | (testEnv, TestName testName, way, metric, value) <- notes
    ]

