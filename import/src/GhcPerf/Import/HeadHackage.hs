{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}

module GhcPerf.Import.HeadHackage where

import Control.Lens hiding (argument)
import Control.Lens.Regex
import Control.Monad
import Data.Char
import Database.PostgreSQL.Simple
import Data.List (intercalate)
import Data.Semigroup (Semigroup(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import GhcPerf.Import.HeadHackage.MRX
import GhcPerf.Import.Types
import GhcPerf.Import.Utils

newtype PackageName = PackageName T.Text
  deriving (Show, Eq, Ord)

newtype ModuleName = ModuleName T.Text
  deriving (Show, Eq, Ord)

newtype PassName = PassName T.Text
  deriving (Show, Eq, Ord)

newtype Measurements = Measurements (M.Map (PackageName, ModuleName, PassName) Metrics)
  deriving (Show)

instance Monoid Measurements where
    mempty = Measurements M.empty

instance Semigroup Measurements where
    Measurements a <> Measurements b = Measurements (M.unionWith (<>) a b)

data Metrics = Metrics { allocations :: Integer, time :: Double }
  deriving (Show)

instance Semigroup Metrics where
    a <> b = Metrics { allocations = allocations a + allocations b
                     , time = time a + time b
                     }

parseLog :: PackageName -> T.Text -> Measurements
parseLog pkg =
    foldMapOf (regex re . groups) f . sanitize
  where
    sanitize = T.filter isAscii -- due to https://github.com/ChrisPenner/lens-regex-pcre/issues/4
    re = [mrx|(*UTF)^([\w\d/ ]+) \[([\w\d\.]+)\]: alloc=(\d+) time=(\d+\.\d+)|]
    f grps
      | [pass,mod,alloc,time] <- grps =
        let alloc' = read' $ T.unpack alloc
            time' = read' $ T.unpack time

            read' :: Read a => String -> a
            read' s
              | (x, ""):_ <- reads s = x
              | otherwise = error $ "failed to parse: "<>show grps
         in Measurements $ M.singleton (pkg, ModuleName mod, PassName pass) (Metrics alloc' time')
    
toMetrics :: Measurements -> M.Map TestName Double
toMetrics (Measurements ms) = 
    M.fromList
    $ concat
    [ [ (name "alloc", realToFrac alloc)
      , (name "time", time)
      ]
    | ((PackageName pkgName, ModuleName modName, PassName passName), Metrics alloc time) <- M.toList ms
    , let name s = intercalate "/" [T.unpack pkgName, T.unpack modName, T.unpack passName, s]
    ]

