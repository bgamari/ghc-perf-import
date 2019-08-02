{-# LANGUAGE QuasiQuotes #-}

import Control.Lens hiding (argument)
import Control.Lens.Regex
import Control.Monad
import Data.Char
import Database.PostgreSQL.Simple
import Data.List (intercalate)
import Data.Semigroup (Semigroup(..))
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import MRX
import Utils
import Types

testEnv :: TestEnvName
testEnv = "head-hackage"

args :: Parser (String, Commit, [FilePath])
args =
    (,,)
      <$> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option str (short 'C' <> long "commit" <> help "which commit these logs were computed from")
      <*> some (argument str (help "log files"))

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

importLog :: Connection -> Commit -> FilePath -> IO ()
importLog conn commit logPath = do
    contents <- TIO.readFile logPath
    let pkg = PackageName $ T.pack $ takeBaseName logPath
        parsed :: Measurements
        parsed = parseLog pkg contents
    print parsed
    ingestLog conn commit parsed

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
    

ingestLog :: Connection -> Commit -> Measurements -> IO ()
ingestLog conn commit (Measurements ms) = 
    void 
    $ addMetrics conn commit testEnv
    $ M.fromList
    $ concat
    [ [ (name "alloc", realToFrac alloc)
      , (name "time", time)
      ]
    | ((PackageName pkgName, ModuleName modName, PassName passName), Metrics alloc time) <- M.toList ms
    , let name s = intercalate "/" [T.unpack pkgName, T.unpack modName, T.unpack passName, s]
    ]

main :: IO ()
main = do
    (connString, commit, logFiles) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    ensureTestEnvExists conn testEnv
    mapM_ (importLog conn commit) logFiles

