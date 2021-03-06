{-# LANGUAGE RecordWildCards #-}

module SummarizeResults where

import qualified Slurp as Nofib
import qualified TestsuiteResults
import Data.List (isSuffixOf, intercalate)
import qualified Data.Map as M
import Control.Monad.Trans.Writer
import Data.DList (DList, singleton)
import Data.Foldable
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString.Lazy.Char8 as BSL
import GhcPerf.Import.Types

sample :: Real a => [String] -> a -> Writer (DList (MetricName, Double)) ()
sample k v = tell $ singleton (MetricName $ intercalate "/" k, realToFrac v)

parseResults :: FilePath -> IO [(MetricName, Double)]
parseResults path = do
    input <- decompress <$> BSL.readFile path
    let nofibResults = toList $ execWriter
                       $ traverse (uncurry buildNofibResults)
                       $ M.toList $ Nofib.parse_log $ BSL.unpack input
        testsuiteResults = TestsuiteResults.parseResults input
    return $ nofibResults ++ testsuiteResults
  where
    decompress
      | ".xz" `isSuffixOf` path = Lzma.decompress
      | otherwise               = id

-- | The name of a nofib test.
type NofibTest = String

buildNofibResults :: NofibTest -> Nofib.Results -> Writer (DList (MetricName, Double)) ()
buildNofibResults testName (Nofib.Results{..}) = do
    forM_ (M.assocs compile_time)   $ \(k,v) -> sample ["compile-time", testName, k] v
    forM_ (M.assocs compile_allocs) $ \(k,v) -> sample ["compile-allocs", testName, k] v
    forM_ (M.assocs module_size)    $ \(k,v) -> sample ["module-size", testName, k] v
    forM_ binary_size               $ \v     -> sample ["binary-size", testName] v
    forM_ link_time                 $ \v     -> sample ["link-time", testName] v
    sample ["run-time", testName] (geomMean run_time)
    sample ["elapsed-time", testName] (geomMean elapsed_time)
    sample ["mut-time", testName] (geomMean mut_time)
    sample ["mut-elasped-time", testName] (geomMean mut_elapsed_time)
    sample ["gc-time", testName] (geomMean gc_time)
    sample ["allocs", testName] (arithMean allocs)

arithMean :: Real a => [a] -> Float
arithMean xs = (realToFrac $ sum xs) / (realToFrac (length xs))

geomMean :: Real a => [a] -> Float
geomMean xs = (realToFrac $ product xs)**(1/realToFrac (length xs))
