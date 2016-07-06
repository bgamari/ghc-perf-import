{-# LANGUAGE RecordWildCards #-}

module SummarizeResults where

import Slurp
import Data.List (isSuffixOf, intercalate)
import qualified Data.Map as M
import Control.Monad.Trans.Writer
import Data.DList (DList, singleton)
import Data.Foldable
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString.Lazy.Char8 as BSL

sample :: Real a => [String] -> a -> Writer (DList (String, Double)) ()
sample k v = tell $ singleton (intercalate "/" k, realToFrac v)

parseResults :: FilePath  -> IO [(String, Double)]
parseResults path = do
    res <- parse_log . BSL.unpack . decompress <$> BSL.readFile path
    return $ toList $ execWriter $ traverse (uncurry buildResults) $ M.toList res
  where
    decompress
      | ".xz" `isSuffixOf` path = Lzma.decompress
      | otherwise               = id

-- | The name of a nofib test.
type NofibTest = String

buildResults :: NofibTest -> Results -> Writer (DList (String, Double)) ()
buildResults testName (Results{..}) = do
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
