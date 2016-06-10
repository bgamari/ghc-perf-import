{-# LANGUAGE RecordWildCards #-}

module SummarizeResults where

import Slurp
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Control.Monad.Trans.Writer
import Data.DList (DList, singleton)
import Data.Foldable
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString.Lazy.Char8 as BSL

sample :: Real a => String -> a -> Writer (DList (String, Double)) ()
sample k v = tell $ singleton (k, realToFrac v)

parseResults :: FilePath  -> IO [(String, Double)]
parseResults path = do
    res <- parse_log . BSL.unpack . decompress <$> BSL.readFile path
    return $ toList $ execWriter $ traverse buildResults res
  where
    decompress
      | ".xz" `isSuffixOf` path = Lzma.decompress
      | otherwise               = id

buildResults :: Results -> Writer (DList (String, Double)) ()
buildResults (Results{..}) = do
    forM_ (M.assocs compile_time)   $ \(k,v) -> sample ("compile-time/"++k) v
    forM_ (M.assocs compile_allocs) $ \(k,v) -> sample ("compile-allocs/"++k) v
    forM_ (M.assocs module_size)    $ \(k,v) -> sample ("module-size/"++k) v
    forM_ binary_size               $ \v     -> sample "binary-size" v
    forM_ link_time                 $ \v     -> sample "link-time" v
    sample "run-time" (geomMean run_time)
    sample "elapsed-time" (geomMean elapsed_time)
    sample "mut-time" (geomMean mut_time)
    sample "mut-elasped-time" (geomMean mut_elapsed_time)
    sample "gc-time" (geomMean gc_time)
    sample "allocs" (arithMean allocs)

arithMean :: Real a => [a] -> Float
arithMean xs = (realToFrac $ sum xs) / (realToFrac (length xs))

geomMean :: Real a => [a] -> Float
geomMean xs = (realToFrac $ product xs)**(1/realToFrac (length xs))
