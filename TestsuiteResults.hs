{-# LANGUAGE OverloadedStrings #-}
module TestsuiteResults where

import Data.Char
import Data.List
import Control.Monad (void)
import Data.Maybe
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

parseResults :: BSL.ByteString -> [(String, Double)]
parseResults = mapMaybe parseResult . BSL.lines

parseResult :: BSL.ByteString -> Maybe (String, Double)
parseResult s
  | A.Done _ r <- A.parse (fmap Just parseResult') s
  = r
  | otherwise = Nothing

parseResult' :: Parser (String, Double)
parseResult' = do
    skipSpace
    void $ string "Expected"
    skipSpace
    testName <- takeTill (== '(')
    void $ char '('
    way <- takeTill (== ')')
    void $ char ')'
    skipSpace
    metric <- takeTill (== ':')
    void $ char ':'
    skipSpace
    value <- double
    void $ skipWhile (/= '\n')
    let normalize ' ' = '-'
        normalize c   = c
        metricName =
            intercalate "/" [ BS.unpack testName
                            , BS.unpack way
                            , map normalize $ BS.unpack metric
                            ]
    return (metricName, value)
