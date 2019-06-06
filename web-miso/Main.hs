{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Data.Aeson hiding (Result)
import           Data.Aeson.Types hiding (Result)
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Data.JSString.RealFloat

import           Miso                          hiding (defaultOptions, Result)
import           Miso.String

import           Db

-- | Model
data Model
  = Model
  { results1 :: Maybe (M.Map TestName Double)
  , results2 :: Maybe (M.Map TestName Double)
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchResults1 CommitSha
  | FetchResults2 CommitSha
  | SetResults1 (M.Map TestName Double)
  | SetResults2 (M.Map TestName Double)
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  startApp App { model = Model Nothing Nothing
               , initialAction = NoOp
               , mountPoint = Nothing
               , ..
               }
    where
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel

getCommitResults' :: CommitSha -> IO (M.Map TestName Double)
getCommitResults' commit = do
    results <- getCommitResults commit (TestEnv 1)
    return $ M.fromList [ (testName x, resultValue x)
                        | x <- results
                        ]

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchResults1 commit) m = m <# do
  SetResults1 <$> getCommitResults' commit
updateModel (FetchResults2 commit) m = m <# do
  SetResults2 <$> getCommitResults' commit
updateModel (SetResults1 results) m =
  noEff m { results1 = Just results }
updateModel (SetResults2 results) m =
  noEff m { results2 = Just results }
updateModel NoOp m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view = div_ [ style_ $ M.fromList
                  [ (pack "text-align", pack "center")
                  , (pack "margin", pack "200px")
                  ]
                ]
      [ h1_ [class_ $ pack "title" ] [ text $ pack "Miso XHR Example" ]

      , input_
        [ placeholder_ "commit"
        , onChange (FetchResults1 . CommitSha)
        ]
      , input_
        [ placeholder_ "commit"
        , onChange (FetchResults2 . CommitSha)
        ]

      , case (,) <$> results1 <*> results2 of
          Nothing -> div_ [] [ text $ pack "No data" ]
          Just (results1, results2) ->
            table_ [ class_ $ pack "table is-striped" ]
            [ thead_ [] [
                tr_ []
                [ th_ [] [ text $ pack "test name" ]
                , th_ [] [ text $ pack "result1" ]
                , th_ [] [ text $ pack "result2" ]
                , th_ [] [ text $ pack "relative change" ]
                ]
              ]
            , tbody_ []
              [ tr_ [] [ td_ [] [ span_ [class_ "testname"] [text $ getTestName testName] ]
                       , td_ [] [ text $ formatDouble Fixed (Just 3) value1 ]
                       , td_ [] [ text $ formatDouble Fixed (Just 3) value2 ]
                       , td_ [] [ text $ formatDouble Fixed (Just 1) (100 * (value2 - value1) / value1) <> "%" ]
                       ]
              | (testName, (value1, value2)) <- M.toList $ M.intersectionWith (,) results1 results2
              ]
            ]
          ]
    fmt = formatDouble Fixed (Just 2)
