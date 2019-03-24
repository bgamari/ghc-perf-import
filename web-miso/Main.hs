{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Data.Aeson hiding (Result)
import           Data.Aeson.Types hiding (Result)
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest

import           Miso                          hiding (defaultOptions, Result)
import           Miso.String

import           Db

-- | Model
data Model
  = Model
  { results :: Maybe [Result]
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchResults CommitSha
  | SetResults [Result]
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  startApp App { model = Model Nothing
               , initialAction = NoOp
               , mountPoint = Nothing
               , ..
               }
    where
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchResults commit) m = m <# do
  SetResults <$> getCommitResults commit (TestEnv 1)
updateModel (SetResults results) m =
  noEff m { results = Just results }
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
        , onChange (FetchResults . CommitSha)
        ]

      , case results of
          Nothing -> div_ [] [ text $ pack "No data" ]
          Just results ->
            table_ [ class_ $ pack "table is-striped" ] [
              thead_ [] [
                tr_ [] [
                  th_ [] [ text $ pack "test name" ]
                ]
              ]
            , tbody_ [] $ [
                tr_ [] [ td_ [] [ text $ getTestName testName ] ]
              | Result{..} <- results
              ]
            ]
          ]

