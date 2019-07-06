{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import           Control.Lens
import           Data.List
import           Data.Aeson hiding (Result, (.=))
import           Data.Aeson.Types hiding (Result, Pair, (.=))
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Data.JSString.RealFloat

import           Miso hiding (defaultOptions, Result)
import           Miso.Types (mapAction)
import           Miso.String (MisoString, pack, unpack, ms, (<>))

import           Db
import qualified Completer as Comp

data Pair a = Pair a a
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Field1 (Pair a) (Pair a) a a
instance Field2 (Pair a) (Pair a) a a

data Which = Commit1 | Commit2
           deriving (Eq, Ord, Show, Bounded, Enum)

select :: Which -> Lens' (Pair a) a
select Commit1 = _1
select Commit2 = _2


-- | Model
data Model
  = Model
  { _testEnvs :: M.Map TestEnv MisoString
  , _activeTestEnv :: TestEnv
  , _results :: Pair (Maybe (Commit, M.Map TestName Double))
  , _commitCompleters :: Pair (Comp.Model Commit)
  } deriving (Eq, Show)

makeLenses ''Model

-- | Action
data Action
  = FetchTestEnvs
  | SetTestEnvs (M.Map TestEnv MisoString)
  | SetActiveTestEnv TestEnv
  | CommitCompleterAction Which (Comp.Action Commit)
  | SetCommit Which Commit
  | SetResults Which (M.Map TestName Double)
  | NoOp
  deriving (Show, Eq)


-- | Main entry point
main :: IO ()
main = do
    startApp App { model = Model { _testEnvs = mempty 
                                 , _activeTestEnv = TestEnv 0
                                 , _results = Pair Nothing Nothing
                                 , _commitCompleters = 
                                     let m0 = Comp.initialModel commitCompleter
                                      in Pair m0 m0
                                 }
                 , initialAction = FetchTestEnvs
                 , mountPoint = Nothing
                 , ..
                 }
  where
    update = updateModel
    events = defaultEvents
    subs   = []
    view   = viewModel

commitCompleter :: Comp.Completer TestEnv Commit
commitCompleter = 
  Comp.newCompleter 
  $ Comp.Config { fetchCompletions = fetchCommitsWithPrefix
                , renderCompletion = \Commit{..} ->
                    [ span_ [class_ "result-count"] [text $ ms $ show commitResultsCount]
                    , span_ [class_ "commit"] [text $ getCommitSha commitSha]
                    , span_ [class_ "summary"] [text $ fromMaybe "commit title unavailable" commitTitle]
                    ]
                , minCompletionLength = 2
                , placeholderText  = "commit SHA"
                , toText           = getCommitSha . commitSha
                }

getCommitResults' :: TestEnv -> CommitSha -> IO (M.Map TestName Double)
getCommitResults' testEnv commit = do
    results <- getCommitResults commit testEnv
    return $ M.fromList [ (testName x, resultValue x)
                        | x <- results
                        ]

fetchResults :: Which -> Transition Action Model ()
fetchResults which = do
  mbResults <- use $ results . select which
  case mbResults of
    Just (commit, _)  -> do
      env <- use activeTestEnv
      scheduleIO $ SetResults which <$> getCommitResults' env (commitSha commit)
    Nothing -> return ()

updateModel' :: Action -> Transition Action Model ()
updateModel' FetchTestEnvs = do
  scheduleIO $ fmap SetTestEnvs getTestEnvs
updateModel' (SetTestEnvs xs) = do
  testEnvs .= xs
  case M.keys xs of
    x:_ -> activeTestEnv .= x
    []  -> return ()
updateModel' (SetActiveTestEnv env) = do
  activeTestEnv .= env
  fetchResults Commit1
  fetchResults Commit2
updateModel' (SetCommit which commit) = do
  results . select which .= Just (commit, mempty)
  fetchResults which
updateModel' (SetResults which rs) = do
  results . select which . _Just . _2 .= rs
updateModel' (CommitCompleterAction which action) = do
  testEnv <- use activeTestEnv
  zoom (commitCompleters . select which) $ do
    mcommit <- mapAction (CommitCompleterAction which) 
               $ Comp.handleCompletionAction commitCompleter testEnv action
    case mcommit of
      Nothing -> return ()
      Just commit -> scheduleIO $ return $ SetCommit which commit
updateModel' NoOp = return ()

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel action = fromTransition (updateModel' action)

relChange :: Double -> Double -> Double
relChange x y = (y - x) / x

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model{..} = view
  where
    view = div_ [ style_ $ M.fromList
                  [ (pack "text-align", pack "center")
                  , (pack "margin", pack "200px")
                  ]
                ]
      [ h1_ [class_ $ pack "title" ] [ text $ pack "GHC Performance Statistics Comparison" ]

      , div_ []
        [ select_
          [ onChange (SetActiveTestEnv . read . unpack) ]
          [ option_ [ value_ $ ms $ show env ] [ text name ]
          | (env, name) <- M.toList _testEnvs
          ]
        ]

      , label_ []
          [ text "commit 1"
          , fmap (CommitCompleterAction Commit1) $ Comp.render commitCompleter (_commitCompleters ^. _1)
          ]
      , renderCommit (_results ^? _1 . each . _1)

      , label_ []
          [ text "commit 2"
          , fmap (CommitCompleterAction Commit2) $ Comp.render commitCompleter (_commitCompleters ^. _2)
          ]
      , renderCommit (_results ^? _2 . each . _1)

      , case _results of
          Pair (Just (commit1, results1)) (Just (commit2, results2)) ->
            table_ [ class_ $ pack "table is-striped" ]
            [ thead_ [] [
                tr_ []
                [ th_ [] [ text $ pack "test name" ]
                , th_ [] [ text $ getCommitSha $ commitSha commit1 ]
                , th_ [] [ text $ getCommitSha $ commitSha commit2 ]
                , th_ [] [ text $ pack "relative change" ]
                ]
              ]
            , tbody_ []
              [ tr_ [] [ td_ [] [ span_ [class_ "testname"] [text $ getTestName testName] ]
                       , td_ [] [ text $ formatDouble Fixed (Just 3) value1 ]
                       , td_ [] [ text $ formatDouble Fixed (Just 3) value2 ]
                       , let cls
                               | pctChange >  0.1 = "increase"
                               | pctChange < -0.1 = "decrease"
                               | otherwise     = ""
                             delta = relChange value1 value2 * 100
                         in td_ [class_ cls] [ text $ formatDouble Fixed (Just 1) pctChange <> "%" ]
                       ]
              | (testName, delta, (value1, value2)) <- sortedDeltas results1 results2
              , let pctChange = 100 * delta
              ]
            ]
          _ -> div_ [] [ text $ pack "Enter two commits above" ]

      ]
    fmt = formatDouble Fixed (Just 2)
      
    renderCommit (Just commit) =
      span_ []
      [ div_ [] [text $ getCommitSha $ commitSha commit]
      , div_ [] [text $ fromMaybe "the past" $ commitDate commit]
      , div_ [] [text $ fromMaybe "commit title unavailable" $ commitTitle commit]
      ]
    renderCommit Nothing = span_ [] []

sortedDeltas :: Ord a => M.Map a Double -> M.Map a Double -> [(a, Double, (Double, Double))]
sortedDeltas xs ys =
    sortOn (\(_,rel,_) -> rel)
    [ (k, relChange v1 v2, (v1, v2))
    | (k, (v1, v2)) <- M.toList $ M.intersectionWith (,) xs ys
    ]
