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
import           Data.Aeson hiding (Result, (.=))
import           Data.Aeson.Types hiding (Result, Pair, (.=))
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Data.JSString.RealFloat

import           Miso hiding (defaultOptions, Result)
import           Miso.Types (mapAction)
import           Miso.String

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
  , _results :: Pair (Maybe (CommitSha, M.Map TestName Double))
  , _commitCompleters :: Pair (Comp.Model Commit)
  } deriving (Eq, Show)

makeLenses ''Model

-- | Action
data Action
  = FetchTestEnvs
  | SetTestEnvs (M.Map TestEnv MisoString)
  | SetActiveTestEnv TestEnv
  | CommitCompleterAction Which (Comp.Action Commit)
  | SetCommit Which CommitSha
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

commitCompleter :: Comp.Completer Commit
commitCompleter = 
  Comp.newCompleter 
  $ Comp.Config { fetchCompletions = fetchCommitsWithPrefix
                , renderCompletion = \Commit{..} ->
                    [ span_ [class_ "commit"] [text $ getCommitSha commitSha]
                    , span_ [class_ "summary"] [text commitTitle]
                    ]
                , minCompletionLength = 3
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
  Just (commit, _) <- use $ results . select which
  env <- use activeTestEnv
  scheduleIO $ SetResults which <$> getCommitResults' env commit

updateModel' :: Action -> Transition Action Model ()
updateModel' FetchTestEnvs = do
  scheduleIO $ fmap SetTestEnvs getTestEnvs
updateModel' (SetTestEnvs xs) = do
  testEnvs .= xs
updateModel' (SetActiveTestEnv env) = do
  activeTestEnv .= env
  fetchResults Commit1
  fetchResults Commit2
updateModel' (SetCommit which commit) = do
  results . select which .= Just (commit, mempty)
  fetchResults which
updateModel' (SetResults which rs) = do
  results . select which . _Just . _2 .= rs
updateModel' (CommitCompleterAction which action) = zoom (commitCompleters . select which) $ do
  mcommit <- mapAction (CommitCompleterAction which) $ Comp.handleCompletionAction commitCompleter action
  case mcommit of
    Nothing -> return ()
    Just commit -> scheduleIO $ return $ SetCommit which (commitSha commit)
updateModel' NoOp = return ()

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel action = fromTransition (updateModel' action)

{-
updateModel FetchTestEnvs m = m <# do
  SetTestEnvs <$> getTestEnvs
updateModel (SetTestEnvs xs) m =
  noEff m { testEnvs = xs }
updateModel (SetActiveTestEnv xs) m = m <# return FetchResults
updateModel (SetCommit1 commit) m = 
  let Pair _ x = results m
  in m { results = Pair (Just (commit, mempty)) x } <# return FetchResults
updateModel (SetCommit2 commit) m = 
  let Pair x _ = results m
  in m { results = Pair x (Just (commit, mempty)) } <# return FetchResults
updateModel (Commit1CompleterAction action) = do
  (commit, _) <- Comp.handleCompletionAction action
  scheduleIO $ return $ SetCommit1 commit
updateModel FetchResults m = m <#
  let f (commit, _) = do x <- getCommitResults' (activeTestEnv m) commit
                         return (commit, x)
  in SetResults <$> traverse (traverse f) (results m)
updateModel (SetResults results') m =
  noEff m { results = results' }
updateModel NoOp m = noEff m
-}

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

      , select_
        [ onChange (SetActiveTestEnv . read . unpack) ]
        [ option_ [ value_ $ ms $ show env ] [ text name ]
        | (env, name) <- M.toList _testEnvs
        ]

      , fmap (CommitCompleterAction Commit1) $ Comp.render commitCompleter (_commitCompleters ^. _1)
      , fmap (CommitCompleterAction Commit2) $ Comp.render commitCompleter (_commitCompleters ^. _2)

      , case _results of
          Pair (Just (commit1, results1)) (Just (commit2, results2)) ->
            table_ [ class_ $ pack "table is-striped" ]
            [ thead_ [] [
                tr_ []
                [ th_ [] [ text $ pack "test name" ]
                , th_ [] [ text $ getCommitSha commit1 ]
                , th_ [] [ text $ getCommitSha commit2 ]
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
          _ -> div_ [] [ text $ pack "Enter two commits above" ]

      ]
    fmt = formatDouble Fixed (Just 2)
