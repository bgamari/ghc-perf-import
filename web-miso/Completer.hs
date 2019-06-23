{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Completer
  ( Config(..)
  , Completer(..)
  , Model
  , Action
  , newCompleter
  ) where

import Control.Monad.Trans.State.Strict

import Miso
import Miso.String (MisoString, ToMisoString(..))

data Config env a
  = Config { fetchCompletions    :: env -> MisoString -> IO [a]
           , renderCompletion    :: a -> [View (Action a)]
           , toText              :: a -> MisoString
           , minCompletionLength :: Int
           , placeholderText     :: MisoString
           }

data Completer env a
  = Completer { handleCompletionAction :: env -> Action a -> Transition (Action a) (Model a) (Maybe a)
              , initialModel           :: Model a
              , render                 :: Model a -> View (Action a)
              }

data Model a
  = Inactive
  | Loading
  | Completing { currentCompletion :: !(Maybe Int)
               , nCompletions      :: !Int
               , completions       :: [a]
               }
  | Selected a
  deriving (Eq, Show)

data Action a 
  = SetCompletionText MisoString
  | SetCompletions [a]
  | AcceptCompletion a
  | FocusInput
  | MoveUp
  | MoveDown
  | Noop
  deriving (Eq, Show)

newCompleter :: forall env a. Show a => Config env a -> Completer env a
newCompleter Config{..} = 
    Completer { handleCompletionAction = handle
              , initialModel           = Inactive
              , render                 = render
              }
  where
    handle :: env -> Action a -> Transition (Action a) (Model a) (Maybe a)
    handle m (SetCompletionText t)
      | length (fromMisoString t :: String) >= minCompletionLength = do
          put Loading
          scheduleIO $ fmap SetCompletions $ fetchCompletions m t
          return Nothing
      | otherwise = do
          put Inactive
          return Nothing
    handle _ (SetCompletions xs) = do
      put $ Completing { currentCompletion = Nothing 
                       , completions = xs
                       , nCompletions = length xs
                       }
      return Nothing
    handle _ (AcceptCompletion x) = do
      put $ Selected x
      return $ Just x
    handle _ FocusInput = do
      get >>= \case Completing{..} -> put $ Completing { currentCompletion = Nothing, .. }
                    _              -> put Inactive
      return Nothing
    handle _ MoveUp   = mapCurrent pred
    handle _ MoveDown = mapCurrent succ
    handle _ Noop = return Nothing

    mapCurrent :: Monad m => (Int -> Int) -> StateT (Model a) m (Maybe a)
    mapCurrent f = do
      get >>= \case
        Completing{..}
          | Just i <- currentCompletion -> put $ Completing {currentCompletion = clamp $ f i, ..}
          | otherwise                   -> put $ Completing {currentCompletion = clamp 0, ..}
          where clamp :: Int -> Maybe Int
                clamp i
                  | i < 0 = Nothing
                  | i >= nCompletions = Nothing
                  | otherwise = Just i
        _              -> return ()
      return Nothing

    render :: Model a -> View (Action a)
    render model = 
      div_ [class_ "completer"]
      $ [ let valueAttr =
                case model of
                  Selected x -> [value_ $ toText x]
                  Completing{..}
                    | Just i <- currentCompletion -> [value_ $ toText $ completions !! i]
                  _ -> []
           in input_ $ [ placeholder_ placeholderText
                       , onInput SetCompletionText
                       , onKeyUp $ \x -> if | x == arrowDown -> MoveDown
                                            | x == arrowUp   -> MoveUp
                                            | otherwise      -> Noop
                       , onFocus FocusInput
                       , onChange $ const $ 
                           case model of 
                             Completing{..}
                               | Just i <- currentCompletion -> AcceptCompletion $ completions !! i
                             _ -> FocusInput
                       ] ++ valueAttr
        ] ++ results
      where
        results =
          case model of
            Inactive -> []
            Loading -> [ p_ [] [text "loading completions..."] ]
            Completing{..} ->
              let handleKeyUp :: a -> KeyCode -> Action a
                  handleKeyUp c key
                    | key == arrowUp   = MoveUp
                    | key == arrowDown = MoveDown
                    | key == tab       = AcceptCompletion c
                    | otherwise        = Noop
              in
                [ ul_ [ class_ "completer" ]
                  [ li_
                      [ onKeyUp $ handleKeyUp c
                      , onClick $ AcceptCompletion c
                      , class_ $ if Just i == currentCompletion then "active" else ""
                      ]
                      (renderCompletion c)
                  | (i, c) <- zip [0..] completions
                  ]
                ]
            Selected _ -> []

arrowUp, arrowDown, tab :: KeyCode
arrowUp   = KeyCode 0x26
arrowDown = KeyCode 0x28
tab       = KeyCode 0x09
