{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GitLabHook
  ( hookServer
  , Hook
    -- * Events
  , Event(..)
  , PushEvent(..)
  , JobEvent(..)
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import GhcPerf.Import.Types
import Servant
import Servant.Server
import qualified Data.Text as T

data PushEvent = PushEvent { pushProjectId :: Int
                           , pushBeforeSha :: T.Text
                           , pushAfterSha  :: T.Text
                           }

instance FromJSON PushEvent where
  parseJSON = withObject "push event" $ \o ->
    PushEvent
      <$> ((o .: "project") >>= (.: "id"))
      <*> o .: "before"
      <*> o .: "after"

data JobEvent = JobEvent  { jobId :: Int
                          , jobBeforeSha :: T.Text
                          , jobAfterSha :: T.Text
                          }

instance FromJSON JobEvent where
  parseJSON = withObject "job event" $ \o ->
    JobEvent
      <$> o .: "job_id"
      <*> o .: "before_sha"
      <*> o .: "sha"

data Event = PushEvent' PushEvent
           | JobEvent' JobEvent

instance FromJSON Event where
  parseJSON val = withObject "event" f val
    where
      f o = do
        kind <- o .: "object_kind"
        case kind :: T.Text of
          "push" -> PushEvent' <$> parseJSON val
          "job"  -> JobEvent' <$> parseJSON val

type Hook = "event" :> ReqBody '[ JSON ] Event :> Post '[ JSON ] NoContent

hookServer :: (Event -> IO ()) -> Server Hook
hookServer f = handleHook
  where --handleHook :: Event -> _ NoContent
        handleHook event = do
          liftIO (f event)
          return NoContent
