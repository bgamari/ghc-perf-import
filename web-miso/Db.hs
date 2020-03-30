{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db
  ( -- * Basic types
    TestEnv(..)
  , TestName(..)
  , BranchName(..)
  , CommitSha(..)
    -- * Fetch metric results
  , getCommitResults
  , Result(..)
    -- * Fetching test environments
  , getTestEnvs
    -- * Fetching commits
  , Commit(..)
  , fetchCommitsWithPrefix
  ) where

import qualified Data.Map                      as M
import           Data.Aeson hiding (Result)

import           JavaScript.Web.XMLHttpRequest
import           Miso.String

newtype TestEnv = TestEnv { getTestEnv :: Int }
                deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)
newtype TestName = TestName { getTestName :: MisoString }
                 deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype BranchName = BranchName { getBranchName :: MisoString }
                   deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype CommitSha = CommitSha { getCommitSha :: MisoString }
                  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data Result = Result { testName :: !TestName
                     , resultValue :: !Double
                     }
            deriving (Eq, Ord, Show)

instance FromJSON Result where
  parseJSON = withObject "result" $ \o ->
    Result <$> o .: "test_name"
           <*> o .: "result_value"

rootUrl :: JSString
rootUrl = "http://home.smart-cactus.org:8889"

getCommitResults :: CommitSha -> TestEnv -> IO [Result]
getCommitResults sha testEnv = fetchJson url
  where
    url = rootUrl <> "/results_view?commit_sha=eq." <> getCommitSha sha <> "&test_env_id=eq." <> ms (show $ getTestEnv testEnv) <> "&limit=100"

fetchJson :: forall a. FromJSON a => JSString -> IO a
fetchJson url = do
    Just resp <- contents <$> xhrByteString req
    case eitherDecodeStrict resp :: Either String a of
      Left s -> fail s
      Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = url
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getTestEnvs :: IO (M.Map TestEnv MisoString)
getTestEnvs = 
    M.fromList . fmap (\(TestEnvRow a b) -> (a,b)) <$> fetchJson url
  where
    url = rootUrl <> "/test_envs"

data TestEnvRow = TestEnvRow TestEnv JSString

instance FromJSON TestEnvRow where
  parseJSON = withObject "test environment" $ \o ->
    TestEnvRow <$> o .: "test_env_id"
               <*> o .: "test_env_name"

data Commit = Commit { commitSha   :: CommitSha
                     , commitTitle :: Maybe JSString
                     , commitDate  :: Maybe JSString
                     , commitResultsCount :: Int
                     }
            deriving (Eq, Show)

instance FromJSON Commit where
  parseJSON = withObject "commit" $ \o ->
    Commit <$> o .: "commit_sha"
           <*> o .: "commit_title"
           <*> o .: "commit_date"
           <*> o .: "result_count"

fetchCommitsWithPrefix :: TestEnv -> MisoString -> IO [Commit]
fetchCommitsWithPrefix env prefix = fetchJson url
  where
    url = rootUrl <> "/commit_metric_counts"
        <> "?test_env_id=eq." <> ms (show $ getTestEnv env) <> "&" 
        <> "commit_sha=like." <> prefix <> "*"

