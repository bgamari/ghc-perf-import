{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Db where

import           Data.Aeson hiding (Result)
import           Data.Aeson.Types hiding (Result)

import           JavaScript.Web.XMLHttpRequest
import           Miso.String

newtype TestEnv = TestEnv { getTestEnv :: Int }
                deriving (Eq, Ord, Show, FromJSON, ToJSON)
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

getCommitResults :: CommitSha -> TestEnv -> IO [Result]
getCommitResults sha testEnv = do
    Just resp <- contents <$> xhrByteString req
    case eitherDecodeStrict resp :: Either String [Result] of
      Left s -> error s
      Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack "http://home.smart-cactus.org:8889/results_view?commit_sha=eq." <> getCommitSha sha <> "&test_env_id=eq." <> ms (show $ getTestEnv testEnv) <> "&limit=100"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
