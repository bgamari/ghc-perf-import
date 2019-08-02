{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GhcPerf.Import.Types where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

newtype Commit = Commit { getCommit :: String }
               deriving (Eq, Ord, Show, FromField, ToField)

newtype TestEnvName = TestEnvName { getTestEnvName :: String }
                    deriving (Eq, Ord, Show, FromField, ToField)

newtype MetricName = MetricName { getMetricName :: String }
                 deriving (Eq, Ord, Show, FromField, ToField)
