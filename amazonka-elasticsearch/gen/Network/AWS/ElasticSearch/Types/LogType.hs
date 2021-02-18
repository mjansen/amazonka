{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.LogType where

import Network.AWS.Prelude

-- | Type of Log File, it can be one of the following:     * INDEX_SLOW_LOGS: Index slow logs contains insert requests that took more time than configured index query log threshold to execute.    * SEARCH_SLOW_LOGS: Search slow logs contains search queries that took more time than configured search query log threshold to execute.
--
--
--
--
data LogType = IndexSlowLogs
             | SearchSlowLogs
                 deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                           Typeable, Generic)

instance FromText LogType where
  parser = takeLowerText >>= \case
    "index_slow_logs"  -> pure IndexSlowLogs
    "search_slow_logs" -> pure SearchSlowLogs
    e ->
      fromTextError
        $  "Failure parsing LogType from value: '"
        <> e
        <> "'. Accepted values: index_slow_logs, search_slow_logs"

instance ToText LogType where
  toText = \case
    IndexSlowLogs  -> "INDEX_SLOW_LOGS"
    SearchSlowLogs -> "SEARCH_SLOW_LOGS"

instance Hashable     LogType
instance NFData       LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
  toJSON = toJSONText

instance FromJSON LogType where
  parseJSON = parseJSONText "LogType"
