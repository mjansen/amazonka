{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ComparisonOperator where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ComparisonOperator
  = GreaterThanOrEqualToThreshold
  | GreaterThanThreshold
  | LessThanOrEqualToThreshold
  | LessThanThreshold
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ComparisonOperator where
  parser =
    takeLowerText >>= \case
      "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
      "greaterthanthreshold" -> pure GreaterThanThreshold
      "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
      "lessthanthreshold" -> pure LessThanThreshold
      e ->
        fromTextError $
          "Failure parsing ComparisonOperator from value: '" <> e
            <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperator where
  toText = \case
    GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
    GreaterThanThreshold -> "GreaterThanThreshold"
    LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
    LessThanThreshold -> "LessThanThreshold"

instance Hashable ComparisonOperator

instance NFData ComparisonOperator

instance ToByteString ComparisonOperator

instance ToQuery ComparisonOperator

instance ToHeader ComparisonOperator

instance FromXML ComparisonOperator where
  parseXML = parseXMLText "ComparisonOperator"