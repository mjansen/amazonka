{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        ComparisonOperatorGreaterThan,
        ComparisonOperatorLessThan,
        ComparisonOperatorEqualTo,
        fromComparisonOperator
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The comparison operator of a notification. Currently the service supports the following operators:
--
-- @GREATER_THAN@ , @LESS_THAN@ , @EQUAL_TO@
newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ComparisonOperatorGreaterThan :: ComparisonOperator
pattern ComparisonOperatorGreaterThan = ComparisonOperator' "GREATER_THAN"

pattern ComparisonOperatorLessThan :: ComparisonOperator
pattern ComparisonOperatorLessThan = ComparisonOperator' "LESS_THAN"

pattern ComparisonOperatorEqualTo :: ComparisonOperator
pattern ComparisonOperatorEqualTo = ComparisonOperator' "EQUAL_TO"

{-# COMPLETE
  ComparisonOperatorGreaterThan,
  ComparisonOperatorLessThan,
  ComparisonOperatorEqualTo,
  ComparisonOperator'
  #-}
