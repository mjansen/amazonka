{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OperatorType
  ( OperatorType
      ( OperatorType',
        OperatorTypeEQ,
        OperatorTypeLT,
        OperatorTypeGT,
        OperatorTypeLE,
        OperatorTypeGE,
        OperatorTypeIN,
        OperatorTypeBetween,
        fromOperatorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

newtype OperatorType = OperatorType' {fromOperatorType :: Core.Text}
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

pattern OperatorTypeEQ :: OperatorType
pattern OperatorTypeEQ = OperatorType' "eq"

pattern OperatorTypeLT :: OperatorType
pattern OperatorTypeLT = OperatorType' "lt"

pattern OperatorTypeGT :: OperatorType
pattern OperatorTypeGT = OperatorType' "gt"

pattern OperatorTypeLE :: OperatorType
pattern OperatorTypeLE = OperatorType' "le"

pattern OperatorTypeGE :: OperatorType
pattern OperatorTypeGE = OperatorType' "ge"

pattern OperatorTypeIN :: OperatorType
pattern OperatorTypeIN = OperatorType' "in"

pattern OperatorTypeBetween :: OperatorType
pattern OperatorTypeBetween = OperatorType' "between"

{-# COMPLETE
  OperatorTypeEQ,
  OperatorTypeLT,
  OperatorTypeGT,
  OperatorTypeLE,
  OperatorTypeGE,
  OperatorTypeIN,
  OperatorTypeBetween,
  OperatorType'
  #-}
