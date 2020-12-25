{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RuleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RuleAction
  ( RuleAction
      ( RuleAction',
        RuleActionAllow,
        RuleActionDeny,
        fromRuleAction
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RuleAction = RuleAction' {fromRuleAction :: Core.Text}
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

pattern RuleActionAllow :: RuleAction
pattern RuleActionAllow = RuleAction' "allow"

pattern RuleActionDeny :: RuleAction
pattern RuleActionDeny = RuleAction' "deny"

{-# COMPLETE
  RuleActionAllow,
  RuleActionDeny,
  RuleAction'
  #-}
