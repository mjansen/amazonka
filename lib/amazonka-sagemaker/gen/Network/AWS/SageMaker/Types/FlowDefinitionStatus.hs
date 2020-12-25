{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionStatus
  ( FlowDefinitionStatus
      ( FlowDefinitionStatus',
        FlowDefinitionStatusInitializing,
        FlowDefinitionStatusActive,
        FlowDefinitionStatusFailed,
        FlowDefinitionStatusDeleting,
        fromFlowDefinitionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FlowDefinitionStatus = FlowDefinitionStatus'
  { fromFlowDefinitionStatus ::
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

pattern FlowDefinitionStatusInitializing :: FlowDefinitionStatus
pattern FlowDefinitionStatusInitializing = FlowDefinitionStatus' "Initializing"

pattern FlowDefinitionStatusActive :: FlowDefinitionStatus
pattern FlowDefinitionStatusActive = FlowDefinitionStatus' "Active"

pattern FlowDefinitionStatusFailed :: FlowDefinitionStatus
pattern FlowDefinitionStatusFailed = FlowDefinitionStatus' "Failed"

pattern FlowDefinitionStatusDeleting :: FlowDefinitionStatus
pattern FlowDefinitionStatusDeleting = FlowDefinitionStatus' "Deleting"

{-# COMPLETE
  FlowDefinitionStatusInitializing,
  FlowDefinitionStatusActive,
  FlowDefinitionStatusFailed,
  FlowDefinitionStatusDeleting,
  FlowDefinitionStatus'
  #-}
