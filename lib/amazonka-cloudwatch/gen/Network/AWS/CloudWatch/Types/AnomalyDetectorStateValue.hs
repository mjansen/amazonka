{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
  ( AnomalyDetectorStateValue
      ( AnomalyDetectorStateValue',
        AnomalyDetectorStateValuePendingTraining,
        AnomalyDetectorStateValueTrainedInsufficientData,
        AnomalyDetectorStateValueTrained,
        fromAnomalyDetectorStateValue
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AnomalyDetectorStateValue = AnomalyDetectorStateValue'
  { fromAnomalyDetectorStateValue ::
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

pattern AnomalyDetectorStateValuePendingTraining :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValuePendingTraining = AnomalyDetectorStateValue' "PENDING_TRAINING"

pattern AnomalyDetectorStateValueTrainedInsufficientData :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValueTrainedInsufficientData = AnomalyDetectorStateValue' "TRAINED_INSUFFICIENT_DATA"

pattern AnomalyDetectorStateValueTrained :: AnomalyDetectorStateValue
pattern AnomalyDetectorStateValueTrained = AnomalyDetectorStateValue' "TRAINED"

{-# COMPLETE
  AnomalyDetectorStateValuePendingTraining,
  AnomalyDetectorStateValueTrainedInsufficientData,
  AnomalyDetectorStateValueTrained,
  AnomalyDetectorStateValue'
  #-}
