{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupState
  ( BackupState
      ( BackupState',
        BackupStateCreateInProgress,
        BackupStateReady,
        BackupStateDeleted,
        BackupStatePendingDeletion,
        fromBackupState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BackupState = BackupState' {fromBackupState :: Core.Text}
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

pattern BackupStateCreateInProgress :: BackupState
pattern BackupStateCreateInProgress = BackupState' "CREATE_IN_PROGRESS"

pattern BackupStateReady :: BackupState
pattern BackupStateReady = BackupState' "READY"

pattern BackupStateDeleted :: BackupState
pattern BackupStateDeleted = BackupState' "DELETED"

pattern BackupStatePendingDeletion :: BackupState
pattern BackupStatePendingDeletion = BackupState' "PENDING_DELETION"

{-# COMPLETE
  BackupStateCreateInProgress,
  BackupStateReady,
  BackupStateDeleted,
  BackupStatePendingDeletion,
  BackupState'
  #-}
