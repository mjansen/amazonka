{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ChangeAction
  ( ChangeAction
      ( ChangeAction',
        ChangeActionInsert,
        ChangeActionDelete,
        fromChangeAction
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChangeAction = ChangeAction' {fromChangeAction :: Core.Text}
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

pattern ChangeActionInsert :: ChangeAction
pattern ChangeActionInsert = ChangeAction' "INSERT"

pattern ChangeActionDelete :: ChangeAction
pattern ChangeActionDelete = ChangeAction' "DELETE"

{-# COMPLETE
  ChangeActionInsert,
  ChangeActionDelete,
  ChangeAction'
  #-}
