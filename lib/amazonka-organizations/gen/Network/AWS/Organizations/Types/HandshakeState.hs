{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeState
  ( HandshakeState
      ( HandshakeState',
        HandshakeStateRequested,
        HandshakeStateOpen,
        HandshakeStateCanceled,
        HandshakeStateAccepted,
        HandshakeStateDeclined,
        HandshakeStateExpired,
        fromHandshakeState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HandshakeState = HandshakeState'
  { fromHandshakeState ::
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

pattern HandshakeStateRequested :: HandshakeState
pattern HandshakeStateRequested = HandshakeState' "REQUESTED"

pattern HandshakeStateOpen :: HandshakeState
pattern HandshakeStateOpen = HandshakeState' "OPEN"

pattern HandshakeStateCanceled :: HandshakeState
pattern HandshakeStateCanceled = HandshakeState' "CANCELED"

pattern HandshakeStateAccepted :: HandshakeState
pattern HandshakeStateAccepted = HandshakeState' "ACCEPTED"

pattern HandshakeStateDeclined :: HandshakeState
pattern HandshakeStateDeclined = HandshakeState' "DECLINED"

pattern HandshakeStateExpired :: HandshakeState
pattern HandshakeStateExpired = HandshakeState' "EXPIRED"

{-# COMPLETE
  HandshakeStateRequested,
  HandshakeStateOpen,
  HandshakeStateCanceled,
  HandshakeStateAccepted,
  HandshakeStateDeclined,
  HandshakeStateExpired,
  HandshakeState'
  #-}
