{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.CertificateState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.CertificateState
  ( CertificateState
      ( CertificateState',
        CertificateStateRegistering,
        CertificateStateRegistered,
        CertificateStateRegisterFailed,
        CertificateStateDeregistering,
        CertificateStateDeregistered,
        CertificateStateDeregisterFailed,
        fromCertificateState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CertificateState = CertificateState'
  { fromCertificateState ::
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

pattern CertificateStateRegistering :: CertificateState
pattern CertificateStateRegistering = CertificateState' "Registering"

pattern CertificateStateRegistered :: CertificateState
pattern CertificateStateRegistered = CertificateState' "Registered"

pattern CertificateStateRegisterFailed :: CertificateState
pattern CertificateStateRegisterFailed = CertificateState' "RegisterFailed"

pattern CertificateStateDeregistering :: CertificateState
pattern CertificateStateDeregistering = CertificateState' "Deregistering"

pattern CertificateStateDeregistered :: CertificateState
pattern CertificateStateDeregistered = CertificateState' "Deregistered"

pattern CertificateStateDeregisterFailed :: CertificateState
pattern CertificateStateDeregisterFailed = CertificateState' "DeregisterFailed"

{-# COMPLETE
  CertificateStateRegistering,
  CertificateStateRegistered,
  CertificateStateRegisterFailed,
  CertificateStateDeregistering,
  CertificateStateDeregistered,
  CertificateStateDeregisterFailed,
  CertificateState'
  #-}
