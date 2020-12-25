{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
  ( Phase2IntegrityAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase2IntegrityAlgorithmsRequestListValue,

    -- * Lenses
    piarlvValue,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the integrity algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsRequestListValue = Phase2IntegrityAlgorithmsRequestListValue'
  { -- | The integrity algorithm.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2IntegrityAlgorithmsRequestListValue' value with any optional fields omitted.
mkPhase2IntegrityAlgorithmsRequestListValue ::
  Phase2IntegrityAlgorithmsRequestListValue
mkPhase2IntegrityAlgorithmsRequestListValue =
  Phase2IntegrityAlgorithmsRequestListValue' {value = Core.Nothing}

-- | The integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piarlvValue :: Lens.Lens' Phase2IntegrityAlgorithmsRequestListValue (Core.Maybe Types.String)
piarlvValue = Lens.field @"value"
{-# DEPRECATED piarlvValue "Use generic-lens or generic-optics with 'value' instead." #-}
