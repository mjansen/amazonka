{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProvisionedBandwidth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProvisionedBandwidth
  ( ProvisionedBandwidth (..),

    -- * Smart constructor
    mkProvisionedBandwidth,

    -- * Lenses
    pbProvisionTime,
    pbProvisioned,
    pbRequestTime,
    pbRequested,
    pbStatus,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /See:/ 'mkProvisionedBandwidth' smart constructor.
data ProvisionedBandwidth = ProvisionedBandwidth'
  { -- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
    provisionTime :: Core.Maybe Core.UTCTime,
    -- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
    provisioned :: Core.Maybe Types.String,
    -- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
    requestTime :: Core.Maybe Core.UTCTime,
    -- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
    requested :: Core.Maybe Types.String,
    -- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisionedBandwidth' value with any optional fields omitted.
mkProvisionedBandwidth ::
  ProvisionedBandwidth
mkProvisionedBandwidth =
  ProvisionedBandwidth'
    { provisionTime = Core.Nothing,
      provisioned = Core.Nothing,
      requestTime = Core.Nothing,
      requested = Core.Nothing,
      status = Core.Nothing
    }

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProvisionTime :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.UTCTime)
pbProvisionTime = Lens.field @"provisionTime"
{-# DEPRECATED pbProvisionTime "Use generic-lens or generic-optics with 'provisionTime' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisioned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProvisioned :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Types.String)
pbProvisioned = Lens.field @"provisioned"
{-# DEPRECATED pbProvisioned "Use generic-lens or generic-optics with 'provisioned' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'requestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbRequestTime :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Core.UTCTime)
pbRequestTime = Lens.field @"requestTime"
{-# DEPRECATED pbRequestTime "Use generic-lens or generic-optics with 'requestTime' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'requested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbRequested :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Types.String)
pbRequested = Lens.field @"requested"
{-# DEPRECATED pbRequested "Use generic-lens or generic-optics with 'requested' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbStatus :: Lens.Lens' ProvisionedBandwidth (Core.Maybe Types.String)
pbStatus = Lens.field @"status"
{-# DEPRECATED pbStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML ProvisionedBandwidth where
  parseXML x =
    ProvisionedBandwidth'
      Core.<$> (x Core..@? "provisionTime")
      Core.<*> (x Core..@? "provisioned")
      Core.<*> (x Core..@? "requestTime")
      Core.<*> (x Core..@? "requested")
      Core.<*> (x Core..@? "status")
