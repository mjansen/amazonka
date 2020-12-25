{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Reservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Reservation
  ( Reservation (..),

    -- * Smart constructor
    mkReservation,

    -- * Lenses
    rGroups,
    rInstances,
    rOwnerId,
    rRequesterId,
    rReservationId,
  )
where

import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.Instance as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch request for one or more instances, and includes owner, requester, and security group information that applies to all instances in the launch request.
--
-- /See:/ 'mkReservation' smart constructor.
data Reservation = Reservation'
  { -- | [EC2-Classic only] The security groups.
    groups :: Core.Maybe [Types.GroupIdentifier],
    -- | The instances.
    instances :: Core.Maybe [Types.Instance],
    -- | The ID of the AWS account that owns the reservation.
    ownerId :: Types.String,
    -- | The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
    requesterId :: Core.Maybe Types.String,
    -- | The ID of the reservation.
    reservationId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Reservation' value with any optional fields omitted.
mkReservation ::
  -- | 'ownerId'
  Types.String ->
  -- | 'reservationId'
  Types.String ->
  Reservation
mkReservation ownerId reservationId =
  Reservation'
    { groups = Core.Nothing,
      instances = Core.Nothing,
      ownerId,
      requesterId = Core.Nothing,
      reservationId
    }

-- | [EC2-Classic only] The security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGroups :: Lens.Lens' Reservation (Core.Maybe [Types.GroupIdentifier])
rGroups = Lens.field @"groups"
{-# DEPRECATED rGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstances :: Lens.Lens' Reservation (Core.Maybe [Types.Instance])
rInstances = Lens.field @"instances"
{-# DEPRECATED rInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The ID of the AWS account that owns the reservation.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOwnerId :: Lens.Lens' Reservation Types.String
rOwnerId = Lens.field @"ownerId"
{-# DEPRECATED rOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- /Note:/ Consider using 'requesterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRequesterId :: Lens.Lens' Reservation (Core.Maybe Types.String)
rRequesterId = Lens.field @"requesterId"
{-# DEPRECATED rRequesterId "Use generic-lens or generic-optics with 'requesterId' instead." #-}

-- | The ID of the reservation.
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReservationId :: Lens.Lens' Reservation Types.String
rReservationId = Lens.field @"reservationId"
{-# DEPRECATED rReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

instance Core.FromXML Reservation where
  parseXML x =
    Reservation'
      Core.<$> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "ownerId")
      Core.<*> (x Core..@? "requesterId")
      Core.<*> (x Core..@ "reservationId")
