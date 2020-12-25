{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
  ( ReplicationGroupPendingModifiedValues (..),

    -- * Smart constructor
    mkReplicationGroupPendingModifiedValues,

    -- * Lenses
    rgpmvAuthTokenStatus,
    rgpmvAutomaticFailoverStatus,
    rgpmvPrimaryClusterId,
    rgpmvResharding,
    rgpmvUserGroups,
  )
where

import qualified Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.PrimaryClusterId as Types
import qualified Network.AWS.ElastiCache.Types.ReshardingStatus as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.
--
-- /See:/ 'mkReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { -- | The auth token status
    authTokenStatus :: Core.Maybe Types.AuthTokenUpdateStatus,
    -- | Indicates the status of automatic failover for this Redis replication group.
    automaticFailoverStatus :: Core.Maybe Types.PendingAutomaticFailoverStatus,
    -- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
    primaryClusterId :: Core.Maybe Types.PrimaryClusterId,
    -- | The status of an online resharding operation.
    resharding :: Core.Maybe Types.ReshardingStatus,
    -- | The user groups being modified.
    userGroups :: Core.Maybe Types.UserGroupsUpdateStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationGroupPendingModifiedValues' value with any optional fields omitted.
mkReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
mkReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { authTokenStatus =
        Core.Nothing,
      automaticFailoverStatus = Core.Nothing,
      primaryClusterId = Core.Nothing,
      resharding = Core.Nothing,
      userGroups = Core.Nothing
    }

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAuthTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.AuthTokenUpdateStatus)
rgpmvAuthTokenStatus = Lens.field @"authTokenStatus"
{-# DEPRECATED rgpmvAuthTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead." #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailoverStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAutomaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = Lens.field @"automaticFailoverStatus"
{-# DEPRECATED rgpmvAutomaticFailoverStatus "Use generic-lens or generic-optics with 'automaticFailoverStatus' instead." #-}

-- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvPrimaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.PrimaryClusterId)
rgpmvPrimaryClusterId = Lens.field @"primaryClusterId"
{-# DEPRECATED rgpmvPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

-- | The status of an online resharding operation.
--
-- /Note:/ Consider using 'resharding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvResharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.ReshardingStatus)
rgpmvResharding = Lens.field @"resharding"
{-# DEPRECATED rgpmvResharding "Use generic-lens or generic-optics with 'resharding' instead." #-}

-- | The user groups being modified.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvUserGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.UserGroupsUpdateStatus)
rgpmvUserGroups = Lens.field @"userGroups"
{-# DEPRECATED rgpmvUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

instance Core.FromXML ReplicationGroupPendingModifiedValues where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Core.<$> (x Core..@? "AuthTokenStatus")
      Core.<*> (x Core..@? "AutomaticFailoverStatus")
      Core.<*> (x Core..@? "PrimaryClusterId")
      Core.<*> (x Core..@? "Resharding")
      Core.<*> (x Core..@? "UserGroups")
