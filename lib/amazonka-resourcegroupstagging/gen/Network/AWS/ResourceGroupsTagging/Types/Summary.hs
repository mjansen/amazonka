{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.Summary
  ( Summary (..),

    -- * Smart constructor
    mkSummary,

    -- * Lenses
    sLastUpdated,
    sNonCompliantResources,
    sRegion,
    sResourceType,
    sTargetId,
    sTargetIdType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroupsTagging.Types.LastUpdated as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.Region as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.ResourceType as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.TargetId as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.TargetIdType as Types

-- | A count of noncompliant resources.
--
-- /See:/ 'mkSummary' smart constructor.
data Summary = Summary'
  { -- | The timestamp that shows when this summary was generated in this Region.
    lastUpdated :: Core.Maybe Types.LastUpdated,
    -- | The count of noncompliant resources.
    nonCompliantResources :: Core.Maybe Core.Integer,
    -- | The AWS Region that the summary applies to.
    region :: Core.Maybe Types.Region,
    -- | The AWS resource type.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
    targetId :: Core.Maybe Types.TargetId,
    -- | Whether the target is an account, an OU, or the organization root.
    targetIdType :: Core.Maybe Types.TargetIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Summary' value with any optional fields omitted.
mkSummary ::
  Summary
mkSummary =
  Summary'
    { lastUpdated = Core.Nothing,
      nonCompliantResources = Core.Nothing,
      region = Core.Nothing,
      resourceType = Core.Nothing,
      targetId = Core.Nothing,
      targetIdType = Core.Nothing
    }

-- | The timestamp that shows when this summary was generated in this Region.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdated :: Lens.Lens' Summary (Core.Maybe Types.LastUpdated)
sLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED sLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The count of noncompliant resources.
--
-- /Note:/ Consider using 'nonCompliantResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNonCompliantResources :: Lens.Lens' Summary (Core.Maybe Core.Integer)
sNonCompliantResources = Lens.field @"nonCompliantResources"
{-# DEPRECATED sNonCompliantResources "Use generic-lens or generic-optics with 'nonCompliantResources' instead." #-}

-- | The AWS Region that the summary applies to.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRegion :: Lens.Lens' Summary (Core.Maybe Types.Region)
sRegion = Lens.field @"region"
{-# DEPRECATED sRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The AWS resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceType :: Lens.Lens' Summary (Core.Maybe Types.ResourceType)
sResourceType = Lens.field @"resourceType"
{-# DEPRECATED sResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetId :: Lens.Lens' Summary (Core.Maybe Types.TargetId)
sTargetId = Lens.field @"targetId"
{-# DEPRECATED sTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | Whether the target is an account, an OU, or the organization root.
--
-- /Note:/ Consider using 'targetIdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetIdType :: Lens.Lens' Summary (Core.Maybe Types.TargetIdType)
sTargetIdType = Lens.field @"targetIdType"
{-# DEPRECATED sTargetIdType "Use generic-lens or generic-optics with 'targetIdType' instead." #-}

instance Core.FromJSON Summary where
  parseJSON =
    Core.withObject "Summary" Core.$
      \x ->
        Summary'
          Core.<$> (x Core..:? "LastUpdated")
          Core.<*> (x Core..:? "NonCompliantResources")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "ResourceType")
          Core.<*> (x Core..:? "TargetId")
          Core.<*> (x Core..:? "TargetIdType")
