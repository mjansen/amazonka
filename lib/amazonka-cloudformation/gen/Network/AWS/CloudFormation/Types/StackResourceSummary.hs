{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceSummary
  ( StackResourceSummary (..),

    -- * Smart constructor
    mkStackResourceSummary,

    -- * Lenses
    srsLogicalResourceId,
    srsResourceType,
    srsLastUpdatedTimestamp,
    srsResourceStatus,
    srsDriftInformation,
    srsModuleInfo,
    srsPhysicalResourceId,
    srsResourceStatusReason,
  )
where

import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ModuleInfo as Types
import qualified Network.AWS.CloudFormation.Types.PhysicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ResourceStatus as Types
import qualified Network.AWS.CloudFormation.Types.ResourceStatusReason as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'mkStackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
  { -- | The logical name of the resource specified in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
    resourceType :: Types.ResourceType,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Core.UTCTime,
    -- | Current status of the resource.
    resourceStatus :: Types.ResourceStatus,
    -- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Core.Maybe Types.StackResourceDriftInformationSummary,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Core.Maybe Types.ModuleInfo,
    -- | The name or unique identifier that corresponds to a physical instance ID of the resource.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | Success/failure message associated with the resource.
    resourceStatusReason :: Core.Maybe Types.ResourceStatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackResourceSummary' value with any optional fields omitted.
mkStackResourceSummary ::
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'lastUpdatedTimestamp'
  Core.UTCTime ->
  -- | 'resourceStatus'
  Types.ResourceStatus ->
  StackResourceSummary
mkStackResourceSummary
  logicalResourceId
  resourceType
  lastUpdatedTimestamp
  resourceStatus =
    StackResourceSummary'
      { logicalResourceId,
        resourceType,
        lastUpdatedTimestamp,
        resourceStatus,
        driftInformation = Core.Nothing,
        moduleInfo = Core.Nothing,
        physicalResourceId = Core.Nothing,
        resourceStatusReason = Core.Nothing
      }

-- | The logical name of the resource specified in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLogicalResourceId :: Lens.Lens' StackResourceSummary Types.LogicalResourceId
srsLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED srsLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | Type of resource. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceType :: Lens.Lens' StackResourceSummary Types.ResourceType
srsResourceType = Lens.field @"resourceType"
{-# DEPRECATED srsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Time the status was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLastUpdatedTimestamp :: Lens.Lens' StackResourceSummary Core.UTCTime
srsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED srsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | Current status of the resource.
--
-- /Note:/ Consider using 'resourceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceStatus :: Lens.Lens' StackResourceSummary Types.ResourceStatus
srsResourceStatus = Lens.field @"resourceStatus"
{-# DEPRECATED srsResourceStatus "Use generic-lens or generic-optics with 'resourceStatus' instead." #-}

-- | Information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsDriftInformation :: Lens.Lens' StackResourceSummary (Core.Maybe Types.StackResourceDriftInformationSummary)
srsDriftInformation = Lens.field @"driftInformation"
{-# DEPRECATED srsDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsModuleInfo :: Lens.Lens' StackResourceSummary (Core.Maybe Types.ModuleInfo)
srsModuleInfo = Lens.field @"moduleInfo"
{-# DEPRECATED srsModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of the resource.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsPhysicalResourceId :: Lens.Lens' StackResourceSummary (Core.Maybe Types.PhysicalResourceId)
srsPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED srsPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | Success/failure message associated with the resource.
--
-- /Note:/ Consider using 'resourceStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResourceStatusReason :: Lens.Lens' StackResourceSummary (Core.Maybe Types.ResourceStatusReason)
srsResourceStatusReason = Lens.field @"resourceStatusReason"
{-# DEPRECATED srsResourceStatusReason "Use generic-lens or generic-optics with 'resourceStatusReason' instead." #-}

instance Core.FromXML StackResourceSummary where
  parseXML x =
    StackResourceSummary'
      Core.<$> (x Core..@ "LogicalResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "LastUpdatedTimestamp")
      Core.<*> (x Core..@ "ResourceStatus")
      Core.<*> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "ResourceStatusReason")
