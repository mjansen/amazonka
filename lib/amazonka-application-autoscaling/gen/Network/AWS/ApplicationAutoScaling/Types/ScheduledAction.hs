{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
  ( ScheduledAction (..),

    -- * Smart constructor
    mkScheduledAction,

    -- * Lenses
    saScheduledActionName,
    saScheduledActionARN,
    saServiceNamespace,
    saSchedule,
    saResourceId,
    saCreationTime,
    saEndTime,
    saScalableDimension,
    saScalableTargetAction,
    saStartTime,
  )
where

import qualified Network.AWS.ApplicationAutoScaling.Types.ResourceIdMaxLen1600 as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.ScalableDimension as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.ScheduledActionName as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a scheduled action.
--
-- /See:/ 'mkScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | The name of the scheduled action.
    scheduledActionName :: Types.ScheduledActionName,
    -- | The Amazon Resource Name (ARN) of the scheduled action.
    scheduledActionARN :: Types.ResourceIdMaxLen1600,
    -- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
    serviceNamespace :: Types.ServiceNamespace,
    -- | The schedule for this action. The following formats are supported:
    --
    --
    --     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "
    --
    --
    --     * Rate expressions - "@rate(/value/ /unit/ )@ "
    --
    --
    --     * Cron expressions - "@cron(/fields/ )@ "
    --
    --
    -- At expressions are useful for one-time schedules. Specify the time in UTC.
    -- For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ .
    -- For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
    -- For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
    schedule :: Types.ResourceIdMaxLen1600,
    -- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
    --
    --
    --     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
    --
    --
    --     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
    --
    --
    --     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
    --
    --
    --     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
    --
    --
    --     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
    --
    --
    --     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
    --
    --
    --     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
    --
    --
    --     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
    --
    --
    --     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
    --
    --
    --     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
    --
    --
    --     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
    --
    --
    --     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
    --
    --
    --     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
    --
    --
    --     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
    resourceId :: Types.ResourceIdMaxLen1600,
    -- | The date and time that the scheduled action was created.
    creationTime :: Core.NominalDiffTime,
    -- | The date and time that the action is scheduled to end.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
    --
    --
    --     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
    --
    --
    --     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
    --
    --
    --     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
    --
    --
    --     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
    --
    --
    --     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
    --
    --
    --     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
    --
    --
    --     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
    --
    --
    --     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
    --
    --
    --     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
    --
    --
    --     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
    --
    --
    --     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
    --
    --
    --     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
    --
    --
    --     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
    --
    --
    --     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
    --
    --
    --     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
    --
    --
    --     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
    --
    --
    --     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
    scalableDimension :: Core.Maybe Types.ScalableDimension,
    -- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
    scalableTargetAction :: Core.Maybe Types.ScalableTargetAction,
    -- | The date and time that the action is scheduled to begin.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ScheduledAction' value with any optional fields omitted.
mkScheduledAction ::
  -- | 'scheduledActionName'
  Types.ScheduledActionName ->
  -- | 'scheduledActionARN'
  Types.ResourceIdMaxLen1600 ->
  -- | 'serviceNamespace'
  Types.ServiceNamespace ->
  -- | 'schedule'
  Types.ResourceIdMaxLen1600 ->
  -- | 'resourceId'
  Types.ResourceIdMaxLen1600 ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  ScheduledAction
mkScheduledAction
  scheduledActionName
  scheduledActionARN
  serviceNamespace
  schedule
  resourceId
  creationTime =
    ScheduledAction'
      { scheduledActionName,
        scheduledActionARN,
        serviceNamespace,
        schedule,
        resourceId,
        creationTime,
        endTime = Core.Nothing,
        scalableDimension = Core.Nothing,
        scalableTargetAction = Core.Nothing,
        startTime = Core.Nothing
      }

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionName :: Lens.Lens' ScheduledAction Types.ScheduledActionName
saScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED saScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionARN :: Lens.Lens' ScheduledAction Types.ResourceIdMaxLen1600
saScheduledActionARN = Lens.field @"scheduledActionARN"
{-# DEPRECATED saScheduledActionARN "Use generic-lens or generic-optics with 'scheduledActionARN' instead." #-}

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saServiceNamespace :: Lens.Lens' ScheduledAction Types.ServiceNamespace
saServiceNamespace = Lens.field @"serviceNamespace"
{-# DEPRECATED saServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The schedule for this action. The following formats are supported:
--
--
--     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "
--
--
--     * Rate expressions - "@rate(/value/ /unit/ )@ "
--
--
--     * Cron expressions - "@cron(/fields/ )@ "
--
--
-- At expressions are useful for one-time schedules. Specify the time in UTC.
-- For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ .
-- For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
-- For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSchedule :: Lens.Lens' ScheduledAction Types.ResourceIdMaxLen1600
saSchedule = Lens.field @"schedule"
{-# DEPRECATED saSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
--
--     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
--
--     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
--
--     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
--
--
--     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
--
--
--     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
--
--
--     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
--
--     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saResourceId :: Lens.Lens' ScheduledAction Types.ResourceIdMaxLen1600
saResourceId = Lens.field @"resourceId"
{-# DEPRECATED saResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The date and time that the scheduled action was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saCreationTime :: Lens.Lens' ScheduledAction Core.NominalDiffTime
saCreationTime = Lens.field @"creationTime"
{-# DEPRECATED saCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the action is scheduled to end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saEndTime :: Lens.Lens' ScheduledAction (Core.Maybe Core.NominalDiffTime)
saEndTime = Lens.field @"endTime"
{-# DEPRECATED saEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
--
--     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
--
--     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
--
--
--     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
--
--
--     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
--
--
--     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
--
--
--     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
--
--
--     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
--
--     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
--
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScalableDimension :: Lens.Lens' ScheduledAction (Core.Maybe Types.ScalableDimension)
saScalableDimension = Lens.field @"scalableDimension"
{-# DEPRECATED saScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- /Note:/ Consider using 'scalableTargetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScalableTargetAction :: Lens.Lens' ScheduledAction (Core.Maybe Types.ScalableTargetAction)
saScalableTargetAction = Lens.field @"scalableTargetAction"
{-# DEPRECATED saScalableTargetAction "Use generic-lens or generic-optics with 'scalableTargetAction' instead." #-}

-- | The date and time that the action is scheduled to begin.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saStartTime :: Lens.Lens' ScheduledAction (Core.Maybe Core.NominalDiffTime)
saStartTime = Lens.field @"startTime"
{-# DEPRECATED saStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON ScheduledAction where
  parseJSON =
    Core.withObject "ScheduledAction" Core.$
      \x ->
        ScheduledAction'
          Core.<$> (x Core..: "ScheduledActionName")
          Core.<*> (x Core..: "ScheduledActionARN")
          Core.<*> (x Core..: "ServiceNamespace")
          Core.<*> (x Core..: "Schedule")
          Core.<*> (x Core..: "ResourceId")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "ScalableDimension")
          Core.<*> (x Core..:? "ScalableTargetAction")
          Core.<*> (x Core..:? "StartTime")
