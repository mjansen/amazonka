{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Alarm
  ( Alarm (..),

    -- * Smart constructor
    mkAlarm,

    -- * Lenses
    aArn,
    aComparisonOperator,
    aContactProtocols,
    aCreatedAt,
    aDatapointsToAlarm,
    aEvaluationPeriods,
    aLocation,
    aMetricName,
    aMonitoredResourceInfo,
    aName,
    aNotificationEnabled,
    aNotificationTriggers,
    aPeriod,
    aResourceType,
    aState,
    aStatistic,
    aSupportCode,
    aThreshold,
    aTreatMissingData,
    aUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AlarmState as Types
import qualified Network.AWS.Lightsail.Types.ComparisonOperator as Types
import qualified Network.AWS.Lightsail.Types.ContactProtocol as Types
import qualified Network.AWS.Lightsail.Types.MetricName as Types
import qualified Network.AWS.Lightsail.Types.MetricStatistic as Types
import qualified Network.AWS.Lightsail.Types.MetricUnit as Types
import qualified Network.AWS.Lightsail.Types.MonitoredResourceInfo as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.TreatMissingData as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
-- /See:/ 'mkAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The Amazon Resource Name (ARN) of the alarm.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The arithmetic operation used when comparing the specified statistic and threshold.
    comparisonOperator :: Core.Maybe Types.ComparisonOperator,
    -- | The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
    contactProtocols :: Core.Maybe [Types.ContactProtocol],
    -- | The timestamp when the alarm was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The number of data points that must not within the specified threshold to trigger the alarm.
    datapointsToAlarm :: Core.Maybe Core.Int,
    -- | The number of periods over which data is compared to the specified threshold.
    evaluationPeriods :: Core.Maybe Core.Int,
    -- | An object that lists information about the location of the alarm.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The name of the metric associated with the alarm.
    metricName :: Core.Maybe Types.MetricName,
    -- | An object that lists information about the resource monitored by the alarm.
    monitoredResourceInfo :: Core.Maybe Types.MonitoredResourceInfo,
    -- | The name of the alarm.
    name :: Core.Maybe Types.ResourceName,
    -- | Indicates whether the alarm is enabled.
    notificationEnabled :: Core.Maybe Core.Bool,
    -- | The alarm states that trigger a notification.
    notificationTriggers :: Core.Maybe [Types.AlarmState],
    -- | The period, in seconds, over which the statistic is applied.
    period :: Core.Maybe Core.Natural,
    -- | The Lightsail resource type (e.g., @Alarm@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The current state of the alarm.
    --
    -- An alarm has the following possible states:
    --
    --     * @ALARM@ - The metric is outside of the defined threshold.
    --
    --
    --     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
    --
    --
    --     * @OK@ - The metric is within the defined threshold.
    state :: Core.Maybe Types.AlarmState,
    -- | The statistic for the metric associated with the alarm.
    --
    -- The following statistics are available:
    --
    --     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
    --
    --
    --     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
    --
    --
    --     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
    --
    --
    --     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.
    --
    --
    --     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
    statistic :: Core.Maybe Types.MetricStatistic,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String,
    -- | The value against which the specified statistic is compared.
    threshold :: Core.Maybe Core.Double,
    -- | Specifies how the alarm handles missing data points.
    --
    -- An alarm can treat missing data in the following ways:
    --
    --     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
    --
    --
    --     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
    --
    --
    --     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
    --
    --
    --     * @missing@ - Missing data is treated as missing.
    treatMissingData :: Core.Maybe Types.TreatMissingData,
    -- | The unit of the metric associated with the alarm.
    unit :: Core.Maybe Types.MetricUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Alarm' value with any optional fields omitted.
mkAlarm ::
  Alarm
mkAlarm =
  Alarm'
    { arn = Core.Nothing,
      comparisonOperator = Core.Nothing,
      contactProtocols = Core.Nothing,
      createdAt = Core.Nothing,
      datapointsToAlarm = Core.Nothing,
      evaluationPeriods = Core.Nothing,
      location = Core.Nothing,
      metricName = Core.Nothing,
      monitoredResourceInfo = Core.Nothing,
      name = Core.Nothing,
      notificationEnabled = Core.Nothing,
      notificationTriggers = Core.Nothing,
      period = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      statistic = Core.Nothing,
      supportCode = Core.Nothing,
      threshold = Core.Nothing,
      treatMissingData = Core.Nothing,
      unit = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' Alarm (Core.Maybe Types.NonEmptyString)
aArn = Lens.field @"arn"
{-# DEPRECATED aArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The arithmetic operation used when comparing the specified statistic and threshold.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aComparisonOperator :: Lens.Lens' Alarm (Core.Maybe Types.ComparisonOperator)
aComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED aComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- /Note:/ Consider using 'contactProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aContactProtocols :: Lens.Lens' Alarm (Core.Maybe [Types.ContactProtocol])
aContactProtocols = Lens.field @"contactProtocols"
{-# DEPRECATED aContactProtocols "Use generic-lens or generic-optics with 'contactProtocols' instead." #-}

-- | The timestamp when the alarm was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' Alarm (Core.Maybe Core.NominalDiffTime)
aCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED aCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The number of data points that must not within the specified threshold to trigger the alarm.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDatapointsToAlarm :: Lens.Lens' Alarm (Core.Maybe Core.Int)
aDatapointsToAlarm = Lens.field @"datapointsToAlarm"
{-# DEPRECATED aDatapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead." #-}

-- | The number of periods over which data is compared to the specified threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEvaluationPeriods :: Lens.Lens' Alarm (Core.Maybe Core.Int)
aEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# DEPRECATED aEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | An object that lists information about the location of the alarm.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocation :: Lens.Lens' Alarm (Core.Maybe Types.ResourceLocation)
aLocation = Lens.field @"location"
{-# DEPRECATED aLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the metric associated with the alarm.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetricName :: Lens.Lens' Alarm (Core.Maybe Types.MetricName)
aMetricName = Lens.field @"metricName"
{-# DEPRECATED aMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | An object that lists information about the resource monitored by the alarm.
--
-- /Note:/ Consider using 'monitoredResourceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMonitoredResourceInfo :: Lens.Lens' Alarm (Core.Maybe Types.MonitoredResourceInfo)
aMonitoredResourceInfo = Lens.field @"monitoredResourceInfo"
{-# DEPRECATED aMonitoredResourceInfo "Use generic-lens or generic-optics with 'monitoredResourceInfo' instead." #-}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alarm (Core.Maybe Types.ResourceName)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates whether the alarm is enabled.
--
-- /Note:/ Consider using 'notificationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationEnabled :: Lens.Lens' Alarm (Core.Maybe Core.Bool)
aNotificationEnabled = Lens.field @"notificationEnabled"
{-# DEPRECATED aNotificationEnabled "Use generic-lens or generic-optics with 'notificationEnabled' instead." #-}

-- | The alarm states that trigger a notification.
--
-- /Note:/ Consider using 'notificationTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationTriggers :: Lens.Lens' Alarm (Core.Maybe [Types.AlarmState])
aNotificationTriggers = Lens.field @"notificationTriggers"
{-# DEPRECATED aNotificationTriggers "Use generic-lens or generic-optics with 'notificationTriggers' instead." #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPeriod :: Lens.Lens' Alarm (Core.Maybe Core.Natural)
aPeriod = Lens.field @"period"
{-# DEPRECATED aPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The Lightsail resource type (e.g., @Alarm@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceType :: Lens.Lens' Alarm (Core.Maybe Types.ResourceType)
aResourceType = Lens.field @"resourceType"
{-# DEPRECATED aResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The current state of the alarm.
--
-- An alarm has the following possible states:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' Alarm (Core.Maybe Types.AlarmState)
aState = Lens.field @"state"
{-# DEPRECATED aState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The statistic for the metric associated with the alarm.
--
-- The following statistics are available:
--
--     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
--
--
--     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
--
--
--     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
--
--
--     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.
--
--
--     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
--
--
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatistic :: Lens.Lens' Alarm (Core.Maybe Types.MetricStatistic)
aStatistic = Lens.field @"statistic"
{-# DEPRECATED aStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSupportCode :: Lens.Lens' Alarm (Core.Maybe Types.String)
aSupportCode = Lens.field @"supportCode"
{-# DEPRECATED aSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aThreshold :: Lens.Lens' Alarm (Core.Maybe Core.Double)
aThreshold = Lens.field @"threshold"
{-# DEPRECATED aThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | Specifies how the alarm handles missing data points.
--
-- An alarm can treat missing data in the following ways:
--
--     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
--
--
--     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
--
--
--     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
--
--
--     * @missing@ - Missing data is treated as missing.
--
--
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTreatMissingData :: Lens.Lens' Alarm (Core.Maybe Types.TreatMissingData)
aTreatMissingData = Lens.field @"treatMissingData"
{-# DEPRECATED aTreatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead." #-}

-- | The unit of the metric associated with the alarm.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUnit :: Lens.Lens' Alarm (Core.Maybe Types.MetricUnit)
aUnit = Lens.field @"unit"
{-# DEPRECATED aUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject "Alarm" Core.$
      \x ->
        Alarm'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "comparisonOperator")
          Core.<*> (x Core..:? "contactProtocols")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "datapointsToAlarm")
          Core.<*> (x Core..:? "evaluationPeriods")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "metricName")
          Core.<*> (x Core..:? "monitoredResourceInfo")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "notificationEnabled")
          Core.<*> (x Core..:? "notificationTriggers")
          Core.<*> (x Core..:? "period")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "statistic")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "threshold")
          Core.<*> (x Core..:? "treatMissingData")
          Core.<*> (x Core..:? "unit")
