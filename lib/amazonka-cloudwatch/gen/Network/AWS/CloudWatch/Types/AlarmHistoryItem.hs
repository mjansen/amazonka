{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AlarmHistoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AlarmHistoryItem
  ( AlarmHistoryItem (..),

    -- * Smart constructor
    mkAlarmHistoryItem,

    -- * Lenses
    ahiAlarmName,
    ahiAlarmType,
    ahiHistoryData,
    ahiHistoryItemType,
    ahiHistorySummary,
    ahiTimestamp,
  )
where

import qualified Network.AWS.CloudWatch.Types.AlarmName as Types
import qualified Network.AWS.CloudWatch.Types.AlarmType as Types
import qualified Network.AWS.CloudWatch.Types.HistoryData as Types
import qualified Network.AWS.CloudWatch.Types.HistoryItemType as Types
import qualified Network.AWS.CloudWatch.Types.HistorySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the history of a specific alarm.
--
-- /See:/ 'mkAlarmHistoryItem' smart constructor.
data AlarmHistoryItem = AlarmHistoryItem'
  { -- | The descriptive name for the alarm.
    alarmName :: Core.Maybe Types.AlarmName,
    -- | The type of alarm, either metric alarm or composite alarm.
    alarmType :: Core.Maybe Types.AlarmType,
    -- | Data about the alarm, in JSON format.
    historyData :: Core.Maybe Types.HistoryData,
    -- | The type of alarm history item.
    historyItemType :: Core.Maybe Types.HistoryItemType,
    -- | A summary of the alarm history, in text format.
    historySummary :: Core.Maybe Types.HistorySummary,
    -- | The time stamp for the alarm history item.
    timestamp :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AlarmHistoryItem' value with any optional fields omitted.
mkAlarmHistoryItem ::
  AlarmHistoryItem
mkAlarmHistoryItem =
  AlarmHistoryItem'
    { alarmName = Core.Nothing,
      alarmType = Core.Nothing,
      historyData = Core.Nothing,
      historyItemType = Core.Nothing,
      historySummary = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The descriptive name for the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiAlarmName :: Lens.Lens' AlarmHistoryItem (Core.Maybe Types.AlarmName)
ahiAlarmName = Lens.field @"alarmName"
{-# DEPRECATED ahiAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The type of alarm, either metric alarm or composite alarm.
--
-- /Note:/ Consider using 'alarmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiAlarmType :: Lens.Lens' AlarmHistoryItem (Core.Maybe Types.AlarmType)
ahiAlarmType = Lens.field @"alarmType"
{-# DEPRECATED ahiAlarmType "Use generic-lens or generic-optics with 'alarmType' instead." #-}

-- | Data about the alarm, in JSON format.
--
-- /Note:/ Consider using 'historyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistoryData :: Lens.Lens' AlarmHistoryItem (Core.Maybe Types.HistoryData)
ahiHistoryData = Lens.field @"historyData"
{-# DEPRECATED ahiHistoryData "Use generic-lens or generic-optics with 'historyData' instead." #-}

-- | The type of alarm history item.
--
-- /Note:/ Consider using 'historyItemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistoryItemType :: Lens.Lens' AlarmHistoryItem (Core.Maybe Types.HistoryItemType)
ahiHistoryItemType = Lens.field @"historyItemType"
{-# DEPRECATED ahiHistoryItemType "Use generic-lens or generic-optics with 'historyItemType' instead." #-}

-- | A summary of the alarm history, in text format.
--
-- /Note:/ Consider using 'historySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistorySummary :: Lens.Lens' AlarmHistoryItem (Core.Maybe Types.HistorySummary)
ahiHistorySummary = Lens.field @"historySummary"
{-# DEPRECATED ahiHistorySummary "Use generic-lens or generic-optics with 'historySummary' instead." #-}

-- | The time stamp for the alarm history item.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiTimestamp :: Lens.Lens' AlarmHistoryItem (Core.Maybe Core.UTCTime)
ahiTimestamp = Lens.field @"timestamp"
{-# DEPRECATED ahiTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromXML AlarmHistoryItem where
  parseXML x =
    AlarmHistoryItem'
      Core.<$> (x Core..@? "AlarmName")
      Core.<*> (x Core..@? "AlarmType")
      Core.<*> (x Core..@? "HistoryData")
      Core.<*> (x Core..@? "HistoryItemType")
      Core.<*> (x Core..@? "HistorySummary")
      Core.<*> (x Core..@? "Timestamp")
