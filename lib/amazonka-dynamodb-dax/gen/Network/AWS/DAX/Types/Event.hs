{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eDate,
    eMessage,
    eSourceName,
    eSourceType,
  )
where

import qualified Network.AWS.DAX.Types.Message as Types
import qualified Network.AWS.DAX.Types.SourceName as Types
import qualified Network.AWS.DAX.Types.SourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a DAX cluster, adding or removing a node, or rebooting a node.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The date and time when the event occurred.
    date :: Core.Maybe Core.NominalDiffTime,
    -- | A user-defined message associated with the event.
    message :: Core.Maybe Types.Message,
    -- | The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
    sourceName :: Core.Maybe Types.SourceName,
    -- | Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  Event
mkEvent =
  Event'
    { date = Core.Nothing,
      message = Core.Nothing,
      sourceName = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eDate = Lens.field @"date"
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | A user-defined message associated with the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
--
-- /Note:/ Consider using 'sourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceName :: Lens.Lens' Event (Core.Maybe Types.SourceName)
eSourceName = Lens.field @"sourceName"
{-# DEPRECATED eSourceName "Use generic-lens or generic-optics with 'sourceName' instead." #-}

-- | Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject "Event" Core.$
      \x ->
        Event'
          Core.<$> (x Core..:? "Date")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "SourceName")
          Core.<*> (x Core..:? "SourceType")
