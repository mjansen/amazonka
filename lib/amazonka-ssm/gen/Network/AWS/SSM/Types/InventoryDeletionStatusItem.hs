{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionStatusItem
  ( InventoryDeletionStatusItem (..),

    -- * Smart constructor
    mkInventoryDeletionStatusItem,

    -- * Lenses
    idsiDeletionId,
    idsiDeletionStartTime,
    idsiDeletionSummary,
    idsiLastStatus,
    idsiLastStatusMessage,
    idsiLastStatusUpdateTime,
    idsiTypeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DeletionId as Types
import qualified Network.AWS.SSM.Types.InventoryDeletionStatus as Types
import qualified Network.AWS.SSM.Types.InventoryDeletionSummary as Types
import qualified Network.AWS.SSM.Types.LastStatusMessage as Types
import qualified Network.AWS.SSM.Types.TypeName as Types

-- | Status information returned by the @DeleteInventory@ action.
--
-- /See:/ 'mkInventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { -- | The deletion ID returned by the @DeleteInventory@ action.
    deletionId :: Core.Maybe Types.DeletionId,
    -- | The UTC timestamp when the delete operation started.
    deletionStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
    deletionSummary :: Core.Maybe Types.InventoryDeletionSummary,
    -- | The status of the operation. Possible values are InProgress and Complete.
    lastStatus :: Core.Maybe Types.InventoryDeletionStatus,
    -- | Information about the status.
    lastStatusMessage :: Core.Maybe Types.LastStatusMessage,
    -- | The UTC timestamp of when the last status report.
    lastStatusUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the inventory data type.
    typeName :: Core.Maybe Types.TypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InventoryDeletionStatusItem' value with any optional fields omitted.
mkInventoryDeletionStatusItem ::
  InventoryDeletionStatusItem
mkInventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { deletionId = Core.Nothing,
      deletionStartTime = Core.Nothing,
      deletionSummary = Core.Nothing,
      lastStatus = Core.Nothing,
      lastStatusMessage = Core.Nothing,
      lastStatusUpdateTime = Core.Nothing,
      typeName = Core.Nothing
    }

-- | The deletion ID returned by the @DeleteInventory@ action.
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionId :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Types.DeletionId)
idsiDeletionId = Lens.field @"deletionId"
{-# DEPRECATED idsiDeletionId "Use generic-lens or generic-optics with 'deletionId' instead." #-}

-- | The UTC timestamp when the delete operation started.
--
-- /Note:/ Consider using 'deletionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionStartTime :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.NominalDiffTime)
idsiDeletionStartTime = Lens.field @"deletionStartTime"
{-# DEPRECATED idsiDeletionStartTime "Use generic-lens or generic-optics with 'deletionStartTime' instead." #-}

-- | Information about the delete operation. For more information about this summary, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'deletionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiDeletionSummary :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Types.InventoryDeletionSummary)
idsiDeletionSummary = Lens.field @"deletionSummary"
{-# DEPRECATED idsiDeletionSummary "Use generic-lens or generic-optics with 'deletionSummary' instead." #-}

-- | The status of the operation. Possible values are InProgress and Complete.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatus :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Types.InventoryDeletionStatus)
idsiLastStatus = Lens.field @"lastStatus"
{-# DEPRECATED idsiLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | Information about the status.
--
-- /Note:/ Consider using 'lastStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatusMessage :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Types.LastStatusMessage)
idsiLastStatusMessage = Lens.field @"lastStatusMessage"
{-# DEPRECATED idsiLastStatusMessage "Use generic-lens or generic-optics with 'lastStatusMessage' instead." #-}

-- | The UTC timestamp of when the last status report.
--
-- /Note:/ Consider using 'lastStatusUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiLastStatusUpdateTime :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.NominalDiffTime)
idsiLastStatusUpdateTime = Lens.field @"lastStatusUpdateTime"
{-# DEPRECATED idsiLastStatusUpdateTime "Use generic-lens or generic-optics with 'lastStatusUpdateTime' instead." #-}

-- | The name of the inventory data type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiTypeName :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Types.TypeName)
idsiTypeName = Lens.field @"typeName"
{-# DEPRECATED idsiTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.FromJSON InventoryDeletionStatusItem where
  parseJSON =
    Core.withObject "InventoryDeletionStatusItem" Core.$
      \x ->
        InventoryDeletionStatusItem'
          Core.<$> (x Core..:? "DeletionId")
          Core.<*> (x Core..:? "DeletionStartTime")
          Core.<*> (x Core..:? "DeletionSummary")
          Core.<*> (x Core..:? "LastStatus")
          Core.<*> (x Core..:? "LastStatusMessage")
          Core.<*> (x Core..:? "LastStatusUpdateTime")
          Core.<*> (x Core..:? "TypeName")
