{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the permission of another AWS account to be able to put events to the specified event bus. Specify the account to revoke by the @StatementId@ value that you associated with the account when you granted it permission with @PutPermission@ . You can find the @StatementId@ by using 'DescribeEventBus' .
module Network.AWS.CloudWatchEvents.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpEventBusName,
    rpRemoveAllPermissions,
    rpStatementId,

    -- * Destructuring the response
    RemovePermissionResponse (..),
    mkRemovePermissionResponse,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.NonPartnerEventBusName,
    -- | Specifies whether to remove all permissions.
    removeAllPermissions :: Core.Maybe Core.Bool,
    -- | The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
    statementId :: Core.Maybe Types.StatementId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermission' value with any optional fields omitted.
mkRemovePermission ::
  RemovePermission
mkRemovePermission =
  RemovePermission'
    { eventBusName = Core.Nothing,
      removeAllPermissions = Core.Nothing,
      statementId = Core.Nothing
    }

-- | The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpEventBusName :: Lens.Lens' RemovePermission (Core.Maybe Types.NonPartnerEventBusName)
rpEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED rpEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | Specifies whether to remove all permissions.
--
-- /Note:/ Consider using 'removeAllPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRemoveAllPermissions :: Lens.Lens' RemovePermission (Core.Maybe Core.Bool)
rpRemoveAllPermissions = Lens.field @"removeAllPermissions"
{-# DEPRECATED rpRemoveAllPermissions "Use generic-lens or generic-optics with 'removeAllPermissions' instead." #-}

-- | The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatementId :: Lens.Lens' RemovePermission (Core.Maybe Types.StatementId)
rpStatementId = Lens.field @"statementId"
{-# DEPRECATED rpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

instance Core.FromJSON RemovePermission where
  toJSON RemovePermission {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventBusName" Core..=) Core.<$> eventBusName,
            ("RemoveAllPermissions" Core..=) Core.<$> removeAllPermissions,
            ("StatementId" Core..=) Core.<$> statementId
          ]
      )

instance Core.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.RemovePermission")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RemovePermissionResponse'

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermissionResponse' value with any optional fields omitted.
mkRemovePermissionResponse ::
  RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'
