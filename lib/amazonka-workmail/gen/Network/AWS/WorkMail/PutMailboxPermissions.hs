{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets permissions for a user, group, or resource. This replaces any pre-existing permissions.
module Network.AWS.WorkMail.PutMailboxPermissions
  ( -- * Creating a request
    PutMailboxPermissions (..),
    mkPutMailboxPermissions,

    -- ** Request lenses
    pmpOrganizationId,
    pmpEntityId,
    pmpGranteeId,
    pmpPermissionValues,

    -- * Destructuring the response
    PutMailboxPermissionsResponse (..),
    mkPutMailboxPermissionsResponse,

    -- ** Response lenses
    pmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkPutMailboxPermissions' smart constructor.
data PutMailboxPermissions = PutMailboxPermissions'
  { -- | The identifier of the organization under which the user, group, or resource exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier of the user, group, or resource for which to update mailbox permissions.
    entityId :: Types.EntityId,
    -- | The identifier of the user, group, or resource to which to grant the permissions.
    granteeId :: Types.GranteeId,
    -- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
    permissionValues :: [Types.PermissionType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMailboxPermissions' value with any optional fields omitted.
mkPutMailboxPermissions ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.EntityId ->
  -- | 'granteeId'
  Types.GranteeId ->
  PutMailboxPermissions
mkPutMailboxPermissions organizationId entityId granteeId =
  PutMailboxPermissions'
    { organizationId,
      entityId,
      granteeId,
      permissionValues = Core.mempty
    }

-- | The identifier of the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpOrganizationId :: Lens.Lens' PutMailboxPermissions Types.OrganizationId
pmpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED pmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user, group, or resource for which to update mailbox permissions.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpEntityId :: Lens.Lens' PutMailboxPermissions Types.EntityId
pmpEntityId = Lens.field @"entityId"
{-# DEPRECATED pmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier of the user, group, or resource to which to grant the permissions.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpGranteeId :: Lens.Lens' PutMailboxPermissions Types.GranteeId
pmpGranteeId = Lens.field @"granteeId"
{-# DEPRECATED pmpGranteeId "Use generic-lens or generic-optics with 'granteeId' instead." #-}

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
--
-- /Note:/ Consider using 'permissionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpPermissionValues :: Lens.Lens' PutMailboxPermissions [Types.PermissionType]
pmpPermissionValues = Lens.field @"permissionValues"
{-# DEPRECATED pmpPermissionValues "Use generic-lens or generic-optics with 'permissionValues' instead." #-}

instance Core.FromJSON PutMailboxPermissions where
  toJSON PutMailboxPermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("GranteeId" Core..= granteeId),
            Core.Just ("PermissionValues" Core..= permissionValues)
          ]
      )

instance Core.AWSRequest PutMailboxPermissions where
  type Rs PutMailboxPermissions = PutMailboxPermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.PutMailboxPermissions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMailboxPermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutMailboxPermissionsResponse' smart constructor.
newtype PutMailboxPermissionsResponse = PutMailboxPermissionsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutMailboxPermissionsResponse' value with any optional fields omitted.
mkPutMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutMailboxPermissionsResponse
mkPutMailboxPermissionsResponse responseStatus =
  PutMailboxPermissionsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmprrsResponseStatus :: Lens.Lens' PutMailboxPermissionsResponse Core.Int
pmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
