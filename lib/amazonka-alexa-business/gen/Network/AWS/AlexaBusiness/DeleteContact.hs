{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact by the contact ARN.
module Network.AWS.AlexaBusiness.DeleteContact
  ( -- * Creating a request
    DeleteContact (..),
    mkDeleteContact,

    -- ** Request lenses
    dcContactArn,

    -- * Destructuring the response
    DeleteContactResponse (..),
    mkDeleteContactResponse,

    -- ** Response lenses
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContact' smart constructor.
newtype DeleteContact = DeleteContact'
  { -- | The ARN of the contact to delete.
    contactArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContact' value with any optional fields omitted.
mkDeleteContact ::
  -- | 'contactArn'
  Types.Arn ->
  DeleteContact
mkDeleteContact contactArn = DeleteContact' {contactArn}

-- | The ARN of the contact to delete.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcContactArn :: Lens.Lens' DeleteContact Types.Arn
dcContactArn = Lens.field @"contactArn"
{-# DEPRECATED dcContactArn "Use generic-lens or generic-optics with 'contactArn' instead." #-}

instance Core.FromJSON DeleteContact where
  toJSON DeleteContact {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ContactArn" Core..= contactArn)])

instance Core.AWSRequest DeleteContact where
  type Rs DeleteContact = DeleteContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteContact")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteContactResponse' smart constructor.
newtype DeleteContactResponse = DeleteContactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContactResponse' value with any optional fields omitted.
mkDeleteContactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteContactResponse
mkDeleteContactResponse responseStatus =
  DeleteContactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteContactResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
