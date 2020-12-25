{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager protocols list.
module Network.AWS.FMS.DeleteProtocolsList
  ( -- * Creating a request
    DeleteProtocolsList (..),
    mkDeleteProtocolsList,

    -- ** Request lenses
    dplListId,

    -- * Destructuring the response
    DeleteProtocolsListResponse (..),
    mkDeleteProtocolsListResponse,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProtocolsList' smart constructor.
newtype DeleteProtocolsList = DeleteProtocolsList'
  { -- | The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
    listId :: Types.ListId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtocolsList' value with any optional fields omitted.
mkDeleteProtocolsList ::
  -- | 'listId'
  Types.ListId ->
  DeleteProtocolsList
mkDeleteProtocolsList listId = DeleteProtocolsList' {listId}

-- | The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplListId :: Lens.Lens' DeleteProtocolsList Types.ListId
dplListId = Lens.field @"listId"
{-# DEPRECATED dplListId "Use generic-lens or generic-optics with 'listId' instead." #-}

instance Core.FromJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ListId" Core..= listId)])

instance Core.AWSRequest DeleteProtocolsList where
  type Rs DeleteProtocolsList = DeleteProtocolsListResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.DeleteProtocolsList")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteProtocolsListResponse'

-- | /See:/ 'mkDeleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProtocolsListResponse' value with any optional fields omitted.
mkDeleteProtocolsListResponse ::
  DeleteProtocolsListResponse
mkDeleteProtocolsListResponse = DeleteProtocolsListResponse'
