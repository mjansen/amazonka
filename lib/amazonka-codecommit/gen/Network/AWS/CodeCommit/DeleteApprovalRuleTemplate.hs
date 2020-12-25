{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified approval rule template. Deleting a template does not remove approval rules on pull requests already created with the template.
module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
  ( -- * Creating a request
    DeleteApprovalRuleTemplate (..),
    mkDeleteApprovalRuleTemplate,

    -- ** Request lenses
    dartApprovalRuleTemplateName,

    -- * Destructuring the response
    DeleteApprovalRuleTemplateResponse (..),
    mkDeleteApprovalRuleTemplateResponse,

    -- ** Response lenses
    dartrrsApprovalRuleTemplateId,
    dartrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApprovalRuleTemplate' smart constructor.
newtype DeleteApprovalRuleTemplate = DeleteApprovalRuleTemplate'
  { -- | The name of the approval rule template to delete.
    approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApprovalRuleTemplate' value with any optional fields omitted.
mkDeleteApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Types.ApprovalRuleTemplateName ->
  DeleteApprovalRuleTemplate
mkDeleteApprovalRuleTemplate approvalRuleTemplateName =
  DeleteApprovalRuleTemplate' {approvalRuleTemplateName}

-- | The name of the approval rule template to delete.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartApprovalRuleTemplateName :: Lens.Lens' DeleteApprovalRuleTemplate Types.ApprovalRuleTemplateName
dartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# DEPRECATED dartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Core.FromJSON DeleteApprovalRuleTemplate where
  toJSON DeleteApprovalRuleTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("approvalRuleTemplateName" Core..= approvalRuleTemplateName)
          ]
      )

instance Core.AWSRequest DeleteApprovalRuleTemplate where
  type
    Rs DeleteApprovalRuleTemplate =
      DeleteApprovalRuleTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.DeleteApprovalRuleTemplate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApprovalRuleTemplateResponse'
            Core.<$> (x Core..: "approvalRuleTemplateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteApprovalRuleTemplateResponse' smart constructor.
data DeleteApprovalRuleTemplateResponse = DeleteApprovalRuleTemplateResponse'
  { -- | The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
    approvalRuleTemplateId :: Types.ApprovalRuleTemplateId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApprovalRuleTemplateResponse' value with any optional fields omitted.
mkDeleteApprovalRuleTemplateResponse ::
  -- | 'approvalRuleTemplateId'
  Types.ApprovalRuleTemplateId ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteApprovalRuleTemplateResponse
mkDeleteApprovalRuleTemplateResponse
  approvalRuleTemplateId
  responseStatus =
    DeleteApprovalRuleTemplateResponse'
      { approvalRuleTemplateId,
        responseStatus
      }

-- | The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartrrsApprovalRuleTemplateId :: Lens.Lens' DeleteApprovalRuleTemplateResponse Types.ApprovalRuleTemplateId
dartrrsApprovalRuleTemplateId = Lens.field @"approvalRuleTemplateId"
{-# DEPRECATED dartrrsApprovalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartrrsResponseStatus :: Lens.Lens' DeleteApprovalRuleTemplateResponse Core.Int
dartrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dartrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
