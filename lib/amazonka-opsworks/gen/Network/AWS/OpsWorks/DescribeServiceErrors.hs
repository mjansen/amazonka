{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes AWS OpsWorks Stacks service errors.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
module Network.AWS.OpsWorks.DescribeServiceErrors
  ( -- * Creating a request
    DescribeServiceErrors (..),
    mkDescribeServiceErrors,

    -- ** Request lenses
    dseInstanceId,
    dseServiceErrorIds,
    dseStackId,

    -- * Destructuring the response
    DescribeServiceErrorsResponse (..),
    mkDescribeServiceErrorsResponse,

    -- ** Response lenses
    dserrsServiceErrors,
    dserrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServiceErrors' smart constructor.
data DescribeServiceErrors = DescribeServiceErrors'
  { -- | The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
    instanceId :: Core.Maybe Types.String,
    -- | An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
    serviceErrorIds :: Core.Maybe [Types.String],
    -- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceErrors' value with any optional fields omitted.
mkDescribeServiceErrors ::
  DescribeServiceErrors
mkDescribeServiceErrors =
  DescribeServiceErrors'
    { instanceId = Core.Nothing,
      serviceErrorIds = Core.Nothing,
      stackId = Core.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseInstanceId :: Lens.Lens' DescribeServiceErrors (Core.Maybe Types.String)
dseInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dseInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
--
-- /Note:/ Consider using 'serviceErrorIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseServiceErrorIds :: Lens.Lens' DescribeServiceErrors (Core.Maybe [Types.String])
dseServiceErrorIds = Lens.field @"serviceErrorIds"
{-# DEPRECATED dseServiceErrorIds "Use generic-lens or generic-optics with 'serviceErrorIds' instead." #-}

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseStackId :: Lens.Lens' DescribeServiceErrors (Core.Maybe Types.String)
dseStackId = Lens.field @"stackId"
{-# DEPRECATED dseStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeServiceErrors where
  toJSON DescribeServiceErrors {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("ServiceErrorIds" Core..=) Core.<$> serviceErrorIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeServiceErrors where
  type Rs DescribeServiceErrors = DescribeServiceErrorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeServiceErrors")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceErrorsResponse'
            Core.<$> (x Core..:? "ServiceErrors")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeServiceErrors@ request.
--
-- /See:/ 'mkDescribeServiceErrorsResponse' smart constructor.
data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'
  { -- | An array of @ServiceError@ objects that describe the specified service errors.
    serviceErrors :: Core.Maybe [Types.ServiceError'],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceErrorsResponse' value with any optional fields omitted.
mkDescribeServiceErrorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeServiceErrorsResponse
mkDescribeServiceErrorsResponse responseStatus =
  DescribeServiceErrorsResponse'
    { serviceErrors = Core.Nothing,
      responseStatus
    }

-- | An array of @ServiceError@ objects that describe the specified service errors.
--
-- /Note:/ Consider using 'serviceErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dserrsServiceErrors :: Lens.Lens' DescribeServiceErrorsResponse (Core.Maybe [Types.ServiceError'])
dserrsServiceErrors = Lens.field @"serviceErrors"
{-# DEPRECATED dserrsServiceErrors "Use generic-lens or generic-optics with 'serviceErrors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dserrsResponseStatus :: Lens.Lens' DescribeServiceErrorsResponse Core.Int
dserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
