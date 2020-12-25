{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metric policy for the specified container.
module Network.AWS.MediaStore.GetMetricPolicy
  ( -- * Creating a request
    GetMetricPolicy (..),
    mkGetMetricPolicy,

    -- ** Request lenses
    gmpContainerName,

    -- * Destructuring the response
    GetMetricPolicyResponse (..),
    mkGetMetricPolicyResponse,

    -- ** Response lenses
    gmprrsMetricPolicy,
    gmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMetricPolicy' smart constructor.
newtype GetMetricPolicy = GetMetricPolicy'
  { -- | The name of the container that is associated with the metric policy.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMetricPolicy' value with any optional fields omitted.
mkGetMetricPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  GetMetricPolicy
mkGetMetricPolicy containerName = GetMetricPolicy' {containerName}

-- | The name of the container that is associated with the metric policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpContainerName :: Lens.Lens' GetMetricPolicy Types.ContainerName
gmpContainerName = Lens.field @"containerName"
{-# DEPRECATED gmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON GetMetricPolicy where
  toJSON GetMetricPolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest GetMetricPolicy where
  type Rs GetMetricPolicy = GetMetricPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MediaStore_20170901.GetMetricPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricPolicyResponse'
            Core.<$> (x Core..: "MetricPolicy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMetricPolicyResponse' smart constructor.
data GetMetricPolicyResponse = GetMetricPolicyResponse'
  { -- | The metric policy that is associated with the specific container.
    metricPolicy :: Types.MetricPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMetricPolicyResponse' value with any optional fields omitted.
mkGetMetricPolicyResponse ::
  -- | 'metricPolicy'
  Types.MetricPolicy ->
  -- | 'responseStatus'
  Core.Int ->
  GetMetricPolicyResponse
mkGetMetricPolicyResponse metricPolicy responseStatus =
  GetMetricPolicyResponse' {metricPolicy, responseStatus}

-- | The metric policy that is associated with the specific container.
--
-- /Note:/ Consider using 'metricPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmprrsMetricPolicy :: Lens.Lens' GetMetricPolicyResponse Types.MetricPolicy
gmprrsMetricPolicy = Lens.field @"metricPolicy"
{-# DEPRECATED gmprrsMetricPolicy "Use generic-lens or generic-optics with 'metricPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmprrsResponseStatus :: Lens.Lens' GetMetricPolicyResponse Core.Int
gmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
