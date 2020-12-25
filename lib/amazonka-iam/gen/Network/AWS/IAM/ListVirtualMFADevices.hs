{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual MFA devices defined in the AWS account by assignment status. If you do not specify an assignment status, the operation returns a list of all virtual MFA devices. Assignment status can be @Assigned@ , @Unassigned@ , or @Any@ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListVirtualMFADevices
  ( -- * Creating a request
    ListVirtualMFADevices (..),
    mkListVirtualMFADevices,

    -- ** Request lenses
    lvmfadAssignmentStatus,
    lvmfadMarker,
    lvmfadMaxItems,

    -- * Destructuring the response
    ListVirtualMFADevicesResponse (..),
    mkListVirtualMFADevicesResponse,

    -- ** Response lenses
    lvmfadrrsVirtualMFADevices,
    lvmfadrrsIsTruncated,
    lvmfadrrsMarker,
    lvmfadrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVirtualMFADevices' smart constructor.
data ListVirtualMFADevices = ListVirtualMFADevices'
  { -- | The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ , which lists both assigned and unassigned virtual MFA devices.,
    assignmentStatus :: Core.Maybe Types.AssignmentStatusType,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVirtualMFADevices' value with any optional fields omitted.
mkListVirtualMFADevices ::
  ListVirtualMFADevices
mkListVirtualMFADevices =
  ListVirtualMFADevices'
    { assignmentStatus = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ , which lists both assigned and unassigned virtual MFA devices.,
--
-- /Note:/ Consider using 'assignmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadAssignmentStatus :: Lens.Lens' ListVirtualMFADevices (Core.Maybe Types.AssignmentStatusType)
lvmfadAssignmentStatus = Lens.field @"assignmentStatus"
{-# DEPRECATED lvmfadAssignmentStatus "Use generic-lens or generic-optics with 'assignmentStatus' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadMarker :: Lens.Lens' ListVirtualMFADevices (Core.Maybe Types.MarkerType)
lvmfadMarker = Lens.field @"marker"
{-# DEPRECATED lvmfadMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadMaxItems :: Lens.Lens' ListVirtualMFADevices (Core.Maybe Core.Natural)
lvmfadMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lvmfadMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListVirtualMFADevices where
  type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListVirtualMFADevices")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "AssignmentStatus" Core.<$> assignmentStatus)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListVirtualMFADevicesResult"
      ( \s h x ->
          ListVirtualMFADevicesResponse'
            Core.<$> ( x Core..@? "VirtualMFADevices" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListVirtualMFADevices where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListVirtualMFADevices' request.
--
-- /See:/ 'mkListVirtualMFADevicesResponse' smart constructor.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
  { -- | The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
    virtualMFADevices :: [Types.VirtualMFADevice],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListVirtualMFADevicesResponse' value with any optional fields omitted.
mkListVirtualMFADevicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVirtualMFADevicesResponse
mkListVirtualMFADevicesResponse responseStatus =
  ListVirtualMFADevicesResponse'
    { virtualMFADevices = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
--
-- /Note:/ Consider using 'virtualMFADevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadrrsVirtualMFADevices :: Lens.Lens' ListVirtualMFADevicesResponse [Types.VirtualMFADevice]
lvmfadrrsVirtualMFADevices = Lens.field @"virtualMFADevices"
{-# DEPRECATED lvmfadrrsVirtualMFADevices "Use generic-lens or generic-optics with 'virtualMFADevices' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadrrsIsTruncated :: Lens.Lens' ListVirtualMFADevicesResponse (Core.Maybe Core.Bool)
lvmfadrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lvmfadrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadrrsMarker :: Lens.Lens' ListVirtualMFADevicesResponse (Core.Maybe Types.ResponseMarkerType)
lvmfadrrsMarker = Lens.field @"marker"
{-# DEPRECATED lvmfadrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmfadrrsResponseStatus :: Lens.Lens' ListVirtualMFADevicesResponse Core.Int
lvmfadrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvmfadrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
