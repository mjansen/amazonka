{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLoa
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at AWS Direct Connect Locations> in the /AWS Direct Connect User Guide/ .
module Network.AWS.DirectConnect.DescribeLoa
  ( -- * Creating a request
    DescribeLoa (..),
    mkDescribeLoa,

    -- ** Request lenses
    dlConnectionId,
    dlLoaContentType,
    dlProviderName,

    -- * Destructuring the response
    DescribeLoaResponse (..),
    mkDescribeLoaResponse,

    -- ** Response lenses
    dlrrsLoaContent,
    dlrrsLoaContentType,
    dlrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoa' smart constructor.
data DescribeLoa = DescribeLoa'
  { -- | The ID of a connection, LAG, or interconnect.
    connectionId :: Types.ConnectionId,
    -- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
    loaContentType :: Core.Maybe Types.LoaContentType,
    -- | The name of the service provider who establishes connectivity on your behalf. If you specify this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect.
    providerName :: Core.Maybe Types.ProviderName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoa' value with any optional fields omitted.
mkDescribeLoa ::
  -- | 'connectionId'
  Types.ConnectionId ->
  DescribeLoa
mkDescribeLoa connectionId =
  DescribeLoa'
    { connectionId,
      loaContentType = Core.Nothing,
      providerName = Core.Nothing
    }

-- | The ID of a connection, LAG, or interconnect.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectionId :: Lens.Lens' DescribeLoa Types.ConnectionId
dlConnectionId = Lens.field @"connectionId"
{-# DEPRECATED dlConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
--
-- /Note:/ Consider using 'loaContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLoaContentType :: Lens.Lens' DescribeLoa (Core.Maybe Types.LoaContentType)
dlLoaContentType = Lens.field @"loaContentType"
{-# DEPRECATED dlLoaContentType "Use generic-lens or generic-optics with 'loaContentType' instead." #-}

-- | The name of the service provider who establishes connectivity on your behalf. If you specify this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlProviderName :: Lens.Lens' DescribeLoa (Core.Maybe Types.ProviderName)
dlProviderName = Lens.field @"providerName"
{-# DEPRECATED dlProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Core.FromJSON DescribeLoa where
  toJSON DescribeLoa {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            ("loaContentType" Core..=) Core.<$> loaContentType,
            ("providerName" Core..=) Core.<$> providerName
          ]
      )

instance Core.AWSRequest DescribeLoa where
  type Rs DescribeLoa = DescribeLoaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DescribeLoa")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoaResponse'
            Core.<$> (x Core..:? "loaContent")
            Core.<*> (x Core..:? "loaContentType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Information about a Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.
--
-- /See:/ 'mkDescribeLoaResponse' smart constructor.
data DescribeLoaResponse = DescribeLoaResponse'
  { -- | The binary contents of the LOA-CFA document.
    loaContent :: Core.Maybe Core.Base64,
    -- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
    loaContentType :: Core.Maybe Types.LoaContentType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoaResponse' value with any optional fields omitted.
mkDescribeLoaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoaResponse
mkDescribeLoaResponse responseStatus =
  DescribeLoaResponse'
    { loaContent = Core.Nothing,
      loaContentType = Core.Nothing,
      responseStatus
    }

-- | The binary contents of the LOA-CFA document.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'loaContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLoaContent :: Lens.Lens' DescribeLoaResponse (Core.Maybe Core.Base64)
dlrrsLoaContent = Lens.field @"loaContent"
{-# DEPRECATED dlrrsLoaContent "Use generic-lens or generic-optics with 'loaContent' instead." #-}

-- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
--
-- /Note:/ Consider using 'loaContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLoaContentType :: Lens.Lens' DescribeLoaResponse (Core.Maybe Types.LoaContentType)
dlrrsLoaContentType = Lens.field @"loaContentType"
{-# DEPRECATED dlrrsLoaContentType "Use generic-lens or generic-optics with 'loaContentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLoaResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
