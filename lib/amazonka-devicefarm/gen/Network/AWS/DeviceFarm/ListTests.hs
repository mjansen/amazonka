{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about tests in a given test suite.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListTests
  ( -- * Creating a request
    ListTests (..),
    mkListTests,

    -- ** Request lenses
    ltArn,
    ltNextToken,

    -- * Destructuring the response
    ListTestsResponse (..),
    mkListTestsResponse,

    -- ** Response lenses
    ltrrsNextToken,
    ltrrsTests,
    ltrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list tests operation.
--
-- /See:/ 'mkListTests' smart constructor.
data ListTests = ListTests'
  { -- | The test suite's Amazon Resource Name (ARN).
    arn :: Types.AmazonResourceName,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTests' value with any optional fields omitted.
mkListTests ::
  -- | 'arn'
  Types.AmazonResourceName ->
  ListTests
mkListTests arn = ListTests' {arn, nextToken = Core.Nothing}

-- | The test suite's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltArn :: Lens.Lens' ListTests Types.AmazonResourceName
ltArn = Lens.field @"arn"
{-# DEPRECATED ltArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTests (Core.Maybe Types.PaginationToken)
ltNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTests where
  toJSON ListTests {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTests where
  type Rs ListTests = ListTestsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListTests")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "tests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTests where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tests" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the result of a list tests request.
--
-- /See:/ 'mkListTestsResponse' smart constructor.
data ListTestsResponse = ListTestsResponse'
  { -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Information about the tests.
    tests :: Core.Maybe [Types.Test],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTestsResponse' value with any optional fields omitted.
mkListTestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTestsResponse
mkListTestsResponse responseStatus =
  ListTestsResponse'
    { nextToken = Core.Nothing,
      tests = Core.Nothing,
      responseStatus
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTestsResponse (Core.Maybe Types.PaginationToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the tests.
--
-- /Note:/ Consider using 'tests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTests :: Lens.Lens' ListTestsResponse (Core.Maybe [Types.Test])
ltrrsTests = Lens.field @"tests"
{-# DEPRECATED ltrrsTests "Use generic-lens or generic-optics with 'tests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTestsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
