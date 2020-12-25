{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A bundle describes the performance specifications for a database.
--
-- You can use a bundle ID to create a new database with explicit performance specifications.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBundles
  ( -- * Creating a request
    GetRelationalDatabaseBundles (..),
    mkGetRelationalDatabaseBundles,

    -- ** Request lenses
    grdbsPageToken,

    -- * Destructuring the response
    GetRelationalDatabaseBundlesResponse (..),
    mkGetRelationalDatabaseBundlesResponse,

    -- ** Response lenses
    grdbrfrsBundles,
    grdbrfrsNextPageToken,
    grdbrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseBundles' smart constructor.
newtype GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBundles' value with any optional fields omitted.
mkGetRelationalDatabaseBundles ::
  GetRelationalDatabaseBundles
mkGetRelationalDatabaseBundles =
  GetRelationalDatabaseBundles' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbsPageToken :: Lens.Lens' GetRelationalDatabaseBundles (Core.Maybe Types.String)
grdbsPageToken = Lens.field @"pageToken"
{-# DEPRECATED grdbsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetRelationalDatabaseBundles where
  toJSON GetRelationalDatabaseBundles {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseBundles where
  type
    Rs GetRelationalDatabaseBundles =
      GetRelationalDatabaseBundlesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetRelationalDatabaseBundles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBundlesResponse'
            Core.<$> (x Core..:? "bundles")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetRelationalDatabaseBundles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"bundles" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { -- | An object describing the result of your get relational database bundles request.
    bundles :: Core.Maybe [Types.RelationalDatabaseBundle],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseBundlesResponse' value with any optional fields omitted.
mkGetRelationalDatabaseBundlesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRelationalDatabaseBundlesResponse
mkGetRelationalDatabaseBundlesResponse responseStatus =
  GetRelationalDatabaseBundlesResponse'
    { bundles = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An object describing the result of your get relational database bundles request.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrfrsBundles :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Core.Maybe [Types.RelationalDatabaseBundle])
grdbrfrsBundles = Lens.field @"bundles"
{-# DEPRECATED grdbrfrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrfrsNextPageToken :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Core.Maybe Types.String)
grdbrfrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grdbrfrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrfrsResponseStatus :: Lens.Lens' GetRelationalDatabaseBundlesResponse Core.Int
grdbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
