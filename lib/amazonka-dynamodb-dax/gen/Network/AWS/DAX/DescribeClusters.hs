{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned DAX clusters if no cluster identifier is specified, or about a specific DAX cluster if a cluster identifier is supplied.
--
-- If the cluster is in the CREATING state, only cluster level information will be displayed until all of the nodes are successfully provisioned.
-- If the cluster is in the DELETING state, only cluster level information will be displayed.
-- If nodes are currently being added to the DAX cluster, node endpoint information and creation time for the additional nodes will not be displayed until they are completely provisioned. When the DAX cluster state is /available/ , the cluster is ready for use.
-- If nodes are currently being removed from the DAX cluster, no endpoint information for the removed nodes is displayed.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeClusters
  ( -- * Creating a request
    DescribeClusters (..),
    mkDescribeClusters,

    -- ** Request lenses
    dcClusterNames,
    dcMaxResults,
    dcNextToken,

    -- * Destructuring the response
    DescribeClustersResponse (..),
    mkDescribeClustersResponse,

    -- ** Response lenses
    dcrrsClusters,
    dcrrsNextToken,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | The names of the DAX clusters being described.
    clusterNames :: Core.Maybe [Types.String],
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusters' value with any optional fields omitted.
mkDescribeClusters ::
  DescribeClusters
mkDescribeClusters =
  DescribeClusters'
    { clusterNames = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The names of the DAX clusters being described.
--
-- /Note:/ Consider using 'clusterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterNames :: Lens.Lens' DescribeClusters (Core.Maybe [Types.String])
dcClusterNames = Lens.field @"clusterNames"
{-# DEPRECATED dcClusterNames "Use generic-lens or generic-optics with 'clusterNames' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeClusters (Core.Maybe Core.Int)
dcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeClusters (Core.Maybe Types.String)
dcNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeClusters where
  toJSON DescribeClusters {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClusterNames" Core..=) Core.<$> clusterNames,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeClusters where
  type Rs DescribeClusters = DescribeClustersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DescribeClusters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Core.<$> (x Core..:? "Clusters")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"clusters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
    clusters :: Core.Maybe [Types.Cluster],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeClustersResponse' value with any optional fields omitted.
mkDescribeClustersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClustersResponse
mkDescribeClustersResponse responseStatus =
  DescribeClustersResponse'
    { clusters = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsClusters :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Types.Cluster])
dcrrsClusters = Lens.field @"clusters"
{-# DEPRECATED dcrrsClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsNextToken :: Lens.Lens' DescribeClustersResponse (Core.Maybe Types.NextToken)
dcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeClustersResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
