{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.ListTableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metadata for the tables in the specified data catalog database.
--
-- This operation returns paginated results.
module Network.AWS.Athena.ListTableMetadata
  ( -- * Creating a request
    ListTableMetadata (..),
    mkListTableMetadata,

    -- ** Request lenses
    ltmCatalogName,
    ltmDatabaseName,
    ltmExpression,
    ltmMaxResults,
    ltmNextToken,

    -- * Destructuring the response
    ListTableMetadataResponse (..),
    mkListTableMetadataResponse,

    -- ** Response lenses
    ltmrrsNextToken,
    ltmrrsTableMetadataList,
    ltmrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTableMetadata' smart constructor.
data ListTableMetadata = ListTableMetadata'
  { -- | The name of the data catalog for which table metadata should be returned.
    catalogName :: Types.CatalogName,
    -- | The name of the database for which table metadata should be returned.
    databaseName :: Types.DatabaseName,
    -- | A regex filter that pattern-matches table names. If no expression is supplied, metadata for all tables are listed.
    expression :: Core.Maybe Types.Expression,
    -- | Specifies the maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the NextToken from the response object of the previous page call.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTableMetadata' value with any optional fields omitted.
mkListTableMetadata ::
  -- | 'catalogName'
  Types.CatalogName ->
  -- | 'databaseName'
  Types.DatabaseName ->
  ListTableMetadata
mkListTableMetadata catalogName databaseName =
  ListTableMetadata'
    { catalogName,
      databaseName,
      expression = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the data catalog for which table metadata should be returned.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmCatalogName :: Lens.Lens' ListTableMetadata Types.CatalogName
ltmCatalogName = Lens.field @"catalogName"
{-# DEPRECATED ltmCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The name of the database for which table metadata should be returned.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmDatabaseName :: Lens.Lens' ListTableMetadata Types.DatabaseName
ltmDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ltmDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A regex filter that pattern-matches table names. If no expression is supplied, metadata for all tables are listed.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmExpression :: Lens.Lens' ListTableMetadata (Core.Maybe Types.Expression)
ltmExpression = Lens.field @"expression"
{-# DEPRECATED ltmExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | Specifies the maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmMaxResults :: Lens.Lens' ListTableMetadata (Core.Maybe Core.Natural)
ltmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the NextToken from the response object of the previous page call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmNextToken :: Lens.Lens' ListTableMetadata (Core.Maybe Types.Token)
ltmNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTableMetadata where
  toJSON ListTableMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CatalogName" Core..= catalogName),
            Core.Just ("DatabaseName" Core..= databaseName),
            ("Expression" Core..=) Core.<$> expression,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTableMetadata where
  type Rs ListTableMetadata = ListTableMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.ListTableMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableMetadataResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TableMetadataList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTableMetadata where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"tableMetadataList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTableMetadataResponse' smart constructor.
data ListTableMetadataResponse = ListTableMetadataResponse'
  { -- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the NextToken from the response object of the previous page call.
    nextToken :: Core.Maybe Types.Token,
    -- | A list of table metadata.
    tableMetadataList :: Core.Maybe [Types.TableMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTableMetadataResponse' value with any optional fields omitted.
mkListTableMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTableMetadataResponse
mkListTableMetadataResponse responseStatus =
  ListTableMetadataResponse'
    { nextToken = Core.Nothing,
      tableMetadataList = Core.Nothing,
      responseStatus
    }

-- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the NextToken from the response object of the previous page call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsNextToken :: Lens.Lens' ListTableMetadataResponse (Core.Maybe Types.Token)
ltmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of table metadata.
--
-- /Note:/ Consider using 'tableMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsTableMetadataList :: Lens.Lens' ListTableMetadataResponse (Core.Maybe [Types.TableMetadata])
ltmrrsTableMetadataList = Lens.field @"tableMetadataList"
{-# DEPRECATED ltmrrsTableMetadataList "Use generic-lens or generic-optics with 'tableMetadataList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrrsResponseStatus :: Lens.Lens' ListTableMetadataResponse Core.Int
ltmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
