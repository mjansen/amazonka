{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeMLModels
  ( -- * Creating a request
    DescribeMLModels (..),
    mkDescribeMLModels,

    -- ** Request lenses
    dmlmEQ,
    dmlmFilterVariable,
    dmlmGE,
    dmlmGT,
    dmlmLE,
    dmlmLT,
    dmlmLimit,
    dmlmNE,
    dmlmNextToken,
    dmlmPrefix,
    dmlmSortOrder,

    -- * Destructuring the response
    DescribeMLModelsResponse (..),
    mkDescribeMLModelsResponse,

    -- ** Response lenses
    dmlmrfrsNextToken,
    dmlmrfrsResults,
    dmlmrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMLModels' smart constructor.
data DescribeMLModels = DescribeMLModels'
  { -- | The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
    eq :: Core.Maybe Types.EQ,
    -- | Use one of the following variables to filter a list of @MLModel@ :
    --
    --
    --     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
    --
    --     * @Status@ - Sets the search criteria to @MLModel@ status.
    --
    --     * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .
    --
    --     * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.
    --
    --     * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .
    --
    --     * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.
    --
    --     * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.
    --
    --     * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.
    --
    --     * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
    filterVariable :: Core.Maybe Types.MLModelFilterVariable,
    -- | The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
    ge :: Core.Maybe Types.GE,
    -- | The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
    gt :: Core.Maybe Types.GT,
    -- | The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
    le :: Core.Maybe Types.LE,
    -- | The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
    lt :: Core.Maybe Types.LT,
    -- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
    limit :: Core.Maybe Core.Natural,
    -- | The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
    ne :: Core.Maybe Types.NE,
    -- | The ID of the page in the paginated results.
    nextToken :: Core.Maybe Types.StringType,
    -- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
    --
    -- For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
    --
    --     * 2014-09
    --
    --
    --     * 2014-09-09
    --
    --
    --     * 2014-09-09-Holiday
    prefix :: Core.Maybe Types.Prefix,
    -- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ .
    --
    --
    --     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    --     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMLModels' value with any optional fields omitted.
mkDescribeMLModels ::
  DescribeMLModels
mkDescribeMLModels =
  DescribeMLModels'
    { eq = Core.Nothing,
      filterVariable = Core.Nothing,
      ge = Core.Nothing,
      gt = Core.Nothing,
      le = Core.Nothing,
      lt = Core.Nothing,
      limit = Core.Nothing,
      ne = Core.Nothing,
      nextToken = Core.Nothing,
      prefix = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmEQ :: Lens.Lens' DescribeMLModels (Core.Maybe Types.EQ)
dmlmEQ = Lens.field @"eq"
{-# DEPRECATED dmlmEQ "Use generic-lens or generic-optics with 'eq' instead." #-}

-- | Use one of the following variables to filter a list of @MLModel@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
--
--     * @Status@ - Sets the search criteria to @MLModel@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.
--
--     * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .
--
--     * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.
--
--     * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.
--
--     * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.
--
--     * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
--
-- /Note:/ Consider using 'filterVariable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmFilterVariable :: Lens.Lens' DescribeMLModels (Core.Maybe Types.MLModelFilterVariable)
dmlmFilterVariable = Lens.field @"filterVariable"
{-# DEPRECATED dmlmFilterVariable "Use generic-lens or generic-optics with 'filterVariable' instead." #-}

-- | The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- /Note:/ Consider using 'ge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmGE :: Lens.Lens' DescribeMLModels (Core.Maybe Types.GE)
dmlmGE = Lens.field @"ge"
{-# DEPRECATED dmlmGE "Use generic-lens or generic-optics with 'ge' instead." #-}

-- | The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmGT :: Lens.Lens' DescribeMLModels (Core.Maybe Types.GT)
dmlmGT = Lens.field @"gt"
{-# DEPRECATED dmlmGT "Use generic-lens or generic-optics with 'gt' instead." #-}

-- | The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'le' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLE :: Lens.Lens' DescribeMLModels (Core.Maybe Types.LE)
dmlmLE = Lens.field @"le"
{-# DEPRECATED dmlmLE "Use generic-lens or generic-optics with 'le' instead." #-}

-- | The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLT :: Lens.Lens' DescribeMLModels (Core.Maybe Types.LT)
dmlmLT = Lens.field @"lt"
{-# DEPRECATED dmlmLT "Use generic-lens or generic-optics with 'lt' instead." #-}

-- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLimit :: Lens.Lens' DescribeMLModels (Core.Maybe Core.Natural)
dmlmLimit = Lens.field @"limit"
{-# DEPRECATED dmlmLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'ne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmNE :: Lens.Lens' DescribeMLModels (Core.Maybe Types.NE)
dmlmNE = Lens.field @"ne"
{-# DEPRECATED dmlmNE "Use generic-lens or generic-optics with 'ne' instead." #-}

-- | The ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmNextToken :: Lens.Lens' DescribeMLModels (Core.Maybe Types.StringType)
dmlmNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmlmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
--
--     * 2014-09
--
--
--     * 2014-09-09
--
--
--     * 2014-09-09-Holiday
--
--
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmPrefix :: Lens.Lens' DescribeMLModels (Core.Maybe Types.Prefix)
dmlmPrefix = Lens.field @"prefix"
{-# DEPRECATED dmlmPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmSortOrder :: Lens.Lens' DescribeMLModels (Core.Maybe Types.SortOrder)
dmlmSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED dmlmSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON DescribeMLModels where
  toJSON DescribeMLModels {..} =
    Core.object
      ( Core.catMaybes
          [ ("EQ" Core..=) Core.<$> eq,
            ("FilterVariable" Core..=) Core.<$> filterVariable,
            ("GE" Core..=) Core.<$> ge,
            ("GT" Core..=) Core.<$> gt,
            ("LE" Core..=) Core.<$> le,
            ("LT" Core..=) Core.<$> lt,
            ("Limit" Core..=) Core.<$> limit,
            ("NE" Core..=) Core.<$> ne,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Prefix" Core..=) Core.<$> prefix,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest DescribeMLModels where
  type Rs DescribeMLModels = DescribeMLModelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.DescribeMLModels")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMLModelsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Results")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMLModels where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"results" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @DescribeMLModels@ operation. The content is essentially a list of @MLModel@ .
--
-- /See:/ 'mkDescribeMLModelsResponse' smart constructor.
data DescribeMLModelsResponse = DescribeMLModelsResponse'
  { -- | The ID of the next page in the paginated results that indicates at least one more page follows.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @MLModel@ that meet the search criteria.
    results :: Core.Maybe [Types.MLModel],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeMLModelsResponse' value with any optional fields omitted.
mkDescribeMLModelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMLModelsResponse
mkDescribeMLModelsResponse responseStatus =
  DescribeMLModelsResponse'
    { nextToken = Core.Nothing,
      results = Core.Nothing,
      responseStatus
    }

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrfrsNextToken :: Lens.Lens' DescribeMLModelsResponse (Core.Maybe Types.NextToken)
dmlmrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmlmrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @MLModel@ that meet the search criteria.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrfrsResults :: Lens.Lens' DescribeMLModelsResponse (Core.Maybe [Types.MLModel])
dmlmrfrsResults = Lens.field @"results"
{-# DEPRECATED dmlmrfrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrfrsResponseStatus :: Lens.Lens' DescribeMLModelsResponse Core.Int
dmlmrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmlmrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
