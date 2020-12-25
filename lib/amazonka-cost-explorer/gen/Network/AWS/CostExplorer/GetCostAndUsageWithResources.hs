{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetCostAndUsageWithResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics with resources for your account. You can specify which cost and usage-related metric, such as @BlendedCosts@ or @UsageQuantity@ , that you want the request to return. You can also filter and group your data by various dimensions, such as @SERVICE@ or @AZ@ , in a specific time range. For a complete list of valid dimensions, see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues> operation. Management account in an organization in AWS Organizations have access to all member accounts. This API is currently available for the Amazon Elastic Compute Cloud – Compute service only.
module Network.AWS.CostExplorer.GetCostAndUsageWithResources
  ( -- * Creating a request
    GetCostAndUsageWithResources (..),
    mkGetCostAndUsageWithResources,

    -- ** Request lenses
    gcauwrTimePeriod,
    gcauwrFilter,
    gcauwrGranularity,
    gcauwrGroupBy,
    gcauwrMetrics,
    gcauwrNextPageToken,

    -- * Destructuring the response
    GetCostAndUsageWithResourcesResponse (..),
    mkGetCostAndUsageWithResourcesResponse,

    -- ** Response lenses
    gcauwrrrsGroupDefinitions,
    gcauwrrrsNextPageToken,
    gcauwrrrsResultsByTime,
    gcauwrrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCostAndUsageWithResources' smart constructor.
data GetCostAndUsageWithResources = GetCostAndUsageWithResources'
  { -- | Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: Types.DateInterval,
    -- | Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
    --
    -- The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
    filter :: Types.Expression,
    -- | Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
    granularity :: Core.Maybe Types.Granularity,
    -- | You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
    groupBy :: Core.Maybe [Types.GroupDefinition],
    -- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
    --
    -- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
    -- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
    metrics :: Core.Maybe [Types.MetricName],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostAndUsageWithResources' value with any optional fields omitted.
mkGetCostAndUsageWithResources ::
  -- | 'timePeriod'
  Types.DateInterval ->
  -- | 'filter'
  Types.Expression ->
  GetCostAndUsageWithResources
mkGetCostAndUsageWithResources timePeriod filter =
  GetCostAndUsageWithResources'
    { timePeriod,
      filter,
      granularity = Core.Nothing,
      groupBy = Core.Nothing,
      metrics = Core.Nothing,
      nextPageToken = Core.Nothing
    }

-- | Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrTimePeriod :: Lens.Lens' GetCostAndUsageWithResources Types.DateInterval
gcauwrTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED gcauwrTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrFilter :: Lens.Lens' GetCostAndUsageWithResources Types.Expression
gcauwrFilter = Lens.field @"filter"
{-# DEPRECATED gcauwrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrGranularity :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe Types.Granularity)
gcauwrGranularity = Lens.field @"granularity"
{-# DEPRECATED gcauwrGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrGroupBy :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe [Types.GroupDefinition])
gcauwrGroupBy = Lens.field @"groupBy"
{-# DEPRECATED gcauwrGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrMetrics :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe [Types.MetricName])
gcauwrMetrics = Lens.field @"metrics"
{-# DEPRECATED gcauwrMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrNextPageToken :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe Types.NextPageToken)
gcauwrNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gcauwrNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

instance Core.FromJSON GetCostAndUsageWithResources where
  toJSON GetCostAndUsageWithResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Filter" Core..= filter),
            ("Granularity" Core..=) Core.<$> granularity,
            ("GroupBy" Core..=) Core.<$> groupBy,
            ("Metrics" Core..=) Core.<$> metrics,
            ("NextPageToken" Core..=) Core.<$> nextPageToken
          ]
      )

instance Core.AWSRequest GetCostAndUsageWithResources where
  type
    Rs GetCostAndUsageWithResources =
      GetCostAndUsageWithResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.GetCostAndUsageWithResources"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageWithResourcesResponse'
            Core.<$> (x Core..:? "GroupDefinitions")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ResultsByTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCostAndUsageWithResourcesResponse' smart constructor.
data GetCostAndUsageWithResourcesResponse = GetCostAndUsageWithResourcesResponse'
  { -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
    groupDefinitions :: Core.Maybe [Types.GroupDefinition],
    -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The time period that is covered by the results in the response.
    resultsByTime :: Core.Maybe [Types.ResultByTime],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostAndUsageWithResourcesResponse' value with any optional fields omitted.
mkGetCostAndUsageWithResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCostAndUsageWithResourcesResponse
mkGetCostAndUsageWithResourcesResponse responseStatus =
  GetCostAndUsageWithResourcesResponse'
    { groupDefinitions =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      resultsByTime = Core.Nothing,
      responseStatus
    }

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- /Note:/ Consider using 'groupDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrrsGroupDefinitions :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe [Types.GroupDefinition])
gcauwrrrsGroupDefinitions = Lens.field @"groupDefinitions"
{-# DEPRECATED gcauwrrrsGroupDefinitions "Use generic-lens or generic-optics with 'groupDefinitions' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrrsNextPageToken :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe Types.NextPageToken)
gcauwrrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gcauwrrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The time period that is covered by the results in the response.
--
-- /Note:/ Consider using 'resultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrrsResultsByTime :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe [Types.ResultByTime])
gcauwrrrsResultsByTime = Lens.field @"resultsByTime"
{-# DEPRECATED gcauwrrrsResultsByTime "Use generic-lens or generic-optics with 'resultsByTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrrsResponseStatus :: Lens.Lens' GetCostAndUsageWithResourcesResponse Core.Int
gcauwrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcauwrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
