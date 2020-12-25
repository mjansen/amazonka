{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster parameter group.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameters
  ( -- * Creating a request
    DescribeDBClusterParameters (..),
    mkDescribeDBClusterParameters,

    -- ** Request lenses
    ddbcpDBClusterParameterGroupName,
    ddbcpFilters,
    ddbcpMarker,
    ddbcpMaxRecords,
    ddbcpSource,

    -- * Destructuring the response
    DescribeDBClusterParametersResponse (..),
    mkDescribeDBClusterParametersResponse,

    -- ** Response lenses
    ddbcprrsMarker,
    ddbcprrsParameters,
    ddbcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { -- | The name of a specific DB cluster parameter group to return parameter details for.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBClusterParameterGroup.
    dBClusterParameterGroupName :: Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
    source :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParameters' value with any optional fields omitted.
mkDescribeDBClusterParameters ::
  -- | 'dBClusterParameterGroupName'
  Types.String ->
  DescribeDBClusterParameters
mkDescribeDBClusterParameters dBClusterParameterGroupName =
  DescribeDBClusterParameters'
    { dBClusterParameterGroupName,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      source = Core.Nothing
    }

-- | The name of a specific DB cluster parameter group to return parameter details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpDBClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Types.String
ddbcpDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED ddbcpDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpFilters :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe [Types.Filter])
ddbcpFilters = Lens.field @"filters"
{-# DEPRECATED ddbcpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpMarker :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Types.String)
ddbcpMarker = Lens.field @"marker"
{-# DEPRECATED ddbcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpMaxRecords :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Int)
ddbcpMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpSource :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Types.String)
ddbcpSource = Lens.field @"source"
{-# DEPRECATED ddbcpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.AWSRequest DescribeDBClusterParameters where
  type
    Rs DescribeDBClusterParameters =
      DescribeDBClusterParametersResponse
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
            ( Core.pure ("Action", "DescribeDBClusterParameters")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterParameterGroupName"
                            dBClusterParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "Source" Core.<$> source)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParametersResult"
      ( \s h x ->
          DescribeDBClusterParametersResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBClusterParameters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Provides details about a DB cluster parameter group including the parameters in the DB cluster parameter group.
--
-- /See:/ 'mkDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { -- | An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | Provides a list of parameters for the DB cluster parameter group.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParametersResponse' value with any optional fields omitted.
mkDescribeDBClusterParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBClusterParametersResponse
mkDescribeDBClusterParametersResponse responseStatus =
  DescribeDBClusterParametersResponse'
    { marker = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsMarker :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe Types.String)
ddbcprrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbcprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Provides a list of parameters for the DB cluster parameter group.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsParameters :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe [Types.Parameter])
ddbcprrsParameters = Lens.field @"parameters"
{-# DEPRECATED ddbcprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcprrsResponseStatus :: Lens.Lens' DescribeDBClusterParametersResponse Core.Int
ddbcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
