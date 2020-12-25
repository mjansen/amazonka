{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified security groups or all of your security groups.
--
-- A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ and <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSecurityGroups
  ( -- * Creating a request
    DescribeSecurityGroups (..),
    mkDescribeSecurityGroups,

    -- ** Request lenses
    dsgsDryRun,
    dsgsFilters,
    dsgsGroupIds,
    dsgsGroupNames,
    dsgsMaxResults,
    dsgsNextToken,

    -- * Destructuring the response
    DescribeSecurityGroupsResponse (..),
    mkDescribeSecurityGroupsResponse,

    -- ** Response lenses
    dsgrrsNextToken,
    dsgrrsSecurityGroups,
    dsgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSecurityGroups' smart constructor.
data DescribeSecurityGroups = DescribeSecurityGroups'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.
    --
    --
    --     * @description@ - The description of the security group.
    --
    --
    --     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.
    --
    --
    --     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
    --
    --
    --     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.
    --
    --
    --     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.
    --
    --
    --     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.
    --
    --
    --     * @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to which a security group rule allows outbound access.
    --
    --
    --     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
    --
    --
    --     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
    --
    --
    --     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.
    --
    --
    --     * @group-id@ - The ID of the security group.
    --
    --
    --     * @group-name@ - The name of the security group.
    --
    --
    --     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.
    --
    --
    --     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
    --
    --
    --     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.
    --
    --
    --     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.
    --
    --
    --     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.
    --
    --
    --     * @ip-permission.prefix-list-id@ - The ID of a prefix list from which a security group rule allows inbound access.
    --
    --
    --     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
    --
    --
    --     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
    --
    --
    --     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.
    --
    --
    --     * @owner-id@ - The AWS account ID of the owner of the security group.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC specified when the security group was created.
    filters :: Core.Maybe [Types.Filter],
    -- | The IDs of the security groups. Required for security groups in a nondefault VPC.
    --
    -- Default: Describes all your security groups.
    groupIds :: Core.Maybe [Types.String],
    -- | [EC2-Classic and default VPC only] The names of the security groups. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name.
    --
    -- Default: Describes all your security groups.
    groupNames :: Core.Maybe [Types.SecurityGroupName],
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityGroups' value with any optional fields omitted.
mkDescribeSecurityGroups ::
  DescribeSecurityGroups
mkDescribeSecurityGroups =
  DescribeSecurityGroups'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      groupIds = Core.Nothing,
      groupNames = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsDryRun :: Lens.Lens' DescribeSecurityGroups (Core.Maybe Core.Bool)
dsgsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.
--
--
--     * @description@ - The description of the security group.
--
--
--     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to which a security group rule allows outbound access.
--
--
--     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.
--
--
--     * @group-id@ - The ID of the security group.
--
--
--     * @group-name@ - The name of the security group.
--
--
--     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.prefix-list-id@ - The ID of a prefix list from which a security group rule allows inbound access.
--
--
--     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.
--
--
--     * @owner-id@ - The AWS account ID of the owner of the security group.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC specified when the security group was created.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsFilters :: Lens.Lens' DescribeSecurityGroups (Core.Maybe [Types.Filter])
dsgsFilters = Lens.field @"filters"
{-# DEPRECATED dsgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the security groups. Required for security groups in a nondefault VPC.
--
-- Default: Describes all your security groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsGroupIds :: Lens.Lens' DescribeSecurityGroups (Core.Maybe [Types.String])
dsgsGroupIds = Lens.field @"groupIds"
{-# DEPRECATED dsgsGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | [EC2-Classic and default VPC only] The names of the security groups. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name.
--
-- Default: Describes all your security groups.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsGroupNames :: Lens.Lens' DescribeSecurityGroups (Core.Maybe [Types.SecurityGroupName])
dsgsGroupNames = Lens.field @"groupNames"
{-# DEPRECATED dsgsGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsMaxResults :: Lens.Lens' DescribeSecurityGroups (Core.Maybe Core.Natural)
dsgsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsgsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsNextToken :: Lens.Lens' DescribeSecurityGroups (Core.Maybe Types.String)
dsgsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeSecurityGroups where
  type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse
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
            ( Core.pure ("Action", "DescribeSecurityGroups")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "GroupId" Core.<$> groupIds)
                Core.<> (Core.toQueryList "GroupName" Core.<$> groupNames)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "securityGroupInfo" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSecurityGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"securityGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeSecurityGroupsResponse' smart constructor.
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the security groups.
    securityGroups :: Core.Maybe [Types.SecurityGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeSecurityGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSecurityGroupsResponse
mkDescribeSecurityGroupsResponse responseStatus =
  DescribeSecurityGroupsResponse'
    { nextToken = Core.Nothing,
      securityGroups = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsNextToken :: Lens.Lens' DescribeSecurityGroupsResponse (Core.Maybe Types.String)
dsgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsSecurityGroups :: Lens.Lens' DescribeSecurityGroupsResponse (Core.Maybe [Types.SecurityGroup])
dsgrrsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED dsgrrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DescribeSecurityGroupsResponse Core.Int
dsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
