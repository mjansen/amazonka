{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for your EC2 resources.
--
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtFilters
    , dtNextToken
    , dtDryRun
    , dtMaxResults

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsNextToken
    , dtrsTags
    , dtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
  { _dtFilters    :: !(Maybe [Filter])
  , _dtNextToken  :: !(Maybe Text)
  , _dtDryRun     :: !(Maybe Bool)
  , _dtMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtFilters' - The filters.     * @key@ - The tag key.     * @resource-id@ - The ID of the resource.     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".     * @value@ - The tag value.
--
-- * 'dtNextToken' - The token to retrieve the next page of results.
--
-- * 'dtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtMaxResults' - The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeTags
    :: DescribeTags
describeTags =
  DescribeTags'
    { _dtFilters = Nothing
    , _dtNextToken = Nothing
    , _dtDryRun = Nothing
    , _dtMaxResults = Nothing
    }


-- | The filters.     * @key@ - The tag key.     * @resource-id@ - The ID of the resource.     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".     * @value@ - The tag value.
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\ s a -> s{_dtNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtDryRun :: Lens' DescribeTags (Maybe Bool)
dtDryRun = lens _dtDryRun (\ s a -> s{_dtDryRun = a})

-- | The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\ s a -> s{_dtMaxResults = a})

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrsNextToken) = Nothing
          | stop (rs ^. dtrsTags) = Nothing
          | otherwise =
            Just $ rq & dtNextToken .~ rs ^. dtrsNextToken

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "tagSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTags where

instance NFData DescribeTags where

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Action" =: ("DescribeTags" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dtFilters),
               "NextToken" =: _dtNextToken, "DryRun" =: _dtDryRun,
               "MaxResults" =: _dtMaxResults]

-- | /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { _dtrsNextToken      :: !(Maybe Text)
  , _dtrsTags           :: !(Maybe [TagDescription])
  , _dtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtrsTags' - The tags.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { _dtrsNextToken = Nothing
    , _dtrsTags = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\ s a -> s{_dtrsNextToken = a})

-- | The tags.
dtrsTags :: Lens' DescribeTagsResponse [TagDescription]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DescribeTagsResponse where
