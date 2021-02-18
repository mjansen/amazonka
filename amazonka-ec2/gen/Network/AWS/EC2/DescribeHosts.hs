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
-- Module      : Network.AWS.EC2.DescribeHosts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Dedicated Hosts or all your Dedicated Hosts.
--
--
-- The results describe only the Dedicated Hosts in the Region you're currently using. All listed instances consume capacity on your Dedicated Host. Dedicated Hosts that have recently been released are listed with the state @released@ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHosts
    (
    -- * Creating a Request
      describeHosts
    , DescribeHosts
    -- * Request Lenses
    , dhNextToken
    , dhFilter
    , dhHostIds
    , dhMaxResults

    -- * Destructuring the Response
    , describeHostsResponse
    , DescribeHostsResponse
    -- * Response Lenses
    , dhrsHosts
    , dhrsNextToken
    , dhrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeHosts' smart constructor.
data DescribeHosts = DescribeHosts'
  { _dhNextToken  :: !(Maybe Text)
  , _dhFilter     :: !(Maybe [Filter])
  , _dhHostIds    :: !(Maybe [Text])
  , _dhMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhNextToken' - The token to use to retrieve the next page of results.
--
-- * 'dhFilter' - The filters.     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).     * @availability-zone@ - The Availability Zone of the host.     * @client-token@ - The idempotency token that you provided when you allocated the host.     * @host-reservation-id@ - The ID of the reservation assigned to this host.     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'dhHostIds' - The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
--
-- * 'dhMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error. You cannot specify this parameter and the host IDs parameter in the same request.
describeHosts
    :: DescribeHosts
describeHosts =
  DescribeHosts'
    { _dhNextToken = Nothing
    , _dhFilter = Nothing
    , _dhHostIds = Nothing
    , _dhMaxResults = Nothing
    }


-- | The token to use to retrieve the next page of results.
dhNextToken :: Lens' DescribeHosts (Maybe Text)
dhNextToken = lens _dhNextToken (\ s a -> s{_dhNextToken = a})

-- | The filters.     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).     * @availability-zone@ - The Availability Zone of the host.     * @client-token@ - The idempotency token that you provided when you allocated the host.     * @host-reservation-id@ - The ID of the reservation assigned to this host.     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
dhFilter :: Lens' DescribeHosts [Filter]
dhFilter = lens _dhFilter (\ s a -> s{_dhFilter = a}) . _Default . _Coerce

-- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
dhHostIds :: Lens' DescribeHosts [Text]
dhHostIds = lens _dhHostIds (\ s a -> s{_dhHostIds = a}) . _Default . _Coerce

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error. You cannot specify this parameter and the host IDs parameter in the same request.
dhMaxResults :: Lens' DescribeHosts (Maybe Int)
dhMaxResults = lens _dhMaxResults (\ s a -> s{_dhMaxResults = a})

instance AWSPager DescribeHosts where
        page rq rs
          | stop (rs ^. dhrsNextToken) = Nothing
          | stop (rs ^. dhrsHosts) = Nothing
          | otherwise =
            Just $ rq & dhNextToken .~ rs ^. dhrsNextToken

instance AWSRequest DescribeHosts where
        type Rs DescribeHosts = DescribeHostsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeHostsResponse' <$>
                   (x .@? "hostSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHosts where

instance NFData DescribeHosts where

instance ToHeaders DescribeHosts where
        toHeaders = const mempty

instance ToPath DescribeHosts where
        toPath = const "/"

instance ToQuery DescribeHosts where
        toQuery DescribeHosts'{..}
          = mconcat
              ["Action" =: ("DescribeHosts" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dhNextToken,
               toQuery (toQueryList "Filter" <$> _dhFilter),
               toQuery (toQueryList "HostId" <$> _dhHostIds),
               "MaxResults" =: _dhMaxResults]

-- | /See:/ 'describeHostsResponse' smart constructor.
data DescribeHostsResponse = DescribeHostsResponse'
  { _dhrsHosts          :: !(Maybe [Host])
  , _dhrsNextToken      :: !(Maybe Text)
  , _dhrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrsHosts' - Information about the Dedicated Hosts.
--
-- * 'dhrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dhrsResponseStatus' - -- | The response status code.
describeHostsResponse
    :: Int -- ^ 'dhrsResponseStatus'
    -> DescribeHostsResponse
describeHostsResponse pResponseStatus_ =
  DescribeHostsResponse'
    { _dhrsHosts = Nothing
    , _dhrsNextToken = Nothing
    , _dhrsResponseStatus = pResponseStatus_
    }


-- | Information about the Dedicated Hosts.
dhrsHosts :: Lens' DescribeHostsResponse [Host]
dhrsHosts = lens _dhrsHosts (\ s a -> s{_dhrsHosts = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dhrsNextToken :: Lens' DescribeHostsResponse (Maybe Text)
dhrsNextToken = lens _dhrsNextToken (\ s a -> s{_dhrsNextToken = a})

-- | -- | The response status code.
dhrsResponseStatus :: Lens' DescribeHostsResponse Int
dhrsResponseStatus = lens _dhrsResponseStatus (\ s a -> s{_dhrsResponseStatus = a})

instance NFData DescribeHostsResponse where
