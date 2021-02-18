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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayConnectPeers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect peers.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayConnectPeers
    (
    -- * Creating a Request
      describeTransitGatewayConnectPeers
    , DescribeTransitGatewayConnectPeers
    -- * Request Lenses
    , dtgcpsTransitGatewayConnectPeerIds
    , dtgcpsFilters
    , dtgcpsNextToken
    , dtgcpsDryRun
    , dtgcpsMaxResults

    -- * Destructuring the Response
    , describeTransitGatewayConnectPeersResponse
    , DescribeTransitGatewayConnectPeersResponse
    -- * Response Lenses
    , dtgcprsTransitGatewayConnectPeers
    , dtgcprsNextToken
    , dtgcprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayConnectPeers' smart constructor.
data DescribeTransitGatewayConnectPeers = DescribeTransitGatewayConnectPeers'
  { _dtgcpsTransitGatewayConnectPeerIds :: !(Maybe [Text])
  , _dtgcpsFilters                      :: !(Maybe [Filter])
  , _dtgcpsNextToken                    :: !(Maybe Text)
  , _dtgcpsDryRun                       :: !(Maybe Bool)
  , _dtgcpsMaxResults                   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayConnectPeers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcpsTransitGatewayConnectPeerIds' - The IDs of the Connect peers.
--
-- * 'dtgcpsFilters' - One or more filters. The possible values are:     * @state@ - The state of the Connect peer (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
--
-- * 'dtgcpsNextToken' - The token for the next page of results.
--
-- * 'dtgcpsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgcpsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayConnectPeers
    :: DescribeTransitGatewayConnectPeers
describeTransitGatewayConnectPeers =
  DescribeTransitGatewayConnectPeers'
    { _dtgcpsTransitGatewayConnectPeerIds = Nothing
    , _dtgcpsFilters = Nothing
    , _dtgcpsNextToken = Nothing
    , _dtgcpsDryRun = Nothing
    , _dtgcpsMaxResults = Nothing
    }


-- | The IDs of the Connect peers.
dtgcpsTransitGatewayConnectPeerIds :: Lens' DescribeTransitGatewayConnectPeers [Text]
dtgcpsTransitGatewayConnectPeerIds = lens _dtgcpsTransitGatewayConnectPeerIds (\ s a -> s{_dtgcpsTransitGatewayConnectPeerIds = a}) . _Default . _Coerce

-- | One or more filters. The possible values are:     * @state@ - The state of the Connect peer (@pending@ | @available@ | @deleting@ | @deleted@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
dtgcpsFilters :: Lens' DescribeTransitGatewayConnectPeers [Filter]
dtgcpsFilters = lens _dtgcpsFilters (\ s a -> s{_dtgcpsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgcpsNextToken :: Lens' DescribeTransitGatewayConnectPeers (Maybe Text)
dtgcpsNextToken = lens _dtgcpsNextToken (\ s a -> s{_dtgcpsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgcpsDryRun :: Lens' DescribeTransitGatewayConnectPeers (Maybe Bool)
dtgcpsDryRun = lens _dtgcpsDryRun (\ s a -> s{_dtgcpsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgcpsMaxResults :: Lens' DescribeTransitGatewayConnectPeers (Maybe Natural)
dtgcpsMaxResults = lens _dtgcpsMaxResults (\ s a -> s{_dtgcpsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayConnectPeers
         where
        page rq rs
          | stop (rs ^. dtgcprsNextToken) = Nothing
          | stop (rs ^. dtgcprsTransitGatewayConnectPeers) =
            Nothing
          | otherwise =
            Just $ rq & dtgcpsNextToken .~ rs ^. dtgcprsNextToken

instance AWSRequest
           DescribeTransitGatewayConnectPeers
         where
        type Rs DescribeTransitGatewayConnectPeers =
             DescribeTransitGatewayConnectPeersResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeTransitGatewayConnectPeersResponse' <$>
                   (x .@? "transitGatewayConnectPeerSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTransitGatewayConnectPeers
         where

instance NFData DescribeTransitGatewayConnectPeers
         where

instance ToHeaders DescribeTransitGatewayConnectPeers
         where
        toHeaders = const mempty

instance ToPath DescribeTransitGatewayConnectPeers
         where
        toPath = const "/"

instance ToQuery DescribeTransitGatewayConnectPeers
         where
        toQuery DescribeTransitGatewayConnectPeers'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTransitGatewayConnectPeers" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TransitGatewayConnectPeerIds" <$>
                    _dtgcpsTransitGatewayConnectPeerIds),
               toQuery (toQueryList "Filter" <$> _dtgcpsFilters),
               "NextToken" =: _dtgcpsNextToken,
               "DryRun" =: _dtgcpsDryRun,
               "MaxResults" =: _dtgcpsMaxResults]

-- | /See:/ 'describeTransitGatewayConnectPeersResponse' smart constructor.
data DescribeTransitGatewayConnectPeersResponse = DescribeTransitGatewayConnectPeersResponse'
  { _dtgcprsTransitGatewayConnectPeers :: !(Maybe [TransitGatewayConnectPeer])
  , _dtgcprsNextToken                  :: !(Maybe Text)
  , _dtgcprsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayConnectPeersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcprsTransitGatewayConnectPeers' - Information about the Connect peers.
--
-- * 'dtgcprsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgcprsResponseStatus' - -- | The response status code.
describeTransitGatewayConnectPeersResponse
    :: Int -- ^ 'dtgcprsResponseStatus'
    -> DescribeTransitGatewayConnectPeersResponse
describeTransitGatewayConnectPeersResponse pResponseStatus_ =
  DescribeTransitGatewayConnectPeersResponse'
    { _dtgcprsTransitGatewayConnectPeers = Nothing
    , _dtgcprsNextToken = Nothing
    , _dtgcprsResponseStatus = pResponseStatus_
    }


-- | Information about the Connect peers.
dtgcprsTransitGatewayConnectPeers :: Lens' DescribeTransitGatewayConnectPeersResponse [TransitGatewayConnectPeer]
dtgcprsTransitGatewayConnectPeers = lens _dtgcprsTransitGatewayConnectPeers (\ s a -> s{_dtgcprsTransitGatewayConnectPeers = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgcprsNextToken :: Lens' DescribeTransitGatewayConnectPeersResponse (Maybe Text)
dtgcprsNextToken = lens _dtgcprsNextToken (\ s a -> s{_dtgcprsNextToken = a})

-- | -- | The response status code.
dtgcprsResponseStatus :: Lens' DescribeTransitGatewayConnectPeersResponse Int
dtgcprsResponseStatus = lens _dtgcprsResponseStatus (\ s a -> s{_dtgcprsResponseStatus = a})

instance NFData
           DescribeTransitGatewayConnectPeersResponse
         where
