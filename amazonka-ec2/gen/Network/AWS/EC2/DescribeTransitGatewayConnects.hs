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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayConnects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect attachments.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayConnects
    (
    -- * Creating a Request
      describeTransitGatewayConnects
    , DescribeTransitGatewayConnects
    -- * Request Lenses
    , dtgcsFilters
    , dtgcsNextToken
    , dtgcsTransitGatewayAttachmentIds
    , dtgcsDryRun
    , dtgcsMaxResults

    -- * Destructuring the Response
    , describeTransitGatewayConnectsResponse
    , DescribeTransitGatewayConnectsResponse
    -- * Response Lenses
    , dtgcsrsTransitGatewayConnects
    , dtgcsrsNextToken
    , dtgcsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayConnects' smart constructor.
data DescribeTransitGatewayConnects = DescribeTransitGatewayConnects'
  { _dtgcsFilters                     :: !(Maybe [Filter])
  , _dtgcsNextToken                   :: !(Maybe Text)
  , _dtgcsTransitGatewayAttachmentIds :: !(Maybe [Text])
  , _dtgcsDryRun                      :: !(Maybe Bool)
  , _dtgcsMaxResults                  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayConnects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcsFilters' - One or more filters. The possible values are:     * @options.protocol@ - The tunnel protocol (@gre@ ).     * @state@ - The state of the attachment (@initiating@ | @initiatingRequest@ | @pendingAcceptance@ | @rollingBack@ | @pending@ | @available@ | @modifying@ | @deleting@ | @deleted@ | @failed@ | @rejected@ | @rejecting@ | @failing@ ).     * @transit-gateway-attachment-id@ - The ID of the Connect attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @transport-transit-gateway-attachment-id@ - The ID of the transit gateway attachment from which the Connect attachment was created.
--
-- * 'dtgcsNextToken' - The token for the next page of results.
--
-- * 'dtgcsTransitGatewayAttachmentIds' - The IDs of the attachments.
--
-- * 'dtgcsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgcsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayConnects
    :: DescribeTransitGatewayConnects
describeTransitGatewayConnects =
  DescribeTransitGatewayConnects'
    { _dtgcsFilters = Nothing
    , _dtgcsNextToken = Nothing
    , _dtgcsTransitGatewayAttachmentIds = Nothing
    , _dtgcsDryRun = Nothing
    , _dtgcsMaxResults = Nothing
    }


-- | One or more filters. The possible values are:     * @options.protocol@ - The tunnel protocol (@gre@ ).     * @state@ - The state of the attachment (@initiating@ | @initiatingRequest@ | @pendingAcceptance@ | @rollingBack@ | @pending@ | @available@ | @modifying@ | @deleting@ | @deleted@ | @failed@ | @rejected@ | @rejecting@ | @failing@ ).     * @transit-gateway-attachment-id@ - The ID of the Connect attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @transport-transit-gateway-attachment-id@ - The ID of the transit gateway attachment from which the Connect attachment was created.
dtgcsFilters :: Lens' DescribeTransitGatewayConnects [Filter]
dtgcsFilters = lens _dtgcsFilters (\ s a -> s{_dtgcsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgcsNextToken :: Lens' DescribeTransitGatewayConnects (Maybe Text)
dtgcsNextToken = lens _dtgcsNextToken (\ s a -> s{_dtgcsNextToken = a})

-- | The IDs of the attachments.
dtgcsTransitGatewayAttachmentIds :: Lens' DescribeTransitGatewayConnects [Text]
dtgcsTransitGatewayAttachmentIds = lens _dtgcsTransitGatewayAttachmentIds (\ s a -> s{_dtgcsTransitGatewayAttachmentIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgcsDryRun :: Lens' DescribeTransitGatewayConnects (Maybe Bool)
dtgcsDryRun = lens _dtgcsDryRun (\ s a -> s{_dtgcsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgcsMaxResults :: Lens' DescribeTransitGatewayConnects (Maybe Natural)
dtgcsMaxResults = lens _dtgcsMaxResults (\ s a -> s{_dtgcsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayConnects
         where
        page rq rs
          | stop (rs ^. dtgcsrsNextToken) = Nothing
          | stop (rs ^. dtgcsrsTransitGatewayConnects) =
            Nothing
          | otherwise =
            Just $ rq & dtgcsNextToken .~ rs ^. dtgcsrsNextToken

instance AWSRequest DescribeTransitGatewayConnects
         where
        type Rs DescribeTransitGatewayConnects =
             DescribeTransitGatewayConnectsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeTransitGatewayConnectsResponse' <$>
                   (x .@? "transitGatewayConnectSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTransitGatewayConnects
         where

instance NFData DescribeTransitGatewayConnects where

instance ToHeaders DescribeTransitGatewayConnects
         where
        toHeaders = const mempty

instance ToPath DescribeTransitGatewayConnects where
        toPath = const "/"

instance ToQuery DescribeTransitGatewayConnects where
        toQuery DescribeTransitGatewayConnects'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTransitGatewayConnects" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dtgcsFilters),
               "NextToken" =: _dtgcsNextToken,
               toQuery
                 (toQueryList "TransitGatewayAttachmentIds" <$>
                    _dtgcsTransitGatewayAttachmentIds),
               "DryRun" =: _dtgcsDryRun,
               "MaxResults" =: _dtgcsMaxResults]

-- | /See:/ 'describeTransitGatewayConnectsResponse' smart constructor.
data DescribeTransitGatewayConnectsResponse = DescribeTransitGatewayConnectsResponse'
  { _dtgcsrsTransitGatewayConnects :: !(Maybe [TransitGatewayConnect])
  , _dtgcsrsNextToken              :: !(Maybe Text)
  , _dtgcsrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayConnectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcsrsTransitGatewayConnects' - Information about the Connect attachments.
--
-- * 'dtgcsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgcsrsResponseStatus' - -- | The response status code.
describeTransitGatewayConnectsResponse
    :: Int -- ^ 'dtgcsrsResponseStatus'
    -> DescribeTransitGatewayConnectsResponse
describeTransitGatewayConnectsResponse pResponseStatus_ =
  DescribeTransitGatewayConnectsResponse'
    { _dtgcsrsTransitGatewayConnects = Nothing
    , _dtgcsrsNextToken = Nothing
    , _dtgcsrsResponseStatus = pResponseStatus_
    }


-- | Information about the Connect attachments.
dtgcsrsTransitGatewayConnects :: Lens' DescribeTransitGatewayConnectsResponse [TransitGatewayConnect]
dtgcsrsTransitGatewayConnects = lens _dtgcsrsTransitGatewayConnects (\ s a -> s{_dtgcsrsTransitGatewayConnects = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgcsrsNextToken :: Lens' DescribeTransitGatewayConnectsResponse (Maybe Text)
dtgcsrsNextToken = lens _dtgcsrsNextToken (\ s a -> s{_dtgcsrsNextToken = a})

-- | -- | The response status code.
dtgcsrsResponseStatus :: Lens' DescribeTransitGatewayConnectsResponse Int
dtgcsrsResponseStatus = lens _dtgcsrsResponseStatus (\ s a -> s{_dtgcsrsResponseStatus = a})

instance NFData
           DescribeTransitGatewayConnectsResponse
         where
