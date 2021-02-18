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
-- Module      : Network.AWS.EC2.DescribeAddressesAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes of the specified Elastic IP addresses. For requirements, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html#Using_Elastic_Addressing_Reverse_DNS Using reverse DNS for email applications> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeAddressesAttribute
    (
    -- * Creating a Request
      describeAddressesAttribute
    , DescribeAddressesAttribute
    -- * Request Lenses
    , daaaAttribute
    , daaaNextToken
    , daaaAllocationIds
    , daaaDryRun
    , daaaMaxResults

    -- * Destructuring the Response
    , describeAddressesAttributeResponse
    , DescribeAddressesAttributeResponse
    -- * Response Lenses
    , daarsAddresses
    , daarsNextToken
    , daarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAddressesAttribute' smart constructor.
data DescribeAddressesAttribute = DescribeAddressesAttribute'
  { _daaaAttribute     :: !(Maybe AddressAttributeName)
  , _daaaNextToken     :: !(Maybe Text)
  , _daaaAllocationIds :: !(Maybe [Text])
  , _daaaDryRun        :: !(Maybe Bool)
  , _daaaMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddressesAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaaAttribute' - The attribute of the IP address.
--
-- * 'daaaNextToken' - The token for the next page of results.
--
-- * 'daaaAllocationIds' - [EC2-VPC] The allocation IDs.
--
-- * 'daaaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'daaaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeAddressesAttribute
    :: DescribeAddressesAttribute
describeAddressesAttribute =
  DescribeAddressesAttribute'
    { _daaaAttribute = Nothing
    , _daaaNextToken = Nothing
    , _daaaAllocationIds = Nothing
    , _daaaDryRun = Nothing
    , _daaaMaxResults = Nothing
    }


-- | The attribute of the IP address.
daaaAttribute :: Lens' DescribeAddressesAttribute (Maybe AddressAttributeName)
daaaAttribute = lens _daaaAttribute (\ s a -> s{_daaaAttribute = a})

-- | The token for the next page of results.
daaaNextToken :: Lens' DescribeAddressesAttribute (Maybe Text)
daaaNextToken = lens _daaaNextToken (\ s a -> s{_daaaNextToken = a})

-- | [EC2-VPC] The allocation IDs.
daaaAllocationIds :: Lens' DescribeAddressesAttribute [Text]
daaaAllocationIds = lens _daaaAllocationIds (\ s a -> s{_daaaAllocationIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
daaaDryRun :: Lens' DescribeAddressesAttribute (Maybe Bool)
daaaDryRun = lens _daaaDryRun (\ s a -> s{_daaaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
daaaMaxResults :: Lens' DescribeAddressesAttribute (Maybe Natural)
daaaMaxResults = lens _daaaMaxResults (\ s a -> s{_daaaMaxResults = a}) . mapping _Nat

instance AWSPager DescribeAddressesAttribute where
        page rq rs
          | stop (rs ^. daarsNextToken) = Nothing
          | stop (rs ^. daarsAddresses) = Nothing
          | otherwise =
            Just $ rq & daaaNextToken .~ rs ^. daarsNextToken

instance AWSRequest DescribeAddressesAttribute where
        type Rs DescribeAddressesAttribute =
             DescribeAddressesAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeAddressesAttributeResponse' <$>
                   (x .@? "addressSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAddressesAttribute where

instance NFData DescribeAddressesAttribute where

instance ToHeaders DescribeAddressesAttribute where
        toHeaders = const mempty

instance ToPath DescribeAddressesAttribute where
        toPath = const "/"

instance ToQuery DescribeAddressesAttribute where
        toQuery DescribeAddressesAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAddressesAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _daaaAttribute,
               "NextToken" =: _daaaNextToken,
               toQuery
                 (toQueryList "AllocationId" <$> _daaaAllocationIds),
               "DryRun" =: _daaaDryRun,
               "MaxResults" =: _daaaMaxResults]

-- | /See:/ 'describeAddressesAttributeResponse' smart constructor.
data DescribeAddressesAttributeResponse = DescribeAddressesAttributeResponse'
  { _daarsAddresses      :: !(Maybe [AddressAttribute])
  , _daarsNextToken      :: !(Maybe Text)
  , _daarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddressesAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAddresses' - Information about the IP addresses.
--
-- * 'daarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAddressesAttributeResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAddressesAttributeResponse
describeAddressesAttributeResponse pResponseStatus_ =
  DescribeAddressesAttributeResponse'
    { _daarsAddresses = Nothing
    , _daarsNextToken = Nothing
    , _daarsResponseStatus = pResponseStatus_
    }


-- | Information about the IP addresses.
daarsAddresses :: Lens' DescribeAddressesAttributeResponse [AddressAttribute]
daarsAddresses = lens _daarsAddresses (\ s a -> s{_daarsAddresses = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
daarsNextToken :: Lens' DescribeAddressesAttributeResponse (Maybe Text)
daarsNextToken = lens _daarsNextToken (\ s a -> s{_daarsNextToken = a})

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAddressesAttributeResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a})

instance NFData DescribeAddressesAttributeResponse
         where
