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
-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route table, you can add routes and associate the table with a subnet.
--
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateRouteTable
    (
    -- * Creating a Request
      createRouteTable
    , CreateRouteTable
    -- * Request Lenses
    , crtTagSpecifications
    , crtDryRun
    , crtVPCId

    -- * Destructuring the Response
    , createRouteTableResponse
    , CreateRouteTableResponse
    -- * Response Lenses
    , crtrsRouteTable
    , crtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
  { _crtTagSpecifications :: !(Maybe [TagSpecification])
  , _crtDryRun            :: !(Maybe Bool)
  , _crtVPCId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtTagSpecifications' - The tags to assign to the route table.
--
-- * 'crtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'crtVPCId' - The ID of the VPC.
createRouteTable
    :: Text -- ^ 'crtVPCId'
    -> CreateRouteTable
createRouteTable pVPCId_ =
  CreateRouteTable'
    {_crtTagSpecifications = Nothing, _crtDryRun = Nothing, _crtVPCId = pVPCId_}


-- | The tags to assign to the route table.
crtTagSpecifications :: Lens' CreateRouteTable [TagSpecification]
crtTagSpecifications = lens _crtTagSpecifications (\ s a -> s{_crtTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
crtDryRun :: Lens' CreateRouteTable (Maybe Bool)
crtDryRun = lens _crtDryRun (\ s a -> s{_crtDryRun = a})

-- | The ID of the VPC.
crtVPCId :: Lens' CreateRouteTable Text
crtVPCId = lens _crtVPCId (\ s a -> s{_crtVPCId = a})

instance AWSRequest CreateRouteTable where
        type Rs CreateRouteTable = CreateRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateRouteTableResponse' <$>
                   (x .@? "routeTable") <*> (pure (fromEnum s)))

instance Hashable CreateRouteTable where

instance NFData CreateRouteTable where

instance ToHeaders CreateRouteTable where
        toHeaders = const mempty

instance ToPath CreateRouteTable where
        toPath = const "/"

instance ToQuery CreateRouteTable where
        toQuery CreateRouteTable'{..}
          = mconcat
              ["Action" =: ("CreateRouteTable" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _crtTagSpecifications),
               "DryRun" =: _crtDryRun, "VpcId" =: _crtVPCId]

-- | /See:/ 'createRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
  { _crtrsRouteTable     :: !(Maybe RouteTable)
  , _crtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtrsRouteTable' - Information about the route table.
--
-- * 'crtrsResponseStatus' - -- | The response status code.
createRouteTableResponse
    :: Int -- ^ 'crtrsResponseStatus'
    -> CreateRouteTableResponse
createRouteTableResponse pResponseStatus_ =
  CreateRouteTableResponse'
    {_crtrsRouteTable = Nothing, _crtrsResponseStatus = pResponseStatus_}


-- | Information about the route table.
crtrsRouteTable :: Lens' CreateRouteTableResponse (Maybe RouteTable)
crtrsRouteTable = lens _crtrsRouteTable (\ s a -> s{_crtrsRouteTable = a})

-- | -- | The response status code.
crtrsResponseStatus :: Lens' CreateRouteTableResponse Int
crtrsResponseStatus = lens _crtrsResponseStatus (\ s a -> s{_crtrsResponseStatus = a})

instance NFData CreateRouteTableResponse where
