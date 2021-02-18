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
-- Module      : Network.AWS.EC2.CreateTransitGatewayConnect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Connect attachment from a specified transit gateway attachment. A Connect attachment is a GRE-based tunnel attachment that you can use to establish a connection between a transit gateway and an appliance.
--
--
-- A Connect attachment uses an existing VPC or AWS Direct Connect attachment as the underlying transport mechanism.
--
module Network.AWS.EC2.CreateTransitGatewayConnect
    (
    -- * Creating a Request
      createTransitGatewayConnect
    , CreateTransitGatewayConnect
    -- * Request Lenses
    , ctgcTagSpecifications
    , ctgcDryRun
    , ctgcTransportTransitGatewayAttachmentId
    , ctgcOptions

    -- * Destructuring the Response
    , createTransitGatewayConnectResponse
    , CreateTransitGatewayConnectResponse
    -- * Response Lenses
    , ctgcrsTransitGatewayConnect
    , ctgcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayConnect' smart constructor.
data CreateTransitGatewayConnect = CreateTransitGatewayConnect'
  { _ctgcTagSpecifications :: !(Maybe [TagSpecification])
  , _ctgcDryRun :: !(Maybe Bool)
  , _ctgcTransportTransitGatewayAttachmentId :: !Text
  , _ctgcOptions :: !CreateTransitGatewayConnectRequestOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayConnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgcTagSpecifications' - The tags to apply to the Connect attachment.
--
-- * 'ctgcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgcTransportTransitGatewayAttachmentId' - The ID of the transit gateway attachment. You can specify a VPC attachment or a AWS Direct Connect attachment.
--
-- * 'ctgcOptions' - The Connect attachment options.
createTransitGatewayConnect
    :: Text -- ^ 'ctgcTransportTransitGatewayAttachmentId'
    -> CreateTransitGatewayConnectRequestOptions -- ^ 'ctgcOptions'
    -> CreateTransitGatewayConnect
createTransitGatewayConnect pTransportTransitGatewayAttachmentId_ pOptions_ =
  CreateTransitGatewayConnect'
    { _ctgcTagSpecifications = Nothing
    , _ctgcDryRun = Nothing
    , _ctgcTransportTransitGatewayAttachmentId =
        pTransportTransitGatewayAttachmentId_
    , _ctgcOptions = pOptions_
    }


-- | The tags to apply to the Connect attachment.
ctgcTagSpecifications :: Lens' CreateTransitGatewayConnect [TagSpecification]
ctgcTagSpecifications = lens _ctgcTagSpecifications (\ s a -> s{_ctgcTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgcDryRun :: Lens' CreateTransitGatewayConnect (Maybe Bool)
ctgcDryRun = lens _ctgcDryRun (\ s a -> s{_ctgcDryRun = a})

-- | The ID of the transit gateway attachment. You can specify a VPC attachment or a AWS Direct Connect attachment.
ctgcTransportTransitGatewayAttachmentId :: Lens' CreateTransitGatewayConnect Text
ctgcTransportTransitGatewayAttachmentId = lens _ctgcTransportTransitGatewayAttachmentId (\ s a -> s{_ctgcTransportTransitGatewayAttachmentId = a})

-- | The Connect attachment options.
ctgcOptions :: Lens' CreateTransitGatewayConnect CreateTransitGatewayConnectRequestOptions
ctgcOptions = lens _ctgcOptions (\ s a -> s{_ctgcOptions = a})

instance AWSRequest CreateTransitGatewayConnect where
        type Rs CreateTransitGatewayConnect =
             CreateTransitGatewayConnectResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateTransitGatewayConnectResponse' <$>
                   (x .@? "transitGatewayConnect") <*>
                     (pure (fromEnum s)))

instance Hashable CreateTransitGatewayConnect where

instance NFData CreateTransitGatewayConnect where

instance ToHeaders CreateTransitGatewayConnect where
        toHeaders = const mempty

instance ToPath CreateTransitGatewayConnect where
        toPath = const "/"

instance ToQuery CreateTransitGatewayConnect where
        toQuery CreateTransitGatewayConnect'{..}
          = mconcat
              ["Action" =:
                 ("CreateTransitGatewayConnect" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _ctgcTagSpecifications),
               "DryRun" =: _ctgcDryRun,
               "TransportTransitGatewayAttachmentId" =:
                 _ctgcTransportTransitGatewayAttachmentId,
               "Options" =: _ctgcOptions]

-- | /See:/ 'createTransitGatewayConnectResponse' smart constructor.
data CreateTransitGatewayConnectResponse = CreateTransitGatewayConnectResponse'
  { _ctgcrsTransitGatewayConnect :: !(Maybe TransitGatewayConnect)
  , _ctgcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayConnectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgcrsTransitGatewayConnect' - Information about the Connect attachment.
--
-- * 'ctgcrsResponseStatus' - -- | The response status code.
createTransitGatewayConnectResponse
    :: Int -- ^ 'ctgcrsResponseStatus'
    -> CreateTransitGatewayConnectResponse
createTransitGatewayConnectResponse pResponseStatus_ =
  CreateTransitGatewayConnectResponse'
    { _ctgcrsTransitGatewayConnect = Nothing
    , _ctgcrsResponseStatus = pResponseStatus_
    }


-- | Information about the Connect attachment.
ctgcrsTransitGatewayConnect :: Lens' CreateTransitGatewayConnectResponse (Maybe TransitGatewayConnect)
ctgcrsTransitGatewayConnect = lens _ctgcrsTransitGatewayConnect (\ s a -> s{_ctgcrsTransitGatewayConnect = a})

-- | -- | The response status code.
ctgcrsResponseStatus :: Lens' CreateTransitGatewayConnectResponse Int
ctgcrsResponseStatus = lens _ctgcrsResponseStatus (\ s a -> s{_ctgcrsResponseStatus = a})

instance NFData CreateTransitGatewayConnectResponse
         where
