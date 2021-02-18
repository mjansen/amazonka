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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect peer.
--
--
module Network.AWS.EC2.DeleteTransitGatewayConnectPeer
    (
    -- * Creating a Request
      deleteTransitGatewayConnectPeer
    , DeleteTransitGatewayConnectPeer
    -- * Request Lenses
    , dtgcpDryRun
    , dtgcpTransitGatewayConnectPeerId

    -- * Destructuring the Response
    , deleteTransitGatewayConnectPeerResponse
    , DeleteTransitGatewayConnectPeerResponse
    -- * Response Lenses
    , delrsTransitGatewayConnectPeer
    , delrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayConnectPeer' smart constructor.
data DeleteTransitGatewayConnectPeer = DeleteTransitGatewayConnectPeer'
  { _dtgcpDryRun                      :: !(Maybe Bool)
  , _dtgcpTransitGatewayConnectPeerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayConnectPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgcpTransitGatewayConnectPeerId' - The ID of the Connect peer.
deleteTransitGatewayConnectPeer
    :: Text -- ^ 'dtgcpTransitGatewayConnectPeerId'
    -> DeleteTransitGatewayConnectPeer
deleteTransitGatewayConnectPeer pTransitGatewayConnectPeerId_ =
  DeleteTransitGatewayConnectPeer'
    { _dtgcpDryRun = Nothing
    , _dtgcpTransitGatewayConnectPeerId = pTransitGatewayConnectPeerId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgcpDryRun :: Lens' DeleteTransitGatewayConnectPeer (Maybe Bool)
dtgcpDryRun = lens _dtgcpDryRun (\ s a -> s{_dtgcpDryRun = a})

-- | The ID of the Connect peer.
dtgcpTransitGatewayConnectPeerId :: Lens' DeleteTransitGatewayConnectPeer Text
dtgcpTransitGatewayConnectPeerId = lens _dtgcpTransitGatewayConnectPeerId (\ s a -> s{_dtgcpTransitGatewayConnectPeerId = a})

instance AWSRequest DeleteTransitGatewayConnectPeer
         where
        type Rs DeleteTransitGatewayConnectPeer =
             DeleteTransitGatewayConnectPeerResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayConnectPeerResponse' <$>
                   (x .@? "transitGatewayConnectPeer") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTransitGatewayConnectPeer
         where

instance NFData DeleteTransitGatewayConnectPeer where

instance ToHeaders DeleteTransitGatewayConnectPeer
         where
        toHeaders = const mempty

instance ToPath DeleteTransitGatewayConnectPeer where
        toPath = const "/"

instance ToQuery DeleteTransitGatewayConnectPeer
         where
        toQuery DeleteTransitGatewayConnectPeer'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTransitGatewayConnectPeer" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgcpDryRun,
               "TransitGatewayConnectPeerId" =:
                 _dtgcpTransitGatewayConnectPeerId]

-- | /See:/ 'deleteTransitGatewayConnectPeerResponse' smart constructor.
data DeleteTransitGatewayConnectPeerResponse = DeleteTransitGatewayConnectPeerResponse'
  { _delrsTransitGatewayConnectPeer :: !(Maybe TransitGatewayConnectPeer)
  , _delrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayConnectPeerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsTransitGatewayConnectPeer' - Information about the deleted Connect peer.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteTransitGatewayConnectPeerResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteTransitGatewayConnectPeerResponse
deleteTransitGatewayConnectPeerResponse pResponseStatus_ =
  DeleteTransitGatewayConnectPeerResponse'
    { _delrsTransitGatewayConnectPeer = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }


-- | Information about the deleted Connect peer.
delrsTransitGatewayConnectPeer :: Lens' DeleteTransitGatewayConnectPeerResponse (Maybe TransitGatewayConnectPeer)
delrsTransitGatewayConnectPeer = lens _delrsTransitGatewayConnectPeer (\ s a -> s{_delrsTransitGatewayConnectPeer = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteTransitGatewayConnectPeerResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData
           DeleteTransitGatewayConnectPeerResponse
         where
