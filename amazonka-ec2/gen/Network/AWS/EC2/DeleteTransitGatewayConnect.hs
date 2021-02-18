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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayConnect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect attachment. You must first delete any Connect peers for the attachment.
--
--
module Network.AWS.EC2.DeleteTransitGatewayConnect
    (
    -- * Creating a Request
      deleteTransitGatewayConnect
    , DeleteTransitGatewayConnect
    -- * Request Lenses
    , dtgcDryRun
    , dtgcTransitGatewayAttachmentId

    -- * Destructuring the Response
    , deleteTransitGatewayConnectResponse
    , DeleteTransitGatewayConnectResponse
    -- * Response Lenses
    , dtgcrsTransitGatewayConnect
    , dtgcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayConnect' smart constructor.
data DeleteTransitGatewayConnect = DeleteTransitGatewayConnect'
  { _dtgcDryRun                     :: !(Maybe Bool)
  , _dtgcTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayConnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgcTransitGatewayAttachmentId' - The ID of the Connect attachment.
deleteTransitGatewayConnect
    :: Text -- ^ 'dtgcTransitGatewayAttachmentId'
    -> DeleteTransitGatewayConnect
deleteTransitGatewayConnect pTransitGatewayAttachmentId_ =
  DeleteTransitGatewayConnect'
    { _dtgcDryRun = Nothing
    , _dtgcTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgcDryRun :: Lens' DeleteTransitGatewayConnect (Maybe Bool)
dtgcDryRun = lens _dtgcDryRun (\ s a -> s{_dtgcDryRun = a})

-- | The ID of the Connect attachment.
dtgcTransitGatewayAttachmentId :: Lens' DeleteTransitGatewayConnect Text
dtgcTransitGatewayAttachmentId = lens _dtgcTransitGatewayAttachmentId (\ s a -> s{_dtgcTransitGatewayAttachmentId = a})

instance AWSRequest DeleteTransitGatewayConnect where
        type Rs DeleteTransitGatewayConnect =
             DeleteTransitGatewayConnectResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayConnectResponse' <$>
                   (x .@? "transitGatewayConnect") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTransitGatewayConnect where

instance NFData DeleteTransitGatewayConnect where

instance ToHeaders DeleteTransitGatewayConnect where
        toHeaders = const mempty

instance ToPath DeleteTransitGatewayConnect where
        toPath = const "/"

instance ToQuery DeleteTransitGatewayConnect where
        toQuery DeleteTransitGatewayConnect'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTransitGatewayConnect" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgcDryRun,
               "TransitGatewayAttachmentId" =:
                 _dtgcTransitGatewayAttachmentId]

-- | /See:/ 'deleteTransitGatewayConnectResponse' smart constructor.
data DeleteTransitGatewayConnectResponse = DeleteTransitGatewayConnectResponse'
  { _dtgcrsTransitGatewayConnect :: !(Maybe TransitGatewayConnect)
  , _dtgcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayConnectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgcrsTransitGatewayConnect' - Information about the deleted Connect attachment.
--
-- * 'dtgcrsResponseStatus' - -- | The response status code.
deleteTransitGatewayConnectResponse
    :: Int -- ^ 'dtgcrsResponseStatus'
    -> DeleteTransitGatewayConnectResponse
deleteTransitGatewayConnectResponse pResponseStatus_ =
  DeleteTransitGatewayConnectResponse'
    { _dtgcrsTransitGatewayConnect = Nothing
    , _dtgcrsResponseStatus = pResponseStatus_
    }


-- | Information about the deleted Connect attachment.
dtgcrsTransitGatewayConnect :: Lens' DeleteTransitGatewayConnectResponse (Maybe TransitGatewayConnect)
dtgcrsTransitGatewayConnect = lens _dtgcrsTransitGatewayConnect (\ s a -> s{_dtgcrsTransitGatewayConnect = a})

-- | -- | The response status code.
dtgcrsResponseStatus :: Lens' DeleteTransitGatewayConnectResponse Int
dtgcrsResponseStatus = lens _dtgcrsResponseStatus (\ s a -> s{_dtgcrsResponseStatus = a})

instance NFData DeleteTransitGatewayConnectResponse
         where
