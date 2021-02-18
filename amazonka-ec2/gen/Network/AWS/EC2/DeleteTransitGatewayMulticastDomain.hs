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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
--
--
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
    (
    -- * Creating a Request
      deleteTransitGatewayMulticastDomain
    , DeleteTransitGatewayMulticastDomain
    -- * Request Lenses
    , dtgmdDryRun
    , dtgmdTransitGatewayMulticastDomainId

    -- * Destructuring the Response
    , deleteTransitGatewayMulticastDomainResponse
    , DeleteTransitGatewayMulticastDomainResponse
    -- * Response Lenses
    , dtgmdtrsTransitGatewayMulticastDomain
    , dtgmdtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { _dtgmdDryRun                          :: !(Maybe Bool)
  , _dtgmdTransitGatewayMulticastDomainId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgmdTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
deleteTransitGatewayMulticastDomain
    :: Text -- ^ 'dtgmdTransitGatewayMulticastDomainId'
    -> DeleteTransitGatewayMulticastDomain
deleteTransitGatewayMulticastDomain pTransitGatewayMulticastDomainId_ =
  DeleteTransitGatewayMulticastDomain'
    { _dtgmdDryRun = Nothing
    , _dtgmdTransitGatewayMulticastDomainId = pTransitGatewayMulticastDomainId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgmdDryRun :: Lens' DeleteTransitGatewayMulticastDomain (Maybe Bool)
dtgmdDryRun = lens _dtgmdDryRun (\ s a -> s{_dtgmdDryRun = a})

-- | The ID of the transit gateway multicast domain.
dtgmdTransitGatewayMulticastDomainId :: Lens' DeleteTransitGatewayMulticastDomain Text
dtgmdTransitGatewayMulticastDomainId = lens _dtgmdTransitGatewayMulticastDomainId (\ s a -> s{_dtgmdTransitGatewayMulticastDomainId = a})

instance AWSRequest
           DeleteTransitGatewayMulticastDomain
         where
        type Rs DeleteTransitGatewayMulticastDomain =
             DeleteTransitGatewayMulticastDomainResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayMulticastDomainResponse' <$>
                   (x .@? "transitGatewayMulticastDomain") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTransitGatewayMulticastDomain
         where

instance NFData DeleteTransitGatewayMulticastDomain
         where

instance ToHeaders
           DeleteTransitGatewayMulticastDomain
         where
        toHeaders = const mempty

instance ToPath DeleteTransitGatewayMulticastDomain
         where
        toPath = const "/"

instance ToQuery DeleteTransitGatewayMulticastDomain
         where
        toQuery DeleteTransitGatewayMulticastDomain'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTransitGatewayMulticastDomain" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgmdDryRun,
               "TransitGatewayMulticastDomainId" =:
                 _dtgmdTransitGatewayMulticastDomainId]

-- | /See:/ 'deleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { _dtgmdtrsTransitGatewayMulticastDomain :: !(Maybe TransitGatewayMulticastDomain)
  , _dtgmdtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdtrsTransitGatewayMulticastDomain' - Information about the deleted transit gateway multicast domain.
--
-- * 'dtgmdtrsResponseStatus' - -- | The response status code.
deleteTransitGatewayMulticastDomainResponse
    :: Int -- ^ 'dtgmdtrsResponseStatus'
    -> DeleteTransitGatewayMulticastDomainResponse
deleteTransitGatewayMulticastDomainResponse pResponseStatus_ =
  DeleteTransitGatewayMulticastDomainResponse'
    { _dtgmdtrsTransitGatewayMulticastDomain = Nothing
    , _dtgmdtrsResponseStatus = pResponseStatus_
    }


-- | Information about the deleted transit gateway multicast domain.
dtgmdtrsTransitGatewayMulticastDomain :: Lens' DeleteTransitGatewayMulticastDomainResponse (Maybe TransitGatewayMulticastDomain)
dtgmdtrsTransitGatewayMulticastDomain = lens _dtgmdtrsTransitGatewayMulticastDomain (\ s a -> s{_dtgmdtrsTransitGatewayMulticastDomain = a})

-- | -- | The response status code.
dtgmdtrsResponseStatus :: Lens' DeleteTransitGatewayMulticastDomainResponse Int
dtgmdtrsResponseStatus = lens _dtgmdtrsResponseStatus (\ s a -> s{_dtgmdtrsResponseStatus = a})

instance NFData
           DeleteTransitGatewayMulticastDomainResponse
         where
