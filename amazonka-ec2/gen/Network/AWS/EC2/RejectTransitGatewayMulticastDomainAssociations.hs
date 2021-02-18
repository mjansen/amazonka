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
-- Module      : Network.AWS.EC2.RejectTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to associate cross-account subnets with a transit gateway multicast domain.
--
--
module Network.AWS.EC2.RejectTransitGatewayMulticastDomainAssociations
    (
    -- * Creating a Request
      rejectTransitGatewayMulticastDomainAssociations
    , RejectTransitGatewayMulticastDomainAssociations
    -- * Request Lenses
    , rtgmdaSubnetIds
    , rtgmdaTransitGatewayMulticastDomainId
    , rtgmdaTransitGatewayAttachmentId
    , rtgmdaDryRun

    -- * Destructuring the Response
    , rejectTransitGatewayMulticastDomainAssociationsResponse
    , RejectTransitGatewayMulticastDomainAssociationsResponse
    -- * Response Lenses
    , rtgmdarsAssociations
    , rtgmdarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rejectTransitGatewayMulticastDomainAssociations' smart constructor.
data RejectTransitGatewayMulticastDomainAssociations = RejectTransitGatewayMulticastDomainAssociations'
  { _rtgmdaSubnetIds                       :: !(Maybe [Text])
  , _rtgmdaTransitGatewayMulticastDomainId :: !(Maybe Text)
  , _rtgmdaTransitGatewayAttachmentId      :: !(Maybe Text)
  , _rtgmdaDryRun                          :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectTransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgmdaSubnetIds' - The IDs of the subnets to associate with the transit gateway multicast domain.
--
-- * 'rtgmdaTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'rtgmdaTransitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- * 'rtgmdaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rejectTransitGatewayMulticastDomainAssociations
    :: RejectTransitGatewayMulticastDomainAssociations
rejectTransitGatewayMulticastDomainAssociations =
  RejectTransitGatewayMulticastDomainAssociations'
    { _rtgmdaSubnetIds = Nothing
    , _rtgmdaTransitGatewayMulticastDomainId = Nothing
    , _rtgmdaTransitGatewayAttachmentId = Nothing
    , _rtgmdaDryRun = Nothing
    }


-- | The IDs of the subnets to associate with the transit gateway multicast domain.
rtgmdaSubnetIds :: Lens' RejectTransitGatewayMulticastDomainAssociations [Text]
rtgmdaSubnetIds = lens _rtgmdaSubnetIds (\ s a -> s{_rtgmdaSubnetIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
rtgmdaTransitGatewayMulticastDomainId :: Lens' RejectTransitGatewayMulticastDomainAssociations (Maybe Text)
rtgmdaTransitGatewayMulticastDomainId = lens _rtgmdaTransitGatewayMulticastDomainId (\ s a -> s{_rtgmdaTransitGatewayMulticastDomainId = a})

-- | The ID of the transit gateway attachment.
rtgmdaTransitGatewayAttachmentId :: Lens' RejectTransitGatewayMulticastDomainAssociations (Maybe Text)
rtgmdaTransitGatewayAttachmentId = lens _rtgmdaTransitGatewayAttachmentId (\ s a -> s{_rtgmdaTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rtgmdaDryRun :: Lens' RejectTransitGatewayMulticastDomainAssociations (Maybe Bool)
rtgmdaDryRun = lens _rtgmdaDryRun (\ s a -> s{_rtgmdaDryRun = a})

instance AWSRequest
           RejectTransitGatewayMulticastDomainAssociations
         where
        type Rs
               RejectTransitGatewayMulticastDomainAssociations
             =
             RejectTransitGatewayMulticastDomainAssociationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RejectTransitGatewayMulticastDomainAssociationsResponse'
                   <$> (x .@? "associations") <*> (pure (fromEnum s)))

instance Hashable
           RejectTransitGatewayMulticastDomainAssociations
         where

instance NFData
           RejectTransitGatewayMulticastDomainAssociations
         where

instance ToHeaders
           RejectTransitGatewayMulticastDomainAssociations
         where
        toHeaders = const mempty

instance ToPath
           RejectTransitGatewayMulticastDomainAssociations
         where
        toPath = const "/"

instance ToQuery
           RejectTransitGatewayMulticastDomainAssociations
         where
        toQuery
          RejectTransitGatewayMulticastDomainAssociations'{..}
          = mconcat
              ["Action" =:
                 ("RejectTransitGatewayMulticastDomainAssociations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SubnetIds" <$> _rtgmdaSubnetIds),
               "TransitGatewayMulticastDomainId" =:
                 _rtgmdaTransitGatewayMulticastDomainId,
               "TransitGatewayAttachmentId" =:
                 _rtgmdaTransitGatewayAttachmentId,
               "DryRun" =: _rtgmdaDryRun]

-- | /See:/ 'rejectTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data RejectTransitGatewayMulticastDomainAssociationsResponse = RejectTransitGatewayMulticastDomainAssociationsResponse'
  { _rtgmdarsAssociations :: !(Maybe TransitGatewayMulticastDomainAssociations)
  , _rtgmdarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectTransitGatewayMulticastDomainAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgmdarsAssociations' - Undocumented member.
--
-- * 'rtgmdarsResponseStatus' - -- | The response status code.
rejectTransitGatewayMulticastDomainAssociationsResponse
    :: Int -- ^ 'rtgmdarsResponseStatus'
    -> RejectTransitGatewayMulticastDomainAssociationsResponse
rejectTransitGatewayMulticastDomainAssociationsResponse pResponseStatus_ =
  RejectTransitGatewayMulticastDomainAssociationsResponse'
    { _rtgmdarsAssociations = Nothing
    , _rtgmdarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rtgmdarsAssociations :: Lens' RejectTransitGatewayMulticastDomainAssociationsResponse (Maybe TransitGatewayMulticastDomainAssociations)
rtgmdarsAssociations = lens _rtgmdarsAssociations (\ s a -> s{_rtgmdarsAssociations = a})

-- | -- | The response status code.
rtgmdarsResponseStatus :: Lens' RejectTransitGatewayMulticastDomainAssociationsResponse Int
rtgmdarsResponseStatus = lens _rtgmdarsResponseStatus (\ s a -> s{_rtgmdarsResponseStatus = a})

instance NFData
           RejectTransitGatewayMulticastDomainAssociationsResponse
         where
