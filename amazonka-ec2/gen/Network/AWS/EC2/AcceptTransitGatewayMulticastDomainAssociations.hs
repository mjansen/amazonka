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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to associate subnets with a transit gateway multicast domain.
--
--
module Network.AWS.EC2.AcceptTransitGatewayMulticastDomainAssociations
    (
    -- * Creating a Request
      acceptTransitGatewayMulticastDomainAssociations
    , AcceptTransitGatewayMulticastDomainAssociations
    -- * Request Lenses
    , atgmdaSubnetIds
    , atgmdaTransitGatewayMulticastDomainId
    , atgmdaTransitGatewayAttachmentId
    , atgmdaDryRun

    -- * Destructuring the Response
    , acceptTransitGatewayMulticastDomainAssociationsResponse
    , AcceptTransitGatewayMulticastDomainAssociationsResponse
    -- * Response Lenses
    , atgmdarsAssociations
    , atgmdarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptTransitGatewayMulticastDomainAssociations' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociations = AcceptTransitGatewayMulticastDomainAssociations'
  { _atgmdaSubnetIds                       :: !(Maybe [Text])
  , _atgmdaTransitGatewayMulticastDomainId :: !(Maybe Text)
  , _atgmdaTransitGatewayAttachmentId      :: !(Maybe Text)
  , _atgmdaDryRun                          :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptTransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgmdaSubnetIds' - The IDs of the subnets to associate with the transit gateway multicast domain.
--
-- * 'atgmdaTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'atgmdaTransitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- * 'atgmdaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
acceptTransitGatewayMulticastDomainAssociations
    :: AcceptTransitGatewayMulticastDomainAssociations
acceptTransitGatewayMulticastDomainAssociations =
  AcceptTransitGatewayMulticastDomainAssociations'
    { _atgmdaSubnetIds = Nothing
    , _atgmdaTransitGatewayMulticastDomainId = Nothing
    , _atgmdaTransitGatewayAttachmentId = Nothing
    , _atgmdaDryRun = Nothing
    }


-- | The IDs of the subnets to associate with the transit gateway multicast domain.
atgmdaSubnetIds :: Lens' AcceptTransitGatewayMulticastDomainAssociations [Text]
atgmdaSubnetIds = lens _atgmdaSubnetIds (\ s a -> s{_atgmdaSubnetIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
atgmdaTransitGatewayMulticastDomainId :: Lens' AcceptTransitGatewayMulticastDomainAssociations (Maybe Text)
atgmdaTransitGatewayMulticastDomainId = lens _atgmdaTransitGatewayMulticastDomainId (\ s a -> s{_atgmdaTransitGatewayMulticastDomainId = a})

-- | The ID of the transit gateway attachment.
atgmdaTransitGatewayAttachmentId :: Lens' AcceptTransitGatewayMulticastDomainAssociations (Maybe Text)
atgmdaTransitGatewayAttachmentId = lens _atgmdaTransitGatewayAttachmentId (\ s a -> s{_atgmdaTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
atgmdaDryRun :: Lens' AcceptTransitGatewayMulticastDomainAssociations (Maybe Bool)
atgmdaDryRun = lens _atgmdaDryRun (\ s a -> s{_atgmdaDryRun = a})

instance AWSRequest
           AcceptTransitGatewayMulticastDomainAssociations
         where
        type Rs
               AcceptTransitGatewayMulticastDomainAssociations
             =
             AcceptTransitGatewayMulticastDomainAssociationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AcceptTransitGatewayMulticastDomainAssociationsResponse'
                   <$> (x .@? "associations") <*> (pure (fromEnum s)))

instance Hashable
           AcceptTransitGatewayMulticastDomainAssociations
         where

instance NFData
           AcceptTransitGatewayMulticastDomainAssociations
         where

instance ToHeaders
           AcceptTransitGatewayMulticastDomainAssociations
         where
        toHeaders = const mempty

instance ToPath
           AcceptTransitGatewayMulticastDomainAssociations
         where
        toPath = const "/"

instance ToQuery
           AcceptTransitGatewayMulticastDomainAssociations
         where
        toQuery
          AcceptTransitGatewayMulticastDomainAssociations'{..}
          = mconcat
              ["Action" =:
                 ("AcceptTransitGatewayMulticastDomainAssociations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SubnetIds" <$> _atgmdaSubnetIds),
               "TransitGatewayMulticastDomainId" =:
                 _atgmdaTransitGatewayMulticastDomainId,
               "TransitGatewayAttachmentId" =:
                 _atgmdaTransitGatewayAttachmentId,
               "DryRun" =: _atgmdaDryRun]

-- | /See:/ 'acceptTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data AcceptTransitGatewayMulticastDomainAssociationsResponse = AcceptTransitGatewayMulticastDomainAssociationsResponse'
  { _atgmdarsAssociations :: !(Maybe TransitGatewayMulticastDomainAssociations)
  , _atgmdarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptTransitGatewayMulticastDomainAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgmdarsAssociations' - Undocumented member.
--
-- * 'atgmdarsResponseStatus' - -- | The response status code.
acceptTransitGatewayMulticastDomainAssociationsResponse
    :: Int -- ^ 'atgmdarsResponseStatus'
    -> AcceptTransitGatewayMulticastDomainAssociationsResponse
acceptTransitGatewayMulticastDomainAssociationsResponse pResponseStatus_ =
  AcceptTransitGatewayMulticastDomainAssociationsResponse'
    { _atgmdarsAssociations = Nothing
    , _atgmdarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
atgmdarsAssociations :: Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse (Maybe TransitGatewayMulticastDomainAssociations)
atgmdarsAssociations = lens _atgmdarsAssociations (\ s a -> s{_atgmdarsAssociations = a})

-- | -- | The response status code.
atgmdarsResponseStatus :: Lens' AcceptTransitGatewayMulticastDomainAssociationsResponse Int
atgmdarsResponseStatus = lens _atgmdarsResponseStatus (\ s a -> s{_atgmdarsResponseStatus = a})

instance NFData
           AcceptTransitGatewayMulticastDomainAssociationsResponse
         where
