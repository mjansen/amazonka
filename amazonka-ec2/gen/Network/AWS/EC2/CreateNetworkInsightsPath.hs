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
-- Module      : Network.AWS.EC2.CreateNetworkInsightsPath
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a path to analyze for reachability.
--
--
-- Reachability Analyzer enables you to analyze and debug network reachability between two resources in your virtual private cloud (VPC). For more information, see <https://docs.aws.amazon.com/vpc/latest/reachability/ What is Reachability Analyzer> .
--
module Network.AWS.EC2.CreateNetworkInsightsPath
    (
    -- * Creating a Request
      createNetworkInsightsPath
    , CreateNetworkInsightsPath
    -- * Request Lenses
    , cDestinationIP
    , cTagSpecifications
    , cSourceIP
    , cDestinationPort
    , cDryRun
    , cSource
    , cDestination
    , cProtocol
    , cClientToken

    -- * Destructuring the Response
    , createNetworkInsightsPathResponse
    , CreateNetworkInsightsPathResponse
    -- * Response Lenses
    , crersNetworkInsightsPath
    , crersResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createNetworkInsightsPath' smart constructor.
data CreateNetworkInsightsPath = CreateNetworkInsightsPath'
  { _cDestinationIP     :: !(Maybe Text)
  , _cTagSpecifications :: !(Maybe [TagSpecification])
  , _cSourceIP          :: !(Maybe Text)
  , _cDestinationPort   :: !(Maybe Nat)
  , _cDryRun            :: !(Maybe Bool)
  , _cSource            :: !Text
  , _cDestination       :: !Text
  , _cProtocol          :: !Protocol
  , _cClientToken       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkInsightsPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cDestinationIP' - The IP address of the AWS resource that is the destination of the path.
--
-- * 'cTagSpecifications' - The tags to add to the path.
--
-- * 'cSourceIP' - The IP address of the AWS resource that is the source of the path.
--
-- * 'cDestinationPort' - The destination port.
--
-- * 'cDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cSource' - The AWS resource that is the source of the path.
--
-- * 'cDestination' - The AWS resource that is the destination of the path.
--
-- * 'cProtocol' - The protocol.
--
-- * 'cClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
createNetworkInsightsPath
    :: Text -- ^ 'cSource'
    -> Text -- ^ 'cDestination'
    -> Protocol -- ^ 'cProtocol'
    -> Text -- ^ 'cClientToken'
    -> CreateNetworkInsightsPath
createNetworkInsightsPath pSource_ pDestination_ pProtocol_ pClientToken_ =
  CreateNetworkInsightsPath'
    { _cDestinationIP = Nothing
    , _cTagSpecifications = Nothing
    , _cSourceIP = Nothing
    , _cDestinationPort = Nothing
    , _cDryRun = Nothing
    , _cSource = pSource_
    , _cDestination = pDestination_
    , _cProtocol = pProtocol_
    , _cClientToken = pClientToken_
    }


-- | The IP address of the AWS resource that is the destination of the path.
cDestinationIP :: Lens' CreateNetworkInsightsPath (Maybe Text)
cDestinationIP = lens _cDestinationIP (\ s a -> s{_cDestinationIP = a})

-- | The tags to add to the path.
cTagSpecifications :: Lens' CreateNetworkInsightsPath [TagSpecification]
cTagSpecifications = lens _cTagSpecifications (\ s a -> s{_cTagSpecifications = a}) . _Default . _Coerce

-- | The IP address of the AWS resource that is the source of the path.
cSourceIP :: Lens' CreateNetworkInsightsPath (Maybe Text)
cSourceIP = lens _cSourceIP (\ s a -> s{_cSourceIP = a})

-- | The destination port.
cDestinationPort :: Lens' CreateNetworkInsightsPath (Maybe Natural)
cDestinationPort = lens _cDestinationPort (\ s a -> s{_cDestinationPort = a}) . mapping _Nat

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cDryRun :: Lens' CreateNetworkInsightsPath (Maybe Bool)
cDryRun = lens _cDryRun (\ s a -> s{_cDryRun = a})

-- | The AWS resource that is the source of the path.
cSource :: Lens' CreateNetworkInsightsPath Text
cSource = lens _cSource (\ s a -> s{_cSource = a})

-- | The AWS resource that is the destination of the path.
cDestination :: Lens' CreateNetworkInsightsPath Text
cDestination = lens _cDestination (\ s a -> s{_cDestination = a})

-- | The protocol.
cProtocol :: Lens' CreateNetworkInsightsPath Protocol
cProtocol = lens _cProtocol (\ s a -> s{_cProtocol = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cClientToken :: Lens' CreateNetworkInsightsPath Text
cClientToken = lens _cClientToken (\ s a -> s{_cClientToken = a})

instance AWSRequest CreateNetworkInsightsPath where
        type Rs CreateNetworkInsightsPath =
             CreateNetworkInsightsPathResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateNetworkInsightsPathResponse' <$>
                   (x .@? "networkInsightsPath") <*>
                     (pure (fromEnum s)))

instance Hashable CreateNetworkInsightsPath where

instance NFData CreateNetworkInsightsPath where

instance ToHeaders CreateNetworkInsightsPath where
        toHeaders = const mempty

instance ToPath CreateNetworkInsightsPath where
        toPath = const "/"

instance ToQuery CreateNetworkInsightsPath where
        toQuery CreateNetworkInsightsPath'{..}
          = mconcat
              ["Action" =:
                 ("CreateNetworkInsightsPath" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DestinationIp" =: _cDestinationIP,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _cTagSpecifications),
               "SourceIp" =: _cSourceIP,
               "DestinationPort" =: _cDestinationPort,
               "DryRun" =: _cDryRun, "Source" =: _cSource,
               "Destination" =: _cDestination,
               "Protocol" =: _cProtocol,
               "ClientToken" =: _cClientToken]

-- | /See:/ 'createNetworkInsightsPathResponse' smart constructor.
data CreateNetworkInsightsPathResponse = CreateNetworkInsightsPathResponse'
  { _crersNetworkInsightsPath :: !(Maybe NetworkInsightsPath)
  , _crersResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkInsightsPathResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersNetworkInsightsPath' - Information about the path.
--
-- * 'crersResponseStatus' - -- | The response status code.
createNetworkInsightsPathResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateNetworkInsightsPathResponse
createNetworkInsightsPathResponse pResponseStatus_ =
  CreateNetworkInsightsPathResponse'
    { _crersNetworkInsightsPath = Nothing
    , _crersResponseStatus = pResponseStatus_
    }


-- | Information about the path.
crersNetworkInsightsPath :: Lens' CreateNetworkInsightsPathResponse (Maybe NetworkInsightsPath)
crersNetworkInsightsPath = lens _crersNetworkInsightsPath (\ s a -> s{_crersNetworkInsightsPath = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateNetworkInsightsPathResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateNetworkInsightsPathResponse
         where
