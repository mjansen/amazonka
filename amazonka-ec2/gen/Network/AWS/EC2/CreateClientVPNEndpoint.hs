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
-- Module      : Network.AWS.EC2.CreateClientVPNEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Client VPN endpoint. A Client VPN endpoint is the resource you create and configure to enable and manage client VPN sessions. It is the destination endpoint at which all client VPN sessions are terminated.
--
--
module Network.AWS.EC2.CreateClientVPNEndpoint
    (
    -- * Creating a Request
      createClientVPNEndpoint
    , CreateClientVPNEndpoint
    -- * Request Lenses
    , ccveSecurityGroupIds
    , ccveSplitTunnel
    , ccveClientToken
    , ccveTransportProtocol
    , ccveVPCId
    , ccveVPNPort
    , ccveTagSpecifications
    , ccveDNSServers
    , ccveClientConnectOptions
    , ccveSelfServicePortal
    , ccveDescription
    , ccveDryRun
    , ccveClientCidrBlock
    , ccveServerCertificateARN
    , ccveAuthenticationOptions
    , ccveConnectionLogOptions

    -- * Destructuring the Response
    , createClientVPNEndpointResponse
    , CreateClientVPNEndpointResponse
    -- * Response Lenses
    , ccversStatus
    , ccversClientVPNEndpointId
    , ccversDNSName
    , ccversResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createClientVPNEndpoint' smart constructor.
data CreateClientVPNEndpoint = CreateClientVPNEndpoint'
  { _ccveSecurityGroupIds      :: !(Maybe [Text])
  , _ccveSplitTunnel           :: !(Maybe Bool)
  , _ccveClientToken           :: !(Maybe Text)
  , _ccveTransportProtocol     :: !(Maybe TransportProtocol)
  , _ccveVPCId                 :: !(Maybe Text)
  , _ccveVPNPort               :: !(Maybe Int)
  , _ccveTagSpecifications     :: !(Maybe [TagSpecification])
  , _ccveDNSServers            :: !(Maybe [Text])
  , _ccveClientConnectOptions  :: !(Maybe ClientConnectOptions)
  , _ccveSelfServicePortal     :: !(Maybe SelfServicePortal)
  , _ccveDescription           :: !(Maybe Text)
  , _ccveDryRun                :: !(Maybe Bool)
  , _ccveClientCidrBlock       :: !Text
  , _ccveServerCertificateARN  :: !Text
  , _ccveAuthenticationOptions :: ![ClientVPNAuthenticationRequest]
  , _ccveConnectionLogOptions  :: !ConnectionLogOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClientVPNEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccveSecurityGroupIds' - The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
--
-- * 'ccveSplitTunnel' - Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint. By default, split-tunnel on a VPN endpoint is disabled. For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- * 'ccveClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ccveTransportProtocol' - The transport protocol to be used by the VPN session. Default value: @udp@
--
-- * 'ccveVPCId' - The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
--
-- * 'ccveVPNPort' - The port number to assign to the Client VPN endpoint for TCP and UDP traffic. Valid Values: @443@ | @1194@  Default Value: @443@
--
-- * 'ccveTagSpecifications' - The tags to apply to the Client VPN endpoint during creation.
--
-- * 'ccveDNSServers' - Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
--
-- * 'ccveClientConnectOptions' - The options for managing connection authorization for new client connections.
--
-- * 'ccveSelfServicePortal' - Specify whether to enable the self-service portal for the Client VPN endpoint. Default Value: @enabled@
--
-- * 'ccveDescription' - A brief description of the Client VPN endpoint.
--
-- * 'ccveDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ccveClientCidrBlock' - The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
--
-- * 'ccveServerCertificateARN' - The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
--
-- * 'ccveAuthenticationOptions' - Information about the authentication method to be used to authenticate clients.
--
-- * 'ccveConnectionLogOptions' - Information about the client connection logging options. If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:     * Client connection requests     * Client connection results (successful and unsuccessful)     * Reasons for unsuccessful client connection requests     * Client connection termination time
createClientVPNEndpoint
    :: Text -- ^ 'ccveClientCidrBlock'
    -> Text -- ^ 'ccveServerCertificateARN'
    -> ConnectionLogOptions -- ^ 'ccveConnectionLogOptions'
    -> CreateClientVPNEndpoint
createClientVPNEndpoint pClientCidrBlock_ pServerCertificateARN_ pConnectionLogOptions_ =
  CreateClientVPNEndpoint'
    { _ccveSecurityGroupIds = Nothing
    , _ccveSplitTunnel = Nothing
    , _ccveClientToken = Nothing
    , _ccveTransportProtocol = Nothing
    , _ccveVPCId = Nothing
    , _ccveVPNPort = Nothing
    , _ccveTagSpecifications = Nothing
    , _ccveDNSServers = Nothing
    , _ccveClientConnectOptions = Nothing
    , _ccveSelfServicePortal = Nothing
    , _ccveDescription = Nothing
    , _ccveDryRun = Nothing
    , _ccveClientCidrBlock = pClientCidrBlock_
    , _ccveServerCertificateARN = pServerCertificateARN_
    , _ccveAuthenticationOptions = mempty
    , _ccveConnectionLogOptions = pConnectionLogOptions_
    }


-- | The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
ccveSecurityGroupIds :: Lens' CreateClientVPNEndpoint [Text]
ccveSecurityGroupIds = lens _ccveSecurityGroupIds (\ s a -> s{_ccveSecurityGroupIds = a}) . _Default . _Coerce

-- | Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint. By default, split-tunnel on a VPN endpoint is disabled. For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
ccveSplitTunnel :: Lens' CreateClientVPNEndpoint (Maybe Bool)
ccveSplitTunnel = lens _ccveSplitTunnel (\ s a -> s{_ccveSplitTunnel = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ccveClientToken :: Lens' CreateClientVPNEndpoint (Maybe Text)
ccveClientToken = lens _ccveClientToken (\ s a -> s{_ccveClientToken = a})

-- | The transport protocol to be used by the VPN session. Default value: @udp@
ccveTransportProtocol :: Lens' CreateClientVPNEndpoint (Maybe TransportProtocol)
ccveTransportProtocol = lens _ccveTransportProtocol (\ s a -> s{_ccveTransportProtocol = a})

-- | The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
ccveVPCId :: Lens' CreateClientVPNEndpoint (Maybe Text)
ccveVPCId = lens _ccveVPCId (\ s a -> s{_ccveVPCId = a})

-- | The port number to assign to the Client VPN endpoint for TCP and UDP traffic. Valid Values: @443@ | @1194@  Default Value: @443@
ccveVPNPort :: Lens' CreateClientVPNEndpoint (Maybe Int)
ccveVPNPort = lens _ccveVPNPort (\ s a -> s{_ccveVPNPort = a})

-- | The tags to apply to the Client VPN endpoint during creation.
ccveTagSpecifications :: Lens' CreateClientVPNEndpoint [TagSpecification]
ccveTagSpecifications = lens _ccveTagSpecifications (\ s a -> s{_ccveTagSpecifications = a}) . _Default . _Coerce

-- | Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
ccveDNSServers :: Lens' CreateClientVPNEndpoint [Text]
ccveDNSServers = lens _ccveDNSServers (\ s a -> s{_ccveDNSServers = a}) . _Default . _Coerce

-- | The options for managing connection authorization for new client connections.
ccveClientConnectOptions :: Lens' CreateClientVPNEndpoint (Maybe ClientConnectOptions)
ccveClientConnectOptions = lens _ccveClientConnectOptions (\ s a -> s{_ccveClientConnectOptions = a})

-- | Specify whether to enable the self-service portal for the Client VPN endpoint. Default Value: @enabled@
ccveSelfServicePortal :: Lens' CreateClientVPNEndpoint (Maybe SelfServicePortal)
ccveSelfServicePortal = lens _ccveSelfServicePortal (\ s a -> s{_ccveSelfServicePortal = a})

-- | A brief description of the Client VPN endpoint.
ccveDescription :: Lens' CreateClientVPNEndpoint (Maybe Text)
ccveDescription = lens _ccveDescription (\ s a -> s{_ccveDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ccveDryRun :: Lens' CreateClientVPNEndpoint (Maybe Bool)
ccveDryRun = lens _ccveDryRun (\ s a -> s{_ccveDryRun = a})

-- | The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
ccveClientCidrBlock :: Lens' CreateClientVPNEndpoint Text
ccveClientCidrBlock = lens _ccveClientCidrBlock (\ s a -> s{_ccveClientCidrBlock = a})

-- | The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
ccveServerCertificateARN :: Lens' CreateClientVPNEndpoint Text
ccveServerCertificateARN = lens _ccveServerCertificateARN (\ s a -> s{_ccveServerCertificateARN = a})

-- | Information about the authentication method to be used to authenticate clients.
ccveAuthenticationOptions :: Lens' CreateClientVPNEndpoint [ClientVPNAuthenticationRequest]
ccveAuthenticationOptions = lens _ccveAuthenticationOptions (\ s a -> s{_ccveAuthenticationOptions = a}) . _Coerce

-- | Information about the client connection logging options. If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:     * Client connection requests     * Client connection results (successful and unsuccessful)     * Reasons for unsuccessful client connection requests     * Client connection termination time
ccveConnectionLogOptions :: Lens' CreateClientVPNEndpoint ConnectionLogOptions
ccveConnectionLogOptions = lens _ccveConnectionLogOptions (\ s a -> s{_ccveConnectionLogOptions = a})

instance AWSRequest CreateClientVPNEndpoint where
        type Rs CreateClientVPNEndpoint =
             CreateClientVPNEndpointResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateClientVPNEndpointResponse' <$>
                   (x .@? "status") <*> (x .@? "clientVpnEndpointId")
                     <*> (x .@? "dnsName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateClientVPNEndpoint where

instance NFData CreateClientVPNEndpoint where

instance ToHeaders CreateClientVPNEndpoint where
        toHeaders = const mempty

instance ToPath CreateClientVPNEndpoint where
        toPath = const "/"

instance ToQuery CreateClientVPNEndpoint where
        toQuery CreateClientVPNEndpoint'{..}
          = mconcat
              ["Action" =:
                 ("CreateClientVpnEndpoint" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _ccveSecurityGroupIds),
               "SplitTunnel" =: _ccveSplitTunnel,
               "ClientToken" =: _ccveClientToken,
               "TransportProtocol" =: _ccveTransportProtocol,
               "VpcId" =: _ccveVPCId, "VpnPort" =: _ccveVPNPort,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _ccveTagSpecifications),
               toQuery
                 (toQueryList "DnsServers" <$> _ccveDNSServers),
               "ClientConnectOptions" =: _ccveClientConnectOptions,
               "SelfServicePortal" =: _ccveSelfServicePortal,
               "Description" =: _ccveDescription,
               "DryRun" =: _ccveDryRun,
               "ClientCidrBlock" =: _ccveClientCidrBlock,
               "ServerCertificateArn" =: _ccveServerCertificateARN,
               toQueryList "Authentication"
                 _ccveAuthenticationOptions,
               "ConnectionLogOptions" =: _ccveConnectionLogOptions]

-- | /See:/ 'createClientVPNEndpointResponse' smart constructor.
data CreateClientVPNEndpointResponse = CreateClientVPNEndpointResponse'
  { _ccversStatus              :: !(Maybe ClientVPNEndpointStatus)
  , _ccversClientVPNEndpointId :: !(Maybe Text)
  , _ccversDNSName             :: !(Maybe Text)
  , _ccversResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccversStatus' - The current state of the Client VPN endpoint.
--
-- * 'ccversClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'ccversDNSName' - The DNS name to be used by clients when establishing their VPN session.
--
-- * 'ccversResponseStatus' - -- | The response status code.
createClientVPNEndpointResponse
    :: Int -- ^ 'ccversResponseStatus'
    -> CreateClientVPNEndpointResponse
createClientVPNEndpointResponse pResponseStatus_ =
  CreateClientVPNEndpointResponse'
    { _ccversStatus = Nothing
    , _ccversClientVPNEndpointId = Nothing
    , _ccversDNSName = Nothing
    , _ccversResponseStatus = pResponseStatus_
    }


-- | The current state of the Client VPN endpoint.
ccversStatus :: Lens' CreateClientVPNEndpointResponse (Maybe ClientVPNEndpointStatus)
ccversStatus = lens _ccversStatus (\ s a -> s{_ccversStatus = a})

-- | The ID of the Client VPN endpoint.
ccversClientVPNEndpointId :: Lens' CreateClientVPNEndpointResponse (Maybe Text)
ccversClientVPNEndpointId = lens _ccversClientVPNEndpointId (\ s a -> s{_ccversClientVPNEndpointId = a})

-- | The DNS name to be used by clients when establishing their VPN session.
ccversDNSName :: Lens' CreateClientVPNEndpointResponse (Maybe Text)
ccversDNSName = lens _ccversDNSName (\ s a -> s{_ccversDNSName = a})

-- | -- | The response status code.
ccversResponseStatus :: Lens' CreateClientVPNEndpointResponse Int
ccversResponseStatus = lens _ccversResponseStatus (\ s a -> s{_ccversResponseStatus = a})

instance NFData CreateClientVPNEndpointResponse where
