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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one attribute at a time.
--
--
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    (
    -- * Creating a Request
      describeNetworkInterfaceAttribute
    , DescribeNetworkInterfaceAttribute
    -- * Request Lenses
    , dnianAttribute
    , dnianDryRun
    , dnianNetworkInterfaceId

    -- * Destructuring the Response
    , describeNetworkInterfaceAttributeResponse
    , DescribeNetworkInterfaceAttributeResponse
    -- * Response Lenses
    , dnianrsGroups
    , dnianrsSourceDestCheck
    , dnianrsNetworkInterfaceId
    , dnianrsAttachment
    , dnianrsDescription
    , dnianrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
--
--
-- /See:/ 'describeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { _dnianAttribute          :: !(Maybe NetworkInterfaceAttribute)
  , _dnianDryRun             :: !(Maybe Bool)
  , _dnianNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnianAttribute' - The attribute of the network interface. This parameter is required.
--
-- * 'dnianDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dnianNetworkInterfaceId' - The ID of the network interface.
describeNetworkInterfaceAttribute
    :: Text -- ^ 'dnianNetworkInterfaceId'
    -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute pNetworkInterfaceId_ =
  DescribeNetworkInterfaceAttribute'
    { _dnianAttribute = Nothing
    , _dnianDryRun = Nothing
    , _dnianNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | The attribute of the network interface. This parameter is required.
dnianAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dnianAttribute = lens _dnianAttribute (\ s a -> s{_dnianAttribute = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnianDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dnianDryRun = lens _dnianDryRun (\ s a -> s{_dnianDryRun = a})

-- | The ID of the network interface.
dnianNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dnianNetworkInterfaceId = lens _dnianNetworkInterfaceId (\ s a -> s{_dnianNetworkInterfaceId = a})

instance AWSRequest DescribeNetworkInterfaceAttribute
         where
        type Rs DescribeNetworkInterfaceAttribute =
             DescribeNetworkInterfaceAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkInterfaceAttributeResponse' <$>
                   (x .@? "groupSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "sourceDestCheck")
                     <*> (x .@? "networkInterfaceId")
                     <*> (x .@? "attachment")
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNetworkInterfaceAttribute
         where

instance NFData DescribeNetworkInterfaceAttribute
         where

instance ToHeaders DescribeNetworkInterfaceAttribute
         where
        toHeaders = const mempty

instance ToPath DescribeNetworkInterfaceAttribute
         where
        toPath = const "/"

instance ToQuery DescribeNetworkInterfaceAttribute
         where
        toQuery DescribeNetworkInterfaceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeNetworkInterfaceAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _dnianAttribute,
               "DryRun" =: _dnianDryRun,
               "NetworkInterfaceId" =: _dnianNetworkInterfaceId]

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
--
--
-- /See:/ 'describeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { _dnianrsGroups             :: !(Maybe [GroupIdentifier])
  , _dnianrsSourceDestCheck    :: !(Maybe AttributeBooleanValue)
  , _dnianrsNetworkInterfaceId :: !(Maybe Text)
  , _dnianrsAttachment         :: !(Maybe NetworkInterfaceAttachment)
  , _dnianrsDescription        :: !(Maybe AttributeValue)
  , _dnianrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnianrsGroups' - The security groups associated with the network interface.
--
-- * 'dnianrsSourceDestCheck' - Indicates whether source/destination checking is enabled.
--
-- * 'dnianrsNetworkInterfaceId' - The ID of the network interface.
--
-- * 'dnianrsAttachment' - The attachment (if any) of the network interface.
--
-- * 'dnianrsDescription' - The description of the network interface.
--
-- * 'dnianrsResponseStatus' - -- | The response status code.
describeNetworkInterfaceAttributeResponse
    :: Int -- ^ 'dnianrsResponseStatus'
    -> DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse pResponseStatus_ =
  DescribeNetworkInterfaceAttributeResponse'
    { _dnianrsGroups = Nothing
    , _dnianrsSourceDestCheck = Nothing
    , _dnianrsNetworkInterfaceId = Nothing
    , _dnianrsAttachment = Nothing
    , _dnianrsDescription = Nothing
    , _dnianrsResponseStatus = pResponseStatus_
    }


-- | The security groups associated with the network interface.
dnianrsGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dnianrsGroups = lens _dnianrsGroups (\ s a -> s{_dnianrsGroups = a}) . _Default . _Coerce

-- | Indicates whether source/destination checking is enabled.
dnianrsSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dnianrsSourceDestCheck = lens _dnianrsSourceDestCheck (\ s a -> s{_dnianrsSourceDestCheck = a})

-- | The ID of the network interface.
dnianrsNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dnianrsNetworkInterfaceId = lens _dnianrsNetworkInterfaceId (\ s a -> s{_dnianrsNetworkInterfaceId = a})

-- | The attachment (if any) of the network interface.
dnianrsAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dnianrsAttachment = lens _dnianrsAttachment (\ s a -> s{_dnianrsAttachment = a})

-- | The description of the network interface.
dnianrsDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dnianrsDescription = lens _dnianrsDescription (\ s a -> s{_dnianrsDescription = a})

-- | -- | The response status code.
dnianrsResponseStatus :: Lens' DescribeNetworkInterfaceAttributeResponse Int
dnianrsResponseStatus = lens _dnianrsResponseStatus (\ s a -> s{_dnianrsResponseStatus = a})

instance NFData
           DescribeNetworkInterfaceAttributeResponse
         where
