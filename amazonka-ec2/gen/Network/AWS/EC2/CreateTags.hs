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
-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites only the specified tags for the specified Amazon EC2 resource or resources. When you specify an existing tag key, the value is overwritten with the new value. Each resource can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique per resource.
--
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ . For more information about creating IAM policies that control users' access to resources based on tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html Supported Resource-Level Permissions for Amazon EC2 API Actions> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateTags
    (
    -- * Creating a Request
      createTags
    , CreateTags
    -- * Request Lenses
    , ctsDryRun
    , ctsResources
    , ctsTags

    -- * Destructuring the Response
    , createTagsResponse
    , CreateTagsResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctsDryRun    :: !(Maybe Bool)
  , _ctsResources :: ![Text]
  , _ctsTags      :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctsResources' - The IDs of the resources, separated by spaces. Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
--
-- * 'ctsTags' - The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
createTags
    :: CreateTags
createTags =
  CreateTags' {_ctsDryRun = Nothing, _ctsResources = mempty, _ctsTags = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctsDryRun :: Lens' CreateTags (Maybe Bool)
ctsDryRun = lens _ctsDryRun (\ s a -> s{_ctsDryRun = a})

-- | The IDs of the resources, separated by spaces. Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
ctsResources :: Lens' CreateTags [Text]
ctsResources = lens _ctsResources (\ s a -> s{_ctsResources = a}) . _Coerce

-- | The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
ctsTags :: Lens' CreateTags [Tag]
ctsTags = lens _ctsTags (\ s a -> s{_ctsTags = a}) . _Coerce

instance AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        request = postQuery ec2
        response = receiveNull CreateTagsResponse'

instance Hashable CreateTags where

instance NFData CreateTags where

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery CreateTags'{..}
          = mconcat
              ["Action" =: ("CreateTags" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ctsDryRun,
               toQueryList "ResourceId" _ctsResources,
               toQueryList "Tag" _ctsTags]

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
  CreateTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
createTagsResponse
    :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'


instance NFData CreateTagsResponse where
