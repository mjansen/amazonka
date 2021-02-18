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
-- Module      : Network.AWS.EC2.DeleteNetworkInsightsPath
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified path.
--
--
module Network.AWS.EC2.DeleteNetworkInsightsPath
    (
    -- * Creating a Request
      deleteNetworkInsightsPath
    , DeleteNetworkInsightsPath
    -- * Request Lenses
    , dnipnDryRun
    , dnipnNetworkInsightsPathId

    -- * Destructuring the Response
    , deleteNetworkInsightsPathResponse
    , DeleteNetworkInsightsPathResponse
    -- * Response Lenses
    , dnipnrsNetworkInsightsPathId
    , dnipnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNetworkInsightsPath' smart constructor.
data DeleteNetworkInsightsPath = DeleteNetworkInsightsPath'
  { _dnipnDryRun                :: !(Maybe Bool)
  , _dnipnNetworkInsightsPathId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInsightsPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnipnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dnipnNetworkInsightsPathId' - The ID of the path.
deleteNetworkInsightsPath
    :: Text -- ^ 'dnipnNetworkInsightsPathId'
    -> DeleteNetworkInsightsPath
deleteNetworkInsightsPath pNetworkInsightsPathId_ =
  DeleteNetworkInsightsPath'
    { _dnipnDryRun = Nothing
    , _dnipnNetworkInsightsPathId = pNetworkInsightsPathId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnipnDryRun :: Lens' DeleteNetworkInsightsPath (Maybe Bool)
dnipnDryRun = lens _dnipnDryRun (\ s a -> s{_dnipnDryRun = a})

-- | The ID of the path.
dnipnNetworkInsightsPathId :: Lens' DeleteNetworkInsightsPath Text
dnipnNetworkInsightsPathId = lens _dnipnNetworkInsightsPathId (\ s a -> s{_dnipnNetworkInsightsPathId = a})

instance AWSRequest DeleteNetworkInsightsPath where
        type Rs DeleteNetworkInsightsPath =
             DeleteNetworkInsightsPathResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteNetworkInsightsPathResponse' <$>
                   (x .@? "networkInsightsPathId") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteNetworkInsightsPath where

instance NFData DeleteNetworkInsightsPath where

instance ToHeaders DeleteNetworkInsightsPath where
        toHeaders = const mempty

instance ToPath DeleteNetworkInsightsPath where
        toPath = const "/"

instance ToQuery DeleteNetworkInsightsPath where
        toQuery DeleteNetworkInsightsPath'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNetworkInsightsPath" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dnipnDryRun,
               "NetworkInsightsPathId" =:
                 _dnipnNetworkInsightsPathId]

-- | /See:/ 'deleteNetworkInsightsPathResponse' smart constructor.
data DeleteNetworkInsightsPathResponse = DeleteNetworkInsightsPathResponse'
  { _dnipnrsNetworkInsightsPathId :: !(Maybe Text)
  , _dnipnrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInsightsPathResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnipnrsNetworkInsightsPathId' - The ID of the path.
--
-- * 'dnipnrsResponseStatus' - -- | The response status code.
deleteNetworkInsightsPathResponse
    :: Int -- ^ 'dnipnrsResponseStatus'
    -> DeleteNetworkInsightsPathResponse
deleteNetworkInsightsPathResponse pResponseStatus_ =
  DeleteNetworkInsightsPathResponse'
    { _dnipnrsNetworkInsightsPathId = Nothing
    , _dnipnrsResponseStatus = pResponseStatus_
    }


-- | The ID of the path.
dnipnrsNetworkInsightsPathId :: Lens' DeleteNetworkInsightsPathResponse (Maybe Text)
dnipnrsNetworkInsightsPathId = lens _dnipnrsNetworkInsightsPathId (\ s a -> s{_dnipnrsNetworkInsightsPathId = a})

-- | -- | The response status code.
dnipnrsResponseStatus :: Lens' DeleteNetworkInsightsPathResponse Int
dnipnrsResponseStatus = lens _dnipnrsResponseStatus (\ s a -> s{_dnipnrsResponseStatus = a})

instance NFData DeleteNetworkInsightsPathResponse
         where
