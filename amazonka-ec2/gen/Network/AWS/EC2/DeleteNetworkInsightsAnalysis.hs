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
-- Module      : Network.AWS.EC2.DeleteNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network insights analysis.
--
--
module Network.AWS.EC2.DeleteNetworkInsightsAnalysis
    (
    -- * Creating a Request
      deleteNetworkInsightsAnalysis
    , DeleteNetworkInsightsAnalysis
    -- * Request Lenses
    , dniasDryRun
    , dniasNetworkInsightsAnalysisId

    -- * Destructuring the Response
    , deleteNetworkInsightsAnalysisResponse
    , DeleteNetworkInsightsAnalysisResponse
    -- * Response Lenses
    , dniasrsNetworkInsightsAnalysisId
    , dniasrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNetworkInsightsAnalysis' smart constructor.
data DeleteNetworkInsightsAnalysis = DeleteNetworkInsightsAnalysis'
  { _dniasDryRun                    :: !(Maybe Bool)
  , _dniasNetworkInsightsAnalysisId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInsightsAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniasDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dniasNetworkInsightsAnalysisId' - The ID of the network insights analysis.
deleteNetworkInsightsAnalysis
    :: Text -- ^ 'dniasNetworkInsightsAnalysisId'
    -> DeleteNetworkInsightsAnalysis
deleteNetworkInsightsAnalysis pNetworkInsightsAnalysisId_ =
  DeleteNetworkInsightsAnalysis'
    { _dniasDryRun = Nothing
    , _dniasNetworkInsightsAnalysisId = pNetworkInsightsAnalysisId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dniasDryRun :: Lens' DeleteNetworkInsightsAnalysis (Maybe Bool)
dniasDryRun = lens _dniasDryRun (\ s a -> s{_dniasDryRun = a})

-- | The ID of the network insights analysis.
dniasNetworkInsightsAnalysisId :: Lens' DeleteNetworkInsightsAnalysis Text
dniasNetworkInsightsAnalysisId = lens _dniasNetworkInsightsAnalysisId (\ s a -> s{_dniasNetworkInsightsAnalysisId = a})

instance AWSRequest DeleteNetworkInsightsAnalysis
         where
        type Rs DeleteNetworkInsightsAnalysis =
             DeleteNetworkInsightsAnalysisResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteNetworkInsightsAnalysisResponse' <$>
                   (x .@? "networkInsightsAnalysisId") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteNetworkInsightsAnalysis where

instance NFData DeleteNetworkInsightsAnalysis where

instance ToHeaders DeleteNetworkInsightsAnalysis
         where
        toHeaders = const mempty

instance ToPath DeleteNetworkInsightsAnalysis where
        toPath = const "/"

instance ToQuery DeleteNetworkInsightsAnalysis where
        toQuery DeleteNetworkInsightsAnalysis'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNetworkInsightsAnalysis" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dniasDryRun,
               "NetworkInsightsAnalysisId" =:
                 _dniasNetworkInsightsAnalysisId]

-- | /See:/ 'deleteNetworkInsightsAnalysisResponse' smart constructor.
data DeleteNetworkInsightsAnalysisResponse = DeleteNetworkInsightsAnalysisResponse'
  { _dniasrsNetworkInsightsAnalysisId :: !(Maybe Text)
  , _dniasrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInsightsAnalysisResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniasrsNetworkInsightsAnalysisId' - The ID of the network insights analysis.
--
-- * 'dniasrsResponseStatus' - -- | The response status code.
deleteNetworkInsightsAnalysisResponse
    :: Int -- ^ 'dniasrsResponseStatus'
    -> DeleteNetworkInsightsAnalysisResponse
deleteNetworkInsightsAnalysisResponse pResponseStatus_ =
  DeleteNetworkInsightsAnalysisResponse'
    { _dniasrsNetworkInsightsAnalysisId = Nothing
    , _dniasrsResponseStatus = pResponseStatus_
    }


-- | The ID of the network insights analysis.
dniasrsNetworkInsightsAnalysisId :: Lens' DeleteNetworkInsightsAnalysisResponse (Maybe Text)
dniasrsNetworkInsightsAnalysisId = lens _dniasrsNetworkInsightsAnalysisId (\ s a -> s{_dniasrsNetworkInsightsAnalysisId = a})

-- | -- | The response status code.
dniasrsResponseStatus :: Lens' DeleteNetworkInsightsAnalysisResponse Int
dniasrsResponseStatus = lens _dniasrsResponseStatus (\ s a -> s{_dniasrsResponseStatus = a})

instance NFData DeleteNetworkInsightsAnalysisResponse
         where
