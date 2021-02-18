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
-- Module      : Network.AWS.EC2.StartNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts analyzing the specified path. If the path is reachable, the operation returns the shortest feasible path.
--
--
module Network.AWS.EC2.StartNetworkInsightsAnalysis
    (
    -- * Creating a Request
      startNetworkInsightsAnalysis
    , StartNetworkInsightsAnalysis
    -- * Request Lenses
    , sniaFilterInARNs
    , sniaTagSpecifications
    , sniaDryRun
    , sniaNetworkInsightsPathId
    , sniaClientToken

    -- * Destructuring the Response
    , startNetworkInsightsAnalysisResponse
    , StartNetworkInsightsAnalysisResponse
    -- * Response Lenses
    , sniarsNetworkInsightsAnalysis
    , sniarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startNetworkInsightsAnalysis' smart constructor.
data StartNetworkInsightsAnalysis = StartNetworkInsightsAnalysis'
  { _sniaFilterInARNs          :: !(Maybe [Text])
  , _sniaTagSpecifications     :: !(Maybe [TagSpecification])
  , _sniaDryRun                :: !(Maybe Bool)
  , _sniaNetworkInsightsPathId :: !Text
  , _sniaClientToken           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNetworkInsightsAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sniaFilterInARNs' - The Amazon Resource Names (ARN) of the resources that the path must traverse.
--
-- * 'sniaTagSpecifications' - The tags to apply.
--
-- * 'sniaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'sniaNetworkInsightsPathId' - The ID of the path.
--
-- * 'sniaClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
startNetworkInsightsAnalysis
    :: Text -- ^ 'sniaNetworkInsightsPathId'
    -> Text -- ^ 'sniaClientToken'
    -> StartNetworkInsightsAnalysis
startNetworkInsightsAnalysis pNetworkInsightsPathId_ pClientToken_ =
  StartNetworkInsightsAnalysis'
    { _sniaFilterInARNs = Nothing
    , _sniaTagSpecifications = Nothing
    , _sniaDryRun = Nothing
    , _sniaNetworkInsightsPathId = pNetworkInsightsPathId_
    , _sniaClientToken = pClientToken_
    }


-- | The Amazon Resource Names (ARN) of the resources that the path must traverse.
sniaFilterInARNs :: Lens' StartNetworkInsightsAnalysis [Text]
sniaFilterInARNs = lens _sniaFilterInARNs (\ s a -> s{_sniaFilterInARNs = a}) . _Default . _Coerce

-- | The tags to apply.
sniaTagSpecifications :: Lens' StartNetworkInsightsAnalysis [TagSpecification]
sniaTagSpecifications = lens _sniaTagSpecifications (\ s a -> s{_sniaTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
sniaDryRun :: Lens' StartNetworkInsightsAnalysis (Maybe Bool)
sniaDryRun = lens _sniaDryRun (\ s a -> s{_sniaDryRun = a})

-- | The ID of the path.
sniaNetworkInsightsPathId :: Lens' StartNetworkInsightsAnalysis Text
sniaNetworkInsightsPathId = lens _sniaNetworkInsightsPathId (\ s a -> s{_sniaNetworkInsightsPathId = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
sniaClientToken :: Lens' StartNetworkInsightsAnalysis Text
sniaClientToken = lens _sniaClientToken (\ s a -> s{_sniaClientToken = a})

instance AWSRequest StartNetworkInsightsAnalysis
         where
        type Rs StartNetworkInsightsAnalysis =
             StartNetworkInsightsAnalysisResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 StartNetworkInsightsAnalysisResponse' <$>
                   (x .@? "networkInsightsAnalysis") <*>
                     (pure (fromEnum s)))

instance Hashable StartNetworkInsightsAnalysis where

instance NFData StartNetworkInsightsAnalysis where

instance ToHeaders StartNetworkInsightsAnalysis where
        toHeaders = const mempty

instance ToPath StartNetworkInsightsAnalysis where
        toPath = const "/"

instance ToQuery StartNetworkInsightsAnalysis where
        toQuery StartNetworkInsightsAnalysis'{..}
          = mconcat
              ["Action" =:
                 ("StartNetworkInsightsAnalysis" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "FilterInArn" <$> _sniaFilterInARNs),
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _sniaTagSpecifications),
               "DryRun" =: _sniaDryRun,
               "NetworkInsightsPathId" =:
                 _sniaNetworkInsightsPathId,
               "ClientToken" =: _sniaClientToken]

-- | /See:/ 'startNetworkInsightsAnalysisResponse' smart constructor.
data StartNetworkInsightsAnalysisResponse = StartNetworkInsightsAnalysisResponse'
  { _sniarsNetworkInsightsAnalysis :: !(Maybe NetworkInsightsAnalysis)
  , _sniarsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNetworkInsightsAnalysisResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sniarsNetworkInsightsAnalysis' - Information about the network insights analysis.
--
-- * 'sniarsResponseStatus' - -- | The response status code.
startNetworkInsightsAnalysisResponse
    :: Int -- ^ 'sniarsResponseStatus'
    -> StartNetworkInsightsAnalysisResponse
startNetworkInsightsAnalysisResponse pResponseStatus_ =
  StartNetworkInsightsAnalysisResponse'
    { _sniarsNetworkInsightsAnalysis = Nothing
    , _sniarsResponseStatus = pResponseStatus_
    }


-- | Information about the network insights analysis.
sniarsNetworkInsightsAnalysis :: Lens' StartNetworkInsightsAnalysisResponse (Maybe NetworkInsightsAnalysis)
sniarsNetworkInsightsAnalysis = lens _sniarsNetworkInsightsAnalysis (\ s a -> s{_sniarsNetworkInsightsAnalysis = a})

-- | -- | The response status code.
sniarsResponseStatus :: Lens' StartNetworkInsightsAnalysisResponse Int
sniarsResponseStatus = lens _sniarsResponseStatus (\ s a -> s{_sniarsResponseStatus = a})

instance NFData StartNetworkInsightsAnalysisResponse
         where
