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
-- Module      : Network.AWS.EC2.DescribeNetworkInsightsAnalyses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network insights analyses.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInsightsAnalyses
    (
    -- * Creating a Request
      describeNetworkInsightsAnalyses
    , DescribeNetworkInsightsAnalyses
    -- * Request Lenses
    , dniaNetworkInsightsAnalysisIds
    , dniaAnalysisEndTime
    , dniaFilters
    , dniaNetworkInsightsPathId
    , dniaNextToken
    , dniaAnalysisStartTime
    , dniaDryRun
    , dniaMaxResults

    -- * Destructuring the Response
    , describeNetworkInsightsAnalysesResponse
    , DescribeNetworkInsightsAnalysesResponse
    -- * Response Lenses
    , dniarsNetworkInsightsAnalyses
    , dniarsNextToken
    , dniarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNetworkInsightsAnalyses' smart constructor.
data DescribeNetworkInsightsAnalyses = DescribeNetworkInsightsAnalyses'
  { _dniaNetworkInsightsAnalysisIds :: !(Maybe [Text])
  , _dniaAnalysisEndTime            :: !(Maybe ISO8601)
  , _dniaFilters                    :: !(Maybe [Filter])
  , _dniaNetworkInsightsPathId      :: !(Maybe Text)
  , _dniaNextToken                  :: !(Maybe Text)
  , _dniaAnalysisStartTime          :: !(Maybe ISO8601)
  , _dniaDryRun                     :: !(Maybe Bool)
  , _dniaMaxResults                 :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInsightsAnalyses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniaNetworkInsightsAnalysisIds' - The ID of the network insights analyses. You must specify either analysis IDs or a path ID.
--
-- * 'dniaAnalysisEndTime' - The time when the network insights analyses ended.
--
-- * 'dniaFilters' - The filters. The following are possible values:     * PathFound - A Boolean value that indicates whether a feasible path is found.     * Status - The status of the analysis (running | succeeded | failed).
--
-- * 'dniaNetworkInsightsPathId' - The ID of the path. You must specify either a path ID or analysis IDs.
--
-- * 'dniaNextToken' - The token for the next page of results.
--
-- * 'dniaAnalysisStartTime' - The time when the network insights analyses started.
--
-- * 'dniaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dniaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeNetworkInsightsAnalyses
    :: DescribeNetworkInsightsAnalyses
describeNetworkInsightsAnalyses =
  DescribeNetworkInsightsAnalyses'
    { _dniaNetworkInsightsAnalysisIds = Nothing
    , _dniaAnalysisEndTime = Nothing
    , _dniaFilters = Nothing
    , _dniaNetworkInsightsPathId = Nothing
    , _dniaNextToken = Nothing
    , _dniaAnalysisStartTime = Nothing
    , _dniaDryRun = Nothing
    , _dniaMaxResults = Nothing
    }


-- | The ID of the network insights analyses. You must specify either analysis IDs or a path ID.
dniaNetworkInsightsAnalysisIds :: Lens' DescribeNetworkInsightsAnalyses [Text]
dniaNetworkInsightsAnalysisIds = lens _dniaNetworkInsightsAnalysisIds (\ s a -> s{_dniaNetworkInsightsAnalysisIds = a}) . _Default . _Coerce

-- | The time when the network insights analyses ended.
dniaAnalysisEndTime :: Lens' DescribeNetworkInsightsAnalyses (Maybe UTCTime)
dniaAnalysisEndTime = lens _dniaAnalysisEndTime (\ s a -> s{_dniaAnalysisEndTime = a}) . mapping _Time

-- | The filters. The following are possible values:     * PathFound - A Boolean value that indicates whether a feasible path is found.     * Status - The status of the analysis (running | succeeded | failed).
dniaFilters :: Lens' DescribeNetworkInsightsAnalyses [Filter]
dniaFilters = lens _dniaFilters (\ s a -> s{_dniaFilters = a}) . _Default . _Coerce

-- | The ID of the path. You must specify either a path ID or analysis IDs.
dniaNetworkInsightsPathId :: Lens' DescribeNetworkInsightsAnalyses (Maybe Text)
dniaNetworkInsightsPathId = lens _dniaNetworkInsightsPathId (\ s a -> s{_dniaNetworkInsightsPathId = a})

-- | The token for the next page of results.
dniaNextToken :: Lens' DescribeNetworkInsightsAnalyses (Maybe Text)
dniaNextToken = lens _dniaNextToken (\ s a -> s{_dniaNextToken = a})

-- | The time when the network insights analyses started.
dniaAnalysisStartTime :: Lens' DescribeNetworkInsightsAnalyses (Maybe UTCTime)
dniaAnalysisStartTime = lens _dniaAnalysisStartTime (\ s a -> s{_dniaAnalysisStartTime = a}) . mapping _Time

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dniaDryRun :: Lens' DescribeNetworkInsightsAnalyses (Maybe Bool)
dniaDryRun = lens _dniaDryRun (\ s a -> s{_dniaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dniaMaxResults :: Lens' DescribeNetworkInsightsAnalyses (Maybe Natural)
dniaMaxResults = lens _dniaMaxResults (\ s a -> s{_dniaMaxResults = a}) . mapping _Nat

instance AWSPager DescribeNetworkInsightsAnalyses
         where
        page rq rs
          | stop (rs ^. dniarsNextToken) = Nothing
          | stop (rs ^. dniarsNetworkInsightsAnalyses) =
            Nothing
          | otherwise =
            Just $ rq & dniaNextToken .~ rs ^. dniarsNextToken

instance AWSRequest DescribeNetworkInsightsAnalyses
         where
        type Rs DescribeNetworkInsightsAnalyses =
             DescribeNetworkInsightsAnalysesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkInsightsAnalysesResponse' <$>
                   (x .@? "networkInsightsAnalysisSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNetworkInsightsAnalyses
         where

instance NFData DescribeNetworkInsightsAnalyses where

instance ToHeaders DescribeNetworkInsightsAnalyses
         where
        toHeaders = const mempty

instance ToPath DescribeNetworkInsightsAnalyses where
        toPath = const "/"

instance ToQuery DescribeNetworkInsightsAnalyses
         where
        toQuery DescribeNetworkInsightsAnalyses'{..}
          = mconcat
              ["Action" =:
                 ("DescribeNetworkInsightsAnalyses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "NetworkInsightsAnalysisId" <$>
                    _dniaNetworkInsightsAnalysisIds),
               "AnalysisEndTime" =: _dniaAnalysisEndTime,
               toQuery (toQueryList "Filter" <$> _dniaFilters),
               "NetworkInsightsPathId" =:
                 _dniaNetworkInsightsPathId,
               "NextToken" =: _dniaNextToken,
               "AnalysisStartTime" =: _dniaAnalysisStartTime,
               "DryRun" =: _dniaDryRun,
               "MaxResults" =: _dniaMaxResults]

-- | /See:/ 'describeNetworkInsightsAnalysesResponse' smart constructor.
data DescribeNetworkInsightsAnalysesResponse = DescribeNetworkInsightsAnalysesResponse'
  { _dniarsNetworkInsightsAnalyses :: !(Maybe [NetworkInsightsAnalysis])
  , _dniarsNextToken               :: !(Maybe Text)
  , _dniarsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInsightsAnalysesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniarsNetworkInsightsAnalyses' - Information about the network insights analyses.
--
-- * 'dniarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dniarsResponseStatus' - -- | The response status code.
describeNetworkInsightsAnalysesResponse
    :: Int -- ^ 'dniarsResponseStatus'
    -> DescribeNetworkInsightsAnalysesResponse
describeNetworkInsightsAnalysesResponse pResponseStatus_ =
  DescribeNetworkInsightsAnalysesResponse'
    { _dniarsNetworkInsightsAnalyses = Nothing
    , _dniarsNextToken = Nothing
    , _dniarsResponseStatus = pResponseStatus_
    }


-- | Information about the network insights analyses.
dniarsNetworkInsightsAnalyses :: Lens' DescribeNetworkInsightsAnalysesResponse [NetworkInsightsAnalysis]
dniarsNetworkInsightsAnalyses = lens _dniarsNetworkInsightsAnalyses (\ s a -> s{_dniarsNetworkInsightsAnalyses = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dniarsNextToken :: Lens' DescribeNetworkInsightsAnalysesResponse (Maybe Text)
dniarsNextToken = lens _dniarsNextToken (\ s a -> s{_dniarsNextToken = a})

-- | -- | The response status code.
dniarsResponseStatus :: Lens' DescribeNetworkInsightsAnalysesResponse Int
dniarsResponseStatus = lens _dniarsResponseStatus (\ s a -> s{_dniarsResponseStatus = a})

instance NFData
           DescribeNetworkInsightsAnalysesResponse
         where
