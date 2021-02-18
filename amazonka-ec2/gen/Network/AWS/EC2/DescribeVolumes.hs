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
-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS volumes or all of your EBS volumes.
--
--
-- If you are describing a long list of volumes, we recommend that you paginate the output to make the list more manageable. The @MaxResults@ parameter sets the maximum number of results returned in a single page. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeVolumes@ request to retrieve the remaining results.
--
-- For more information about EBS volumes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumes
    (
    -- * Creating a Request
      describeVolumes
    , DescribeVolumes
    -- * Request Lenses
    , desFilters
    , desVolumeIds
    , desNextToken
    , desDryRun
    , desMaxResults

    -- * Destructuring the Response
    , describeVolumesResponse
    , DescribeVolumesResponse
    -- * Response Lenses
    , dvvrsNextToken
    , dvvrsVolumes
    , dvvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { _desFilters    :: !(Maybe [Filter])
  , _desVolumeIds  :: !(Maybe [Text])
  , _desNextToken  :: !(Maybe Text)
  , _desDryRun     :: !(Maybe Bool)
  , _desMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desFilters' - The filters.     * @attachment.attach-time@ - The time stamp when the attachment initiated.     * @attachment.delete-on-termination@ - Whether the volume is deleted on instance termination.     * @attachment.device@ - The device name specified in the block device mapping (for example, @/dev/sda1@ ).     * @attachment.instance-id@ - The ID of the instance the volume is attached to.     * @attachment.status@ - The attachment state (@attaching@ | @attached@ | @detaching@ ).     * @availability-zone@ - The Availability Zone in which the volume was created.     * @create-time@ - The time stamp when the volume was created.     * @encrypted@ - Indicates whether the volume is encrypted (@true@ | @false@ )     * @multi-attach-enabled@ - Indicates whether the volume is enabled for Multi-Attach (@true@ | @false@ )     * @fast-restored@ - Indicates whether the volume was created from a snapshot that is enabled for fast snapshot restore (@true@ | @false@ ).     * @size@ - The size of the volume, in GiB.     * @snapshot-id@ - The snapshot from which the volume was created.     * @status@ - The state of the volume (@creating@ | @available@ | @in-use@ | @deleting@ | @deleted@ | @error@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @volume-id@ - The volume ID.     * @volume-type@ - The Amazon EBS volume type (@gp2@ | @gp3@ | @io1@ | @io2@ | @st1@ | @sc1@ | @standard@ )
--
-- * 'desVolumeIds' - The volume IDs.
--
-- * 'desNextToken' - The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'desDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'desMaxResults' - The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
describeVolumes
    :: DescribeVolumes
describeVolumes =
  DescribeVolumes'
    { _desFilters = Nothing
    , _desVolumeIds = Nothing
    , _desNextToken = Nothing
    , _desDryRun = Nothing
    , _desMaxResults = Nothing
    }


-- | The filters.     * @attachment.attach-time@ - The time stamp when the attachment initiated.     * @attachment.delete-on-termination@ - Whether the volume is deleted on instance termination.     * @attachment.device@ - The device name specified in the block device mapping (for example, @/dev/sda1@ ).     * @attachment.instance-id@ - The ID of the instance the volume is attached to.     * @attachment.status@ - The attachment state (@attaching@ | @attached@ | @detaching@ ).     * @availability-zone@ - The Availability Zone in which the volume was created.     * @create-time@ - The time stamp when the volume was created.     * @encrypted@ - Indicates whether the volume is encrypted (@true@ | @false@ )     * @multi-attach-enabled@ - Indicates whether the volume is enabled for Multi-Attach (@true@ | @false@ )     * @fast-restored@ - Indicates whether the volume was created from a snapshot that is enabled for fast snapshot restore (@true@ | @false@ ).     * @size@ - The size of the volume, in GiB.     * @snapshot-id@ - The snapshot from which the volume was created.     * @status@ - The state of the volume (@creating@ | @available@ | @in-use@ | @deleting@ | @deleted@ | @error@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @volume-id@ - The volume ID.     * @volume-type@ - The Amazon EBS volume type (@gp2@ | @gp3@ | @io1@ | @io2@ | @st1@ | @sc1@ | @standard@ )
desFilters :: Lens' DescribeVolumes [Filter]
desFilters = lens _desFilters (\ s a -> s{_desFilters = a}) . _Default . _Coerce

-- | The volume IDs.
desVolumeIds :: Lens' DescribeVolumes [Text]
desVolumeIds = lens _desVolumeIds (\ s a -> s{_desVolumeIds = a}) . _Default . _Coerce

-- | The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
desNextToken :: Lens' DescribeVolumes (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
desDryRun :: Lens' DescribeVolumes (Maybe Bool)
desDryRun = lens _desDryRun (\ s a -> s{_desDryRun = a})

-- | The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
desMaxResults :: Lens' DescribeVolumes (Maybe Int)
desMaxResults = lens _desMaxResults (\ s a -> s{_desMaxResults = a})

instance AWSPager DescribeVolumes where
        page rq rs
          | stop (rs ^. dvvrsNextToken) = Nothing
          | stop (rs ^. dvvrsVolumes) = Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dvvrsNextToken

instance AWSRequest DescribeVolumes where
        type Rs DescribeVolumes = DescribeVolumesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVolumesResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "volumeSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVolumes where

instance NFData DescribeVolumes where

instance ToHeaders DescribeVolumes where
        toHeaders = const mempty

instance ToPath DescribeVolumes where
        toPath = const "/"

instance ToQuery DescribeVolumes where
        toQuery DescribeVolumes'{..}
          = mconcat
              ["Action" =: ("DescribeVolumes" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _desFilters),
               toQuery (toQueryList "VolumeId" <$> _desVolumeIds),
               "NextToken" =: _desNextToken, "DryRun" =: _desDryRun,
               "MaxResults" =: _desMaxResults]

-- | /See:/ 'describeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { _dvvrsNextToken      :: !(Maybe Text)
  , _dvvrsVolumes        :: !(Maybe [Volume])
  , _dvvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvvrsNextToken' - The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvvrsVolumes' - Information about the volumes.
--
-- * 'dvvrsResponseStatus' - -- | The response status code.
describeVolumesResponse
    :: Int -- ^ 'dvvrsResponseStatus'
    -> DescribeVolumesResponse
describeVolumesResponse pResponseStatus_ =
  DescribeVolumesResponse'
    { _dvvrsNextToken = Nothing
    , _dvvrsVolumes = Nothing
    , _dvvrsResponseStatus = pResponseStatus_
    }


-- | The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvvrsNextToken :: Lens' DescribeVolumesResponse (Maybe Text)
dvvrsNextToken = lens _dvvrsNextToken (\ s a -> s{_dvvrsNextToken = a})

-- | Information about the volumes.
dvvrsVolumes :: Lens' DescribeVolumesResponse [Volume]
dvvrsVolumes = lens _dvvrsVolumes (\ s a -> s{_dvvrsVolumes = a}) . _Default . _Coerce

-- | -- | The response status code.
dvvrsResponseStatus :: Lens' DescribeVolumesResponse Int
dvvrsResponseStatus = lens _dvvrsResponseStatus (\ s a -> s{_dvvrsResponseStatus = a})

instance NFData DescribeVolumesResponse where
