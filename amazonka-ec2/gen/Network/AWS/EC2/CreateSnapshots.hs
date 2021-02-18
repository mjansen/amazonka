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
-- Module      : Network.AWS.EC2.CreateSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates crash-consistent snapshots of multiple EBS volumes and stores the data in S3. Volumes are chosen by specifying an instance. Any attached volumes will produce one snapshot each that is crash-consistent across the instance. Boot volumes can be excluded by changing the parameters.
--
--
-- You can create multi-volume snapshots of instances in a Region and instances on an Outpost. If you create snapshots from an instance in a Region, the snapshots must be stored in the same Region as the instance. If you create snapshots from an instance on an Outpost, the snapshots can be stored on the same Outpost as the instance, or in the Region for that Outpost.
--
module Network.AWS.EC2.CreateSnapshots
    (
    -- * Creating a Request
      createSnapshots
    , CreateSnapshots
    -- * Request Lenses
    , csOutpostARN
    , csTagSpecifications
    , csCopyTagsFromSource
    , csDescription
    , csDryRun
    , csInstanceSpecification

    -- * Destructuring the Response
    , createSnapshotsResponse
    , CreateSnapshotsResponse
    -- * Response Lenses
    , crsSnapshots
    , crsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSnapshots' smart constructor.
data CreateSnapshots = CreateSnapshots'
  { _csOutpostARN            :: !(Maybe Text)
  , _csTagSpecifications     :: !(Maybe [TagSpecification])
  , _csCopyTagsFromSource    :: !(Maybe CopyTagsFromSource)
  , _csDescription           :: !(Maybe Text)
  , _csDryRun                :: !(Maybe Bool)
  , _csInstanceSpecification :: !InstanceSpecification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csOutpostARN' - The Amazon Resource Name (ARN) of the AWS Outpost on which to create the local snapshots.     * To create snapshots from an instance in a Region, omit this parameter. The snapshots are created in the same Region as the instance.     * To create snapshots from an instance on an Outpost and store the snapshots in the Region, omit this parameter. The snapshots are created in the Region for the Outpost.     * To create snapshots from an instance on an Outpost and store the snapshots on an Outpost, specify the ARN of the destination Outpost. The snapshots must be created on the same Outpost as the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-multivol-snapshot Creating multi-volume local snapshots from instances on an Outpost> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'csTagSpecifications' - Tags to apply to every snapshot specified by the instance.
--
-- * 'csCopyTagsFromSource' - Copies the tags from the specified volume to corresponding snapshot.
--
-- * 'csDescription' - A description propagated to every snapshot specified by the instance.
--
-- * 'csDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'csInstanceSpecification' - The instance to specify which volumes should be included in the snapshots.
createSnapshots
    :: InstanceSpecification -- ^ 'csInstanceSpecification'
    -> CreateSnapshots
createSnapshots pInstanceSpecification_ =
  CreateSnapshots'
    { _csOutpostARN = Nothing
    , _csTagSpecifications = Nothing
    , _csCopyTagsFromSource = Nothing
    , _csDescription = Nothing
    , _csDryRun = Nothing
    , _csInstanceSpecification = pInstanceSpecification_
    }


-- | The Amazon Resource Name (ARN) of the AWS Outpost on which to create the local snapshots.     * To create snapshots from an instance in a Region, omit this parameter. The snapshots are created in the same Region as the instance.     * To create snapshots from an instance on an Outpost and store the snapshots in the Region, omit this parameter. The snapshots are created in the Region for the Outpost.     * To create snapshots from an instance on an Outpost and store the snapshots on an Outpost, specify the ARN of the destination Outpost. The snapshots must be created on the same Outpost as the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-multivol-snapshot Creating multi-volume local snapshots from instances on an Outpost> in the /Amazon Elastic Compute Cloud User Guide/ .
csOutpostARN :: Lens' CreateSnapshots (Maybe Text)
csOutpostARN = lens _csOutpostARN (\ s a -> s{_csOutpostARN = a})

-- | Tags to apply to every snapshot specified by the instance.
csTagSpecifications :: Lens' CreateSnapshots [TagSpecification]
csTagSpecifications = lens _csTagSpecifications (\ s a -> s{_csTagSpecifications = a}) . _Default . _Coerce

-- | Copies the tags from the specified volume to corresponding snapshot.
csCopyTagsFromSource :: Lens' CreateSnapshots (Maybe CopyTagsFromSource)
csCopyTagsFromSource = lens _csCopyTagsFromSource (\ s a -> s{_csCopyTagsFromSource = a})

-- | A description propagated to every snapshot specified by the instance.
csDescription :: Lens' CreateSnapshots (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
csDryRun :: Lens' CreateSnapshots (Maybe Bool)
csDryRun = lens _csDryRun (\ s a -> s{_csDryRun = a})

-- | The instance to specify which volumes should be included in the snapshots.
csInstanceSpecification :: Lens' CreateSnapshots InstanceSpecification
csInstanceSpecification = lens _csInstanceSpecification (\ s a -> s{_csInstanceSpecification = a})

instance AWSRequest CreateSnapshots where
        type Rs CreateSnapshots = CreateSnapshotsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateSnapshotsResponse' <$>
                   (x .@? "snapshotSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateSnapshots where

instance NFData CreateSnapshots where

instance ToHeaders CreateSnapshots where
        toHeaders = const mempty

instance ToPath CreateSnapshots where
        toPath = const "/"

instance ToQuery CreateSnapshots where
        toQuery CreateSnapshots'{..}
          = mconcat
              ["Action" =: ("CreateSnapshots" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "OutpostArn" =: _csOutpostARN,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _csTagSpecifications),
               "CopyTagsFromSource" =: _csCopyTagsFromSource,
               "Description" =: _csDescription,
               "DryRun" =: _csDryRun,
               "InstanceSpecification" =: _csInstanceSpecification]

-- | /See:/ 'createSnapshotsResponse' smart constructor.
data CreateSnapshotsResponse = CreateSnapshotsResponse'
  { _crsSnapshots      :: !(Maybe [SnapshotInfo])
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsSnapshots' - List of snapshots.
--
-- * 'crsResponseStatus' - -- | The response status code.
createSnapshotsResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateSnapshotsResponse
createSnapshotsResponse pResponseStatus_ =
  CreateSnapshotsResponse'
    {_crsSnapshots = Nothing, _crsResponseStatus = pResponseStatus_}


-- | List of snapshots.
crsSnapshots :: Lens' CreateSnapshotsResponse [SnapshotInfo]
crsSnapshots = lens _crsSnapshots (\ s a -> s{_crsSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateSnapshotsResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateSnapshotsResponse where
