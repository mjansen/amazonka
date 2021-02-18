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
-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an EBS volume and stores it in Amazon S3. You can use snapshots for backups, to make copies of EBS volumes, and to save data before shutting down an instance.
--
--
-- You can create snapshots of volumes in a Region and volumes on an Outpost. If you create a snapshot of a volume in a Region, the snapshot must be stored in the same Region as the volume. If you create a snapshot of a volume on an Outpost, the snapshot can be stored on the same Outpost as the volume, or in the Region for that Outpost.
--
-- When a snapshot is created, any AWS Marketplace product codes that are associated with the source volume are propagated to the snapshot.
--
-- You can take a snapshot of an attached volume that is in use. However, snapshots only capture data that has been written to your EBS volume at the time the snapshot command is issued; this might exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the volume long enough to take a snapshot, your snapshot should be complete. However, if you cannot pause all file writes to the volume, you should unmount the volume from within the instance, issue the snapshot command, and then remount the volume to ensure a consistent and complete snapshot. You may remount and use your volume while the snapshot status is @pending@ .
--
-- To create a snapshot for EBS volumes that serve as root devices, you should stop the instance before taking the snapshot.
--
-- Snapshots that are taken from encrypted volumes are automatically encrypted. Volumes that are created from encrypted snapshots are also automatically encrypted. Your encrypted volumes and any associated snapshots always remain protected.
--
-- You can tag your snapshots during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateSnapshot
    (
    -- * Creating a Request
      createSnapshot
    , CreateSnapshot
    -- * Request Lenses
    , ccOutpostARN
    , ccTagSpecifications
    , ccDescription
    , ccDryRun
    , ccVolumeId

    -- * Destructuring the Response
    , snapshot
    , Snapshot
    -- * Response Lenses
    , sStateMessage
    , sOwnerAlias
    , sDataEncryptionKeyId
    , sOutpostARN
    , sKMSKeyId
    , sTags
    , sSnapshotId
    , sOwnerId
    , sVolumeId
    , sVolumeSize
    , sDescription
    , sStartTime
    , sProgress
    , sState
    , sEncrypted
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { _ccOutpostARN        :: !(Maybe Text)
  , _ccTagSpecifications :: !(Maybe [TagSpecification])
  , _ccDescription       :: !(Maybe Text)
  , _ccDryRun            :: !(Maybe Bool)
  , _ccVolumeId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccOutpostARN' - The Amazon Resource Name (ARN) of the AWS Outpost on which to create a local snapshot.     * To create a snapshot of a volume in a Region, omit this parameter. The snapshot is created in the same Region as the volume.     * To create a snapshot of a volume on an Outpost and store the snapshot in the Region, omit this parameter. The snapshot is created in the Region for the Outpost.     * To create a snapshot of a volume on an Outpost and store the snapshot on an Outpost, specify the ARN of the destination Outpost. The snapshot must be created on the same Outpost as the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-snapshot Creating local snapshots from volumes on an Outpost> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ccTagSpecifications' - The tags to apply to the snapshot during creation.
--
-- * 'ccDescription' - A description for the snapshot.
--
-- * 'ccDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ccVolumeId' - The ID of the EBS volume.
createSnapshot
    :: Text -- ^ 'ccVolumeId'
    -> CreateSnapshot
createSnapshot pVolumeId_ =
  CreateSnapshot'
    { _ccOutpostARN = Nothing
    , _ccTagSpecifications = Nothing
    , _ccDescription = Nothing
    , _ccDryRun = Nothing
    , _ccVolumeId = pVolumeId_
    }


-- | The Amazon Resource Name (ARN) of the AWS Outpost on which to create a local snapshot.     * To create a snapshot of a volume in a Region, omit this parameter. The snapshot is created in the same Region as the volume.     * To create a snapshot of a volume on an Outpost and store the snapshot in the Region, omit this parameter. The snapshot is created in the Region for the Outpost.     * To create a snapshot of a volume on an Outpost and store the snapshot on an Outpost, specify the ARN of the destination Outpost. The snapshot must be created on the same Outpost as the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-snapshot Creating local snapshots from volumes on an Outpost> in the /Amazon Elastic Compute Cloud User Guide/ .
ccOutpostARN :: Lens' CreateSnapshot (Maybe Text)
ccOutpostARN = lens _ccOutpostARN (\ s a -> s{_ccOutpostARN = a})

-- | The tags to apply to the snapshot during creation.
ccTagSpecifications :: Lens' CreateSnapshot [TagSpecification]
ccTagSpecifications = lens _ccTagSpecifications (\ s a -> s{_ccTagSpecifications = a}) . _Default . _Coerce

-- | A description for the snapshot.
ccDescription :: Lens' CreateSnapshot (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ccDryRun :: Lens' CreateSnapshot (Maybe Bool)
ccDryRun = lens _ccDryRun (\ s a -> s{_ccDryRun = a})

-- | The ID of the EBS volume.
ccVolumeId :: Lens' CreateSnapshot Text
ccVolumeId = lens _ccVolumeId (\ s a -> s{_ccVolumeId = a})

instance AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = Snapshot
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable CreateSnapshot where

instance NFData CreateSnapshot where

instance ToHeaders CreateSnapshot where
        toHeaders = const mempty

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery CreateSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateSnapshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "OutpostArn" =: _ccOutpostARN,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _ccTagSpecifications),
               "Description" =: _ccDescription,
               "DryRun" =: _ccDryRun, "VolumeId" =: _ccVolumeId]
