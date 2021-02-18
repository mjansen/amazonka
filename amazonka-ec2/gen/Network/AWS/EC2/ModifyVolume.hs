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
-- Module      : Network.AWS.EC2.ModifyVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can modify several parameters of an existing EBS volume, including volume size, volume type, and IOPS capacity. If your EBS volume is attached to a current-generation EC2 instance type, you might be able to apply these changes without stopping the instance or detaching the volume from it. For more information about modifying an EBS volume running Linux, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Linux> . For more information about modifying an EBS volume running Windows, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Windows> .
--
--
-- When you complete a resize operation on your volume, you need to extend the volume's file-system size to take advantage of the new storage capacity. For information about extending a Linux file system, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux Extending a Linux file system> . For information about extending a Windows file system, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows Extending a Windows file system> .
--
-- You can use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide> . You can also track the status of a modification using 'DescribeVolumesModifications' . For information about tracking status changes using either method, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring volume modifications> .
--
-- With previous-generation instance types, resizing an EBS volume might require detaching and reattaching the volume or stopping and restarting the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes> (Linux) or <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-modify-volume.html Amazon EBS Elastic Volumes> (Windows).
--
-- If you reach the maximum volume modification rate per volume limit, you will need to wait at least six hours before applying further modifications to the affected EBS volume.
--
module Network.AWS.EC2.ModifyVolume
    (
    -- * Creating a Request
      modifyVolume
    , ModifyVolume
    -- * Request Lenses
    , mvMultiAttachEnabled
    , mvThroughput
    , mvSize
    , mvIOPS
    , mvVolumeType
    , mvDryRun
    , mvVolumeId

    -- * Destructuring the Response
    , modifyVolumeResponse
    , ModifyVolumeResponse
    -- * Response Lenses
    , mvrsVolumeModification
    , mvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { _mvMultiAttachEnabled :: !(Maybe Bool)
  , _mvThroughput         :: !(Maybe Int)
  , _mvSize               :: !(Maybe Int)
  , _mvIOPS               :: !(Maybe Int)
  , _mvVolumeType         :: !(Maybe VolumeType)
  , _mvDryRun             :: !(Maybe Bool)
  , _mvVolumeId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvMultiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. This parameter is supported with @io1@ and @io2@ volumes only. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'mvThroughput' - The target throughput of the volume, in MiB/s. This parameter is valid only for @gp3@ volumes. The maximum value is 1,000. Default: If no throughput value is specified, the existing value is retained. Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- * 'mvSize' - The target size of the volume, in GiB. The target volume size must be greater than or equal to the existing size of the volume. The following are the supported volumes sizes for each volume type:     * @gp2@ and @gp3@ : 1-16,384     * @io1@ and @io2@ : 4-16,384     * @st1@ and @sc1@ : 125-16,384     * @standard@ : 1-1,024 Default: If no size is specified, the existing size is retained.
--
-- * 'mvIOPS' - The target IOPS rate of the volume. This parameter is valid only for @gp3@ , @io1@ , and @io2@ volumes. The following are the supported values for each volume type:     * @gp3@ : 3,000-16,000 IOPS     * @io1@ : 100-64,000 IOPS     * @io2@ : 100-64,000 IOPS Default: If no IOPS value is specified, the existing value is retained.
--
-- * 'mvVolumeType' - The target EBS volume type of the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: If no type is specified, the existing type is retained.
--
-- * 'mvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvVolumeId' - The ID of the volume.
modifyVolume
    :: Text -- ^ 'mvVolumeId'
    -> ModifyVolume
modifyVolume pVolumeId_ =
  ModifyVolume'
    { _mvMultiAttachEnabled = Nothing
    , _mvThroughput = Nothing
    , _mvSize = Nothing
    , _mvIOPS = Nothing
    , _mvVolumeType = Nothing
    , _mvDryRun = Nothing
    , _mvVolumeId = pVolumeId_
    }


-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. This parameter is supported with @io1@ and @io2@ volumes only. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
mvMultiAttachEnabled :: Lens' ModifyVolume (Maybe Bool)
mvMultiAttachEnabled = lens _mvMultiAttachEnabled (\ s a -> s{_mvMultiAttachEnabled = a})

-- | The target throughput of the volume, in MiB/s. This parameter is valid only for @gp3@ volumes. The maximum value is 1,000. Default: If no throughput value is specified, the existing value is retained. Valid Range: Minimum value of 125. Maximum value of 1000.
mvThroughput :: Lens' ModifyVolume (Maybe Int)
mvThroughput = lens _mvThroughput (\ s a -> s{_mvThroughput = a})

-- | The target size of the volume, in GiB. The target volume size must be greater than or equal to the existing size of the volume. The following are the supported volumes sizes for each volume type:     * @gp2@ and @gp3@ : 1-16,384     * @io1@ and @io2@ : 4-16,384     * @st1@ and @sc1@ : 125-16,384     * @standard@ : 1-1,024 Default: If no size is specified, the existing size is retained.
mvSize :: Lens' ModifyVolume (Maybe Int)
mvSize = lens _mvSize (\ s a -> s{_mvSize = a})

-- | The target IOPS rate of the volume. This parameter is valid only for @gp3@ , @io1@ , and @io2@ volumes. The following are the supported values for each volume type:     * @gp3@ : 3,000-16,000 IOPS     * @io1@ : 100-64,000 IOPS     * @io2@ : 100-64,000 IOPS Default: If no IOPS value is specified, the existing value is retained.
mvIOPS :: Lens' ModifyVolume (Maybe Int)
mvIOPS = lens _mvIOPS (\ s a -> s{_mvIOPS = a})

-- | The target EBS volume type of the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Default: If no type is specified, the existing type is retained.
mvVolumeType :: Lens' ModifyVolume (Maybe VolumeType)
mvVolumeType = lens _mvVolumeType (\ s a -> s{_mvVolumeType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvDryRun :: Lens' ModifyVolume (Maybe Bool)
mvDryRun = lens _mvDryRun (\ s a -> s{_mvDryRun = a})

-- | The ID of the volume.
mvVolumeId :: Lens' ModifyVolume Text
mvVolumeId = lens _mvVolumeId (\ s a -> s{_mvVolumeId = a})

instance AWSRequest ModifyVolume where
        type Rs ModifyVolume = ModifyVolumeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVolumeResponse' <$>
                   (x .@? "volumeModification") <*> (pure (fromEnum s)))

instance Hashable ModifyVolume where

instance NFData ModifyVolume where

instance ToHeaders ModifyVolume where
        toHeaders = const mempty

instance ToPath ModifyVolume where
        toPath = const "/"

instance ToQuery ModifyVolume where
        toQuery ModifyVolume'{..}
          = mconcat
              ["Action" =: ("ModifyVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "MultiAttachEnabled" =: _mvMultiAttachEnabled,
               "Throughput" =: _mvThroughput, "Size" =: _mvSize,
               "Iops" =: _mvIOPS, "VolumeType" =: _mvVolumeType,
               "DryRun" =: _mvDryRun, "VolumeId" =: _mvVolumeId]

-- | /See:/ 'modifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { _mvrsVolumeModification :: !(Maybe VolumeModification)
  , _mvrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvrsVolumeModification' - Information about the volume modification.
--
-- * 'mvrsResponseStatus' - -- | The response status code.
modifyVolumeResponse
    :: Int -- ^ 'mvrsResponseStatus'
    -> ModifyVolumeResponse
modifyVolumeResponse pResponseStatus_ =
  ModifyVolumeResponse'
    {_mvrsVolumeModification = Nothing, _mvrsResponseStatus = pResponseStatus_}


-- | Information about the volume modification.
mvrsVolumeModification :: Lens' ModifyVolumeResponse (Maybe VolumeModification)
mvrsVolumeModification = lens _mvrsVolumeModification (\ s a -> s{_mvrsVolumeModification = a})

-- | -- | The response status code.
mvrsResponseStatus :: Lens' ModifyVolumeResponse Int
mvrsResponseStatus = lens _mvrsResponseStatus (\ s a -> s{_mvrsResponseStatus = a})

instance NFData ModifyVolumeResponse where
