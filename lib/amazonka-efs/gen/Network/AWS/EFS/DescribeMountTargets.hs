{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of all the current mount targets, or a specific mount target, for a file system. When requesting all of the current mount targets, the order of mount targets returned in the response is unspecified.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeMountTargets@ action, on either the file system ID that you specify in @FileSystemId@ , or on the file system of the mount target that you specify in @MountTargetId@ .
--
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeMountTargets
  ( -- * Creating a Request
    describeMountTargets,
    DescribeMountTargets,

    -- * Request Lenses
    dmtAccessPointId,
    dmtFileSystemId,
    dmtMarker,
    dmtMaxItems,
    dmtMountTargetId,

    -- * Destructuring the Response
    describeMountTargetsResponse,
    DescribeMountTargetsResponse,

    -- * Response Lenses
    dmtrsMountTargets,
    dmtrsMarker,
    dmtrsNextMarker,
    dmtrsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeMountTargets' smart constructor.
data DescribeMountTargets = DescribeMountTargets'
  { _dmtAccessPointId ::
      !(Maybe Text),
    _dmtFileSystemId :: !(Maybe Text),
    _dmtMarker :: !(Maybe Text),
    _dmtMaxItems :: !(Maybe Nat),
    _dmtMountTargetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMountTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtAccessPointId' - (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
--
-- * 'dmtFileSystemId' - (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
--
-- * 'dmtMarker' - (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
--
-- * 'dmtMaxItems' - (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
--
-- * 'dmtMountTargetId' - (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
describeMountTargets ::
  DescribeMountTargets
describeMountTargets =
  DescribeMountTargets'
    { _dmtAccessPointId = Nothing,
      _dmtFileSystemId = Nothing,
      _dmtMarker = Nothing,
      _dmtMaxItems = Nothing,
      _dmtMountTargetId = Nothing
    }

-- | (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
dmtAccessPointId :: Lens' DescribeMountTargets (Maybe Text)
dmtAccessPointId = lens _dmtAccessPointId (\s a -> s {_dmtAccessPointId = a})

-- | (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
dmtFileSystemId :: Lens' DescribeMountTargets (Maybe Text)
dmtFileSystemId = lens _dmtFileSystemId (\s a -> s {_dmtFileSystemId = a})

-- | (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
dmtMarker :: Lens' DescribeMountTargets (Maybe Text)
dmtMarker = lens _dmtMarker (\s a -> s {_dmtMarker = a})

-- | (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
dmtMaxItems :: Lens' DescribeMountTargets (Maybe Natural)
dmtMaxItems = lens _dmtMaxItems (\s a -> s {_dmtMaxItems = a}) . mapping _Nat

-- | (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
dmtMountTargetId :: Lens' DescribeMountTargets (Maybe Text)
dmtMountTargetId = lens _dmtMountTargetId (\s a -> s {_dmtMountTargetId = a})

instance AWSPager DescribeMountTargets where
  page rq rs
    | stop (rs ^. dmtrsNextMarker) = Nothing
    | stop (rs ^. dmtrsMountTargets) = Nothing
    | otherwise = Just $ rq & dmtMarker .~ rs ^. dmtrsNextMarker

instance AWSRequest DescribeMountTargets where
  type Rs DescribeMountTargets = DescribeMountTargetsResponse
  request = get efs
  response =
    receiveJSON
      ( \s h x ->
          DescribeMountTargetsResponse'
            <$> (x .?> "MountTargets" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (x .?> "NextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeMountTargets

instance NFData DescribeMountTargets

instance ToHeaders DescribeMountTargets where
  toHeaders = const mempty

instance ToPath DescribeMountTargets where
  toPath = const "/2015-02-01/mount-targets"

instance ToQuery DescribeMountTargets where
  toQuery DescribeMountTargets' {..} =
    mconcat
      [ "AccessPointId" =: _dmtAccessPointId,
        "FileSystemId" =: _dmtFileSystemId,
        "Marker" =: _dmtMarker,
        "MaxItems" =: _dmtMaxItems,
        "MountTargetId" =: _dmtMountTargetId
      ]

-- |
--
--
--
-- /See:/ 'describeMountTargetsResponse' smart constructor.
data DescribeMountTargetsResponse = DescribeMountTargetsResponse'
  { _dmtrsMountTargets ::
      !(Maybe [MountTargetDescription]),
    _dmtrsMarker :: !(Maybe Text),
    _dmtrsNextMarker :: !(Maybe Text),
    _dmtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMountTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtrsMountTargets' - Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
--
-- * 'dmtrsMarker' - If the request included the @Marker@ , the response returns that value in this field.
--
-- * 'dmtrsNextMarker' - If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
--
-- * 'dmtrsResponseStatus' - -- | The response status code.
describeMountTargetsResponse ::
  -- | 'dmtrsResponseStatus'
  Int ->
  DescribeMountTargetsResponse
describeMountTargetsResponse pResponseStatus_ =
  DescribeMountTargetsResponse'
    { _dmtrsMountTargets = Nothing,
      _dmtrsMarker = Nothing,
      _dmtrsNextMarker = Nothing,
      _dmtrsResponseStatus = pResponseStatus_
    }

-- | Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
dmtrsMountTargets :: Lens' DescribeMountTargetsResponse [MountTargetDescription]
dmtrsMountTargets = lens _dmtrsMountTargets (\s a -> s {_dmtrsMountTargets = a}) . _Default . _Coerce

-- | If the request included the @Marker@ , the response returns that value in this field.
dmtrsMarker :: Lens' DescribeMountTargetsResponse (Maybe Text)
dmtrsMarker = lens _dmtrsMarker (\s a -> s {_dmtrsMarker = a})

-- | If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
dmtrsNextMarker :: Lens' DescribeMountTargetsResponse (Maybe Text)
dmtrsNextMarker = lens _dmtrsNextMarker (\s a -> s {_dmtrsNextMarker = a})

-- | -- | The response status code.
dmtrsResponseStatus :: Lens' DescribeMountTargetsResponse Int
dmtrsResponseStatus = lens _dmtrsResponseStatus (\s a -> s {_dmtrsResponseStatus = a})

instance NFData DescribeMountTargetsResponse