{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an object's retention settings. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
--
-- This action is not supported by Amazon S3 on Outposts.
module Network.AWS.S3.GetObjectRetention
  ( -- * Creating a request
    GetObjectRetention (..),
    mkGetObjectRetention,

    -- ** Request lenses
    gorBucket,
    gorKey,
    gorExpectedBucketOwner,
    gorRequestPayer,
    gorVersionId,

    -- * Destructuring the response
    GetObjectRetentionResponse (..),
    mkGetObjectRetentionResponse,

    -- ** Response lenses
    gorrrsRetention,
    gorrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObjectRetention' smart constructor.
data GetObjectRetention = GetObjectRetention'
  { -- | The bucket name containing the object whose retention settings you want to retrieve.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | The key name for the object whose retention settings you want to retrieve.
    key :: Types.Key,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | The version ID for the object whose retention settings you want to retrieve.
    versionId :: Core.Maybe Types.ObjectVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectRetention' value with any optional fields omitted.
mkGetObjectRetention ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  GetObjectRetention
mkGetObjectRetention bucket key =
  GetObjectRetention'
    { bucket,
      key,
      expectedBucketOwner = Core.Nothing,
      requestPayer = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The bucket name containing the object whose retention settings you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorBucket :: Lens.Lens' GetObjectRetention Types.BucketName
gorBucket = Lens.field @"bucket"
{-# DEPRECATED gorBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key name for the object whose retention settings you want to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorKey :: Lens.Lens' GetObjectRetention Types.Key
gorKey = Lens.field @"key"
{-# DEPRECATED gorKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorExpectedBucketOwner :: Lens.Lens' GetObjectRetention (Core.Maybe Types.ExpectedBucketOwner)
gorExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gorExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorRequestPayer :: Lens.Lens' GetObjectRetention (Core.Maybe Types.RequestPayer)
gorRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED gorRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The version ID for the object whose retention settings you want to retrieve.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorVersionId :: Lens.Lens' GetObjectRetention (Core.Maybe Types.ObjectVersionId)
gorVersionId = Lens.field @"versionId"
{-# DEPRECATED gorVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest GetObjectRetention where
  type Rs GetObjectRetention = GetObjectRetentionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery =
          Core.toQueryValue "versionId" Core.<$> versionId
            Core.<> (Core.pure ("retention", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectRetentionResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetObjectRetentionResponse' smart constructor.
data GetObjectRetentionResponse = GetObjectRetentionResponse'
  { -- | The container element for an object's retention settings.
    retention :: Core.Maybe Types.ObjectLockRetention,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetObjectRetentionResponse' value with any optional fields omitted.
mkGetObjectRetentionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetObjectRetentionResponse
mkGetObjectRetentionResponse responseStatus =
  GetObjectRetentionResponse'
    { retention = Core.Nothing,
      responseStatus
    }

-- | The container element for an object's retention settings.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrrsRetention :: Lens.Lens' GetObjectRetentionResponse (Core.Maybe Types.ObjectLockRetention)
gorrrsRetention = Lens.field @"retention"
{-# DEPRECATED gorrrsRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrrsResponseStatus :: Lens.Lens' GetObjectRetentionResponse Core.Int
gorrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gorrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
