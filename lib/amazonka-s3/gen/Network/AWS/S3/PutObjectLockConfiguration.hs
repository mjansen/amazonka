{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Lock configuration on the specified bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectLockConfiguration
  ( -- * Creating a request
    PutObjectLockConfiguration (..),
    mkPutObjectLockConfiguration,

    -- ** Request lenses
    polcBucket,
    polcContentMD5,
    polcExpectedBucketOwner,
    polcObjectLockConfiguration,
    polcRequestPayer,
    polcToken,

    -- * Destructuring the response
    PutObjectLockConfigurationResponse (..),
    mkPutObjectLockConfigurationResponse,

    -- ** Response lenses
    polcrrsRequestCharged,
    polcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectLockConfiguration' smart constructor.
data PutObjectLockConfiguration = PutObjectLockConfiguration'
  { -- | The bucket whose Object Lock configuration you want to create or replace.
    bucket :: Types.BucketName,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | The Object Lock configuration that you want to apply to the specified bucket.
    objectLockConfiguration :: Core.Maybe Types.ObjectLockConfiguration,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Core.Maybe Types.ObjectLockToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectLockConfiguration' value with any optional fields omitted.
mkPutObjectLockConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  PutObjectLockConfiguration
mkPutObjectLockConfiguration bucket =
  PutObjectLockConfiguration'
    { bucket,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      objectLockConfiguration = Core.Nothing,
      requestPayer = Core.Nothing,
      token = Core.Nothing
    }

-- | The bucket whose Object Lock configuration you want to create or replace.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcBucket :: Lens.Lens' PutObjectLockConfiguration Types.BucketName
polcBucket = Lens.field @"bucket"
{-# DEPRECATED polcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcContentMD5 :: Lens.Lens' PutObjectLockConfiguration (Core.Maybe Types.ContentMD5)
polcContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED polcContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcExpectedBucketOwner :: Lens.Lens' PutObjectLockConfiguration (Core.Maybe Types.ExpectedBucketOwner)
polcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED polcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The Object Lock configuration that you want to apply to the specified bucket.
--
-- /Note:/ Consider using 'objectLockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcObjectLockConfiguration :: Lens.Lens' PutObjectLockConfiguration (Core.Maybe Types.ObjectLockConfiguration)
polcObjectLockConfiguration = Lens.field @"objectLockConfiguration"
{-# DEPRECATED polcObjectLockConfiguration "Use generic-lens or generic-optics with 'objectLockConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcRequestPayer :: Lens.Lens' PutObjectLockConfiguration (Core.Maybe Types.RequestPayer)
polcRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED polcRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | A token to allow Object Lock to be enabled for an existing bucket.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcToken :: Lens.Lens' PutObjectLockConfiguration (Core.Maybe Types.ObjectLockToken)
polcToken = Lens.field @"token"
{-# DEPRECATED polcToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest PutObjectLockConfiguration where
  type
    Rs PutObjectLockConfiguration =
      PutObjectLockConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("object-lock", ""),
        Core._rqHeaders =
          Core.toHeaders "Content-MD5" contentMD5
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner)
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer)
            Core.<> (Core.toHeaders "x-amz-bucket-object-lock-token" token),
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectLockConfigurationResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutObjectLockConfigurationResponse' smart constructor.
data PutObjectLockConfigurationResponse = PutObjectLockConfigurationResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectLockConfigurationResponse' value with any optional fields omitted.
mkPutObjectLockConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutObjectLockConfigurationResponse
mkPutObjectLockConfigurationResponse responseStatus =
  PutObjectLockConfigurationResponse'
    { requestCharged =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcrrsRequestCharged :: Lens.Lens' PutObjectLockConfigurationResponse (Core.Maybe Types.RequestCharged)
polcrrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED polcrrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polcrrsResponseStatus :: Lens.Lens' PutObjectLockConfigurationResponse Core.Int
polcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED polcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
