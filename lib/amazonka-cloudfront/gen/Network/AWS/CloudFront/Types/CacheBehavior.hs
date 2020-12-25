{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehavior
  ( CacheBehavior (..),

    -- * Smart constructor
    mkCacheBehavior,

    -- * Lenses
    cbPathPattern,
    cbTargetOriginId,
    cbViewerProtocolPolicy,
    cbAllowedMethods,
    cbCachePolicyId,
    cbCompress,
    cbDefaultTTL,
    cbFieldLevelEncryptionId,
    cbForwardedValues,
    cbLambdaFunctionAssociations,
    cbMaxTTL,
    cbMinTTL,
    cbOriginRequestPolicyId,
    cbRealtimeLogConfigArn,
    cbSmoothStreaming,
    cbTrustedKeyGroups,
    cbTrustedSigners,
  )
where

import qualified Network.AWS.CloudFront.Types.AllowedMethods as Types
import qualified Network.AWS.CloudFront.Types.ForwardedValues as Types
import qualified Network.AWS.CloudFront.Types.LambdaFunctionAssociations as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.CloudFront.Types.TrustedKeyGroups as Types
import qualified Network.AWS.CloudFront.Types.TrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.ViewerProtocolPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that describes how CloudFront processes requests.
--
-- You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to serve objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.
-- For the current quota (formerly known as limit) on the number of cache behaviors that you can add to a distribution, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> in the /Amazon CloudFront Developer Guide/ .
-- If you don’t want to specify any cache behaviors, include only an empty @CacheBehaviors@ element. Don’t include an empty @CacheBehavior@ element because this is invalid.
-- To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty @CacheBehaviors@ element.
-- To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.
-- For more information about cache behaviors, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { -- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution.
    --
    -- The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior.
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
    pathPattern :: Types.String,
    -- | The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
    targetOriginId :: Types.String,
    -- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:
    --
    --
    --     * @allow-all@ : Viewers can use HTTP or HTTPS.
    --
    --
    --     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.
    --
    --
    --     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).
    --
    --
    -- For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
    viewerProtocolPolicy :: Types.ViewerProtocolPolicy,
    allowedMethods :: Core.Maybe Types.AllowedMethods,
    -- | The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    cachePolicyId :: Core.Maybe Types.String,
    -- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
    compress :: Core.Maybe Core.Bool,
    -- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    defaultTTL :: Core.Maybe Core.Integer,
    -- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
    fieldLevelEncryptionId :: Core.Maybe Types.String,
    -- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    -- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
    -- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
    forwardedValues :: Core.Maybe Types.ForwardedValues,
    -- | A complex type that contains zero or more Lambda function associations for a cache behavior.
    lambdaFunctionAssociations :: Core.Maybe Types.LambdaFunctionAssociations,
    -- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    maxTTL :: Core.Maybe Core.Integer,
    -- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    -- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
    minTTL :: Core.Maybe Core.Integer,
    -- | The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
    originRequestPolicyId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
    realtimeLogConfigArn :: Core.Maybe Types.String,
    -- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
    smoothStreaming :: Core.Maybe Core.Bool,
    -- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
    --
    -- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
    trustedKeyGroups :: Core.Maybe Types.TrustedKeyGroups,
    -- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
    --
    -- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
    -- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
    trustedSigners :: Core.Maybe Types.TrustedSigners
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheBehavior' value with any optional fields omitted.
mkCacheBehavior ::
  -- | 'pathPattern'
  Types.String ->
  -- | 'targetOriginId'
  Types.String ->
  -- | 'viewerProtocolPolicy'
  Types.ViewerProtocolPolicy ->
  CacheBehavior
mkCacheBehavior pathPattern targetOriginId viewerProtocolPolicy =
  CacheBehavior'
    { pathPattern,
      targetOriginId,
      viewerProtocolPolicy,
      allowedMethods = Core.Nothing,
      cachePolicyId = Core.Nothing,
      compress = Core.Nothing,
      defaultTTL = Core.Nothing,
      fieldLevelEncryptionId = Core.Nothing,
      forwardedValues = Core.Nothing,
      lambdaFunctionAssociations = Core.Nothing,
      maxTTL = Core.Nothing,
      minTTL = Core.Nothing,
      originRequestPolicyId = Core.Nothing,
      realtimeLogConfigArn = Core.Nothing,
      smoothStreaming = Core.Nothing,
      trustedKeyGroups = Core.Nothing,
      trustedSigners = Core.Nothing
    }

-- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution.
--
-- The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'pathPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbPathPattern :: Lens.Lens' CacheBehavior Types.String
cbPathPattern = Lens.field @"pathPattern"
{-# DEPRECATED cbPathPattern "Use generic-lens or generic-optics with 'pathPattern' instead." #-}

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
--
-- /Note:/ Consider using 'targetOriginId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTargetOriginId :: Lens.Lens' CacheBehavior Types.String
cbTargetOriginId = Lens.field @"targetOriginId"
{-# DEPRECATED cbTargetOriginId "Use generic-lens or generic-optics with 'targetOriginId' instead." #-}

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:
--
--
--     * @allow-all@ : Viewers can use HTTP or HTTPS.
--
--
--     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.
--
--
--     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).
--
--
-- For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'viewerProtocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbViewerProtocolPolicy :: Lens.Lens' CacheBehavior Types.ViewerProtocolPolicy
cbViewerProtocolPolicy = Lens.field @"viewerProtocolPolicy"
{-# DEPRECATED cbViewerProtocolPolicy "Use generic-lens or generic-optics with 'viewerProtocolPolicy' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAllowedMethods :: Lens.Lens' CacheBehavior (Core.Maybe Types.AllowedMethods)
cbAllowedMethods = Lens.field @"allowedMethods"
{-# DEPRECATED cbAllowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead." #-}

-- | The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCachePolicyId :: Lens.Lens' CacheBehavior (Core.Maybe Types.String)
cbCachePolicyId = Lens.field @"cachePolicyId"
{-# DEPRECATED cbCachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead." #-}

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'compress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCompress :: Lens.Lens' CacheBehavior (Core.Maybe Core.Bool)
cbCompress = Lens.field @"compress"
{-# DEPRECATED cbCompress "Use generic-lens or generic-optics with 'compress' instead." #-}

-- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDefaultTTL :: Lens.Lens' CacheBehavior (Core.Maybe Core.Integer)
cbDefaultTTL = Lens.field @"defaultTTL"
{-# DEPRECATED cbDefaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead." #-}

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
--
-- /Note:/ Consider using 'fieldLevelEncryptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbFieldLevelEncryptionId :: Lens.Lens' CacheBehavior (Core.Maybe Types.String)
cbFieldLevelEncryptionId = Lens.field @"fieldLevelEncryptionId"
{-# DEPRECATED cbFieldLevelEncryptionId "Use generic-lens or generic-optics with 'fieldLevelEncryptionId' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- /Note:/ Consider using 'forwardedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbForwardedValues :: Lens.Lens' CacheBehavior (Core.Maybe Types.ForwardedValues)
cbForwardedValues = Lens.field @"forwardedValues"
{-# DEPRECATED cbForwardedValues "Use generic-lens or generic-optics with 'forwardedValues' instead." #-}

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- /Note:/ Consider using 'lambdaFunctionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLambdaFunctionAssociations :: Lens.Lens' CacheBehavior (Core.Maybe Types.LambdaFunctionAssociations)
cbLambdaFunctionAssociations = Lens.field @"lambdaFunctionAssociations"
{-# DEPRECATED cbLambdaFunctionAssociations "Use generic-lens or generic-optics with 'lambdaFunctionAssociations' instead." #-}

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'maxTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMaxTTL :: Lens.Lens' CacheBehavior (Core.Maybe Core.Integer)
cbMaxTTL = Lens.field @"maxTTL"
{-# DEPRECATED cbMaxTTL "Use generic-lens or generic-optics with 'maxTTL' instead." #-}

-- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
--
-- /Note:/ Consider using 'minTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMinTTL :: Lens.Lens' CacheBehavior (Core.Maybe Core.Integer)
cbMinTTL = Lens.field @"minTTL"
{-# DEPRECATED cbMinTTL "Use generic-lens or generic-optics with 'minTTL' instead." #-}

-- | The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbOriginRequestPolicyId :: Lens.Lens' CacheBehavior (Core.Maybe Types.String)
cbOriginRequestPolicyId = Lens.field @"originRequestPolicyId"
{-# DEPRECATED cbOriginRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead." #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'realtimeLogConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbRealtimeLogConfigArn :: Lens.Lens' CacheBehavior (Core.Maybe Types.String)
cbRealtimeLogConfigArn = Lens.field @"realtimeLogConfigArn"
{-# DEPRECATED cbRealtimeLogConfigArn "Use generic-lens or generic-optics with 'realtimeLogConfigArn' instead." #-}

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- /Note:/ Consider using 'smoothStreaming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSmoothStreaming :: Lens.Lens' CacheBehavior (Core.Maybe Core.Bool)
cbSmoothStreaming = Lens.field @"smoothStreaming"
{-# DEPRECATED cbSmoothStreaming "Use generic-lens or generic-optics with 'smoothStreaming' instead." #-}

-- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTrustedKeyGroups :: Lens.Lens' CacheBehavior (Core.Maybe Types.TrustedKeyGroups)
cbTrustedKeyGroups = Lens.field @"trustedKeyGroups"
{-# DEPRECATED cbTrustedKeyGroups "Use generic-lens or generic-optics with 'trustedKeyGroups' instead." #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTrustedSigners :: Lens.Lens' CacheBehavior (Core.Maybe Types.TrustedSigners)
cbTrustedSigners = Lens.field @"trustedSigners"
{-# DEPRECATED cbTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

instance Core.ToXML CacheBehavior where
  toXML CacheBehavior {..} =
    Core.toXMLNode "PathPattern" pathPattern
      Core.<> Core.toXMLNode "TargetOriginId" targetOriginId
      Core.<> Core.toXMLNode "ViewerProtocolPolicy" viewerProtocolPolicy
      Core.<> Core.toXMLNode "AllowedMethods"
      Core.<$> allowedMethods
      Core.<> Core.toXMLNode "CachePolicyId"
      Core.<$> cachePolicyId
      Core.<> Core.toXMLNode "Compress"
      Core.<$> compress
      Core.<> Core.toXMLNode "DefaultTTL"
      Core.<$> defaultTTL
      Core.<> Core.toXMLNode "FieldLevelEncryptionId"
      Core.<$> fieldLevelEncryptionId
      Core.<> Core.toXMLNode "ForwardedValues"
      Core.<$> forwardedValues
      Core.<> Core.toXMLNode "LambdaFunctionAssociations"
      Core.<$> lambdaFunctionAssociations
      Core.<> Core.toXMLNode "MaxTTL"
      Core.<$> maxTTL
      Core.<> Core.toXMLNode "MinTTL"
      Core.<$> minTTL
      Core.<> Core.toXMLNode "OriginRequestPolicyId"
      Core.<$> originRequestPolicyId
      Core.<> Core.toXMLNode "RealtimeLogConfigArn"
      Core.<$> realtimeLogConfigArn
      Core.<> Core.toXMLNode "SmoothStreaming"
      Core.<$> smoothStreaming
      Core.<> Core.toXMLNode "TrustedKeyGroups"
      Core.<$> trustedKeyGroups
      Core.<> Core.toXMLNode "TrustedSigners"
      Core.<$> trustedSigners

instance Core.FromXML CacheBehavior where
  parseXML x =
    CacheBehavior'
      Core.<$> (x Core..@ "PathPattern")
      Core.<*> (x Core..@ "TargetOriginId")
      Core.<*> (x Core..@ "ViewerProtocolPolicy")
      Core.<*> (x Core..@? "AllowedMethods")
      Core.<*> (x Core..@? "CachePolicyId")
      Core.<*> (x Core..@? "Compress")
      Core.<*> (x Core..@? "DefaultTTL")
      Core.<*> (x Core..@? "FieldLevelEncryptionId")
      Core.<*> (x Core..@? "ForwardedValues")
      Core.<*> (x Core..@? "LambdaFunctionAssociations")
      Core.<*> (x Core..@? "MaxTTL")
      Core.<*> (x Core..@? "MinTTL")
      Core.<*> (x Core..@? "OriginRequestPolicyId")
      Core.<*> (x Core..@? "RealtimeLogConfigArn")
      Core.<*> (x Core..@? "SmoothStreaming")
      Core.<*> (x Core..@? "TrustedKeyGroups")
      Core.<*> (x Core..@? "TrustedSigners")
