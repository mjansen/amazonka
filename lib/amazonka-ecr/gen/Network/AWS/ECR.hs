{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic Container Registry__
--
-- Amazon Elastic Container Registry (Amazon ECR) is a managed container image registry service. Customers can use the familiar Docker CLI, or their preferred client, to push, pull, and manage images. Amazon ECR provides a secure, scalable, and reliable registry for your Docker or Open Container Initiative (OCI) images. Amazon ECR supports private repositories with resource-based permissions using IAM so that specific users or Amazon EC2 instances can access repositories and images.
module Network.AWS.ECR
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ImageTagAlreadyExistsException
    _ImageTagAlreadyExistsException,

    -- ** LayersNotFoundException
    _LayersNotFoundException,

    -- ** ReferencedImagesNotFoundException
    _ReferencedImagesNotFoundException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LayerAlreadyExistsException
    _LayerAlreadyExistsException,

    -- ** ServerException
    _ServerException,

    -- ** LayerInaccessibleException
    _LayerInaccessibleException,

    -- ** InvalidLayerException
    _InvalidLayerException,

    -- ** LayerPartTooSmallException
    _LayerPartTooSmallException,

    -- ** LifecyclePolicyPreviewNotFoundException
    _LifecyclePolicyPreviewNotFoundException,

    -- ** ImageDigestDoesNotMatchException
    _ImageDigestDoesNotMatchException,

    -- ** ImageNotFoundException
    _ImageNotFoundException,

    -- ** ImageAlreadyExistsException
    _ImageAlreadyExistsException,

    -- ** RepositoryNotFoundException
    _RepositoryNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** LifecyclePolicyPreviewInProgressException
    _LifecyclePolicyPreviewInProgressException,

    -- ** UploadNotFoundException
    _UploadNotFoundException,

    -- ** LifecyclePolicyNotFoundException
    _LifecyclePolicyNotFoundException,

    -- ** KmsException
    _KmsException,

    -- ** InvalidLayerPartException
    _InvalidLayerPartException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** RepositoryNotEmptyException
    _RepositoryNotEmptyException,

    -- ** UnsupportedImageTypeException
    _UnsupportedImageTypeException,

    -- ** RepositoryAlreadyExistsException
    _RepositoryAlreadyExistsException,

    -- ** ScanNotFoundException
    _ScanNotFoundException,

    -- ** RepositoryPolicyNotFoundException
    _RepositoryPolicyNotFoundException,

    -- ** EmptyUploadException
    _EmptyUploadException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** LifecyclePolicyPreviewComplete
    mkLifecyclePolicyPreviewComplete,

    -- ** ImageScanComplete
    mkImageScanComplete,

    -- * Operations
    -- $operations

    -- ** GetRepositoryPolicy
    module Network.AWS.ECR.GetRepositoryPolicy,

    -- ** PutImageScanningConfiguration
    module Network.AWS.ECR.PutImageScanningConfiguration,

    -- ** PutLifecyclePolicy
    module Network.AWS.ECR.PutLifecyclePolicy,

    -- ** DeleteLifecyclePolicy
    module Network.AWS.ECR.DeleteLifecyclePolicy,

    -- ** PutImageTagMutability
    module Network.AWS.ECR.PutImageTagMutability,

    -- ** BatchDeleteImage
    module Network.AWS.ECR.BatchDeleteImage,

    -- ** ListTagsForResource
    module Network.AWS.ECR.ListTagsForResource,

    -- ** GetLifecyclePolicyPreview (Paginated)
    module Network.AWS.ECR.GetLifecyclePolicyPreview,

    -- ** BatchCheckLayerAvailability
    module Network.AWS.ECR.BatchCheckLayerAvailability,

    -- ** DeleteRepositoryPolicy
    module Network.AWS.ECR.DeleteRepositoryPolicy,

    -- ** CreateRepository
    module Network.AWS.ECR.CreateRepository,

    -- ** CompleteLayerUpload
    module Network.AWS.ECR.CompleteLayerUpload,

    -- ** DescribeRepositories (Paginated)
    module Network.AWS.ECR.DescribeRepositories,

    -- ** StartLifecyclePolicyPreview
    module Network.AWS.ECR.StartLifecyclePolicyPreview,

    -- ** UploadLayerPart
    module Network.AWS.ECR.UploadLayerPart,

    -- ** BatchGetImage
    module Network.AWS.ECR.BatchGetImage,

    -- ** StartImageScan
    module Network.AWS.ECR.StartImageScan,

    -- ** GetLifecyclePolicy
    module Network.AWS.ECR.GetLifecyclePolicy,

    -- ** TagResource
    module Network.AWS.ECR.TagResource,

    -- ** SetRepositoryPolicy
    module Network.AWS.ECR.SetRepositoryPolicy,

    -- ** DescribeImageScanFindings (Paginated)
    module Network.AWS.ECR.DescribeImageScanFindings,

    -- ** InitiateLayerUpload
    module Network.AWS.ECR.InitiateLayerUpload,

    -- ** UntagResource
    module Network.AWS.ECR.UntagResource,

    -- ** DeleteRepository
    module Network.AWS.ECR.DeleteRepository,

    -- ** PutImage
    module Network.AWS.ECR.PutImage,

    -- ** ListImages (Paginated)
    module Network.AWS.ECR.ListImages,

    -- ** GetAuthorizationToken
    module Network.AWS.ECR.GetAuthorizationToken,

    -- ** GetDownloadUrlForLayer
    module Network.AWS.ECR.GetDownloadUrlForLayer,

    -- ** DescribeImages (Paginated)
    module Network.AWS.ECR.DescribeImages,

    -- * Types

    -- ** LifecyclePolicyPreviewFilter
    LifecyclePolicyPreviewFilter (..),
    mkLifecyclePolicyPreviewFilter,
    lppfTagStatus,

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aKey,
    aValue,

    -- ** ImageIdentifier
    ImageIdentifier (..),
    mkImageIdentifier,
    iiImageDigest,
    iiImageTag,

    -- ** MediaType
    MediaType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** ImageFailure
    ImageFailure (..),
    mkImageFailure,
    ifFailureCode,
    ifFailureReason,
    ifImageId,

    -- ** Image
    Image (..),
    mkImage,
    iImageId,
    iImageManifest,
    iImageManifestMediaType,
    iRegistryId,
    iRepositoryName,

    -- ** LayerFailureReason
    LayerFailureReason (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** Repository
    Repository (..),
    mkRepository,
    rCreatedAt,
    rEncryptionConfiguration,
    rImageScanningConfiguration,
    rImageTagMutability,
    rRegistryId,
    rRepositoryArn,
    rRepositoryName,
    rRepositoryUri,

    -- ** KmsKey
    KmsKey (..),

    -- ** Arn
    Arn (..),

    -- ** ScanStatusDescription
    ScanStatusDescription (..),

    -- ** RegistryId
    RegistryId (..),

    -- ** AuthorizationData
    AuthorizationData (..),
    mkAuthorizationData,
    adAuthorizationToken,
    adExpiresAt,
    adProxyEndpoint,

    -- ** LayerDigest
    LayerDigest (..),

    -- ** Url
    Url (..),

    -- ** BatchedOperationLayerDigest
    BatchedOperationLayerDigest (..),

    -- ** LifecyclePolicyText
    LifecyclePolicyText (..),

    -- ** ImageDetail
    ImageDetail (..),
    mkImageDetail,
    idArtifactMediaType,
    idImageDigest,
    idImageManifestMediaType,
    idImagePushedAt,
    idImageScanFindingsSummary,
    idImageScanStatus,
    idImageSizeInBytes,
    idImageTags,
    idRegistryId,
    idRepositoryName,

    -- ** ImageScanFindings
    ImageScanFindings (..),
    mkImageScanFindings,
    isfFindingSeverityCounts,
    isfFindings,
    isfImageScanCompletedAt,
    isfVulnerabilitySourceUpdatedAt,

    -- ** ImageScanningConfiguration
    ImageScanningConfiguration (..),
    mkImageScanningConfiguration,
    iscScanOnPush,

    -- ** ListImagesFilter
    ListImagesFilter (..),
    mkListImagesFilter,
    lifTagStatus,

    -- ** ImageActionType
    ImageActionType (..),

    -- ** DescribeImagesFilter
    DescribeImagesFilter (..),
    mkDescribeImagesFilter,
    difTagStatus,

    -- ** ImageScanStatus
    ImageScanStatus (..),
    mkImageScanStatus,
    issDescription,
    issStatus,

    -- ** NextToken
    NextToken (..),

    -- ** ImageDigest
    ImageDigest (..),

    -- ** ProxyEndpoint
    ProxyEndpoint (..),

    -- ** RepositoryPolicyText
    RepositoryPolicyText (..),

    -- ** EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecEncryptionType,
    ecKmsKey,

    -- ** LayerAvailability
    LayerAvailability (..),

    -- ** ImageFailureCode
    ImageFailureCode (..),

    -- ** FindingName
    FindingName (..),

    -- ** ImageScanFindingsSummary
    ImageScanFindingsSummary (..),
    mkImageScanFindingsSummary,
    isfsFindingSeverityCounts,
    isfsImageScanCompletedAt,
    isfsVulnerabilitySourceUpdatedAt,

    -- ** LifecyclePolicyPreviewResult
    LifecyclePolicyPreviewResult (..),
    mkLifecyclePolicyPreviewResult,
    lpprAction,
    lpprAppliedRulePriority,
    lpprImageDigest,
    lpprImagePushedAt,
    lpprImageTags,

    -- ** ImageScanFinding
    ImageScanFinding (..),
    mkImageScanFinding,
    isfAttributes,
    isfDescription,
    isfName,
    isfSeverity,
    isfUri,

    -- ** RepositoryName
    RepositoryName (..),

    -- ** ImageTag
    ImageTag (..),

    -- ** TagKey
    TagKey (..),

    -- ** ImageManifest
    ImageManifest (..),

    -- ** LifecyclePolicyPreviewSummary
    LifecyclePolicyPreviewSummary (..),
    mkLifecyclePolicyPreviewSummary,
    lppsExpiringImageTotalCount,

    -- ** LifecyclePolicyPreviewStatus
    LifecyclePolicyPreviewStatus (..),

    -- ** ImageTagMutability
    ImageTagMutability (..),

    -- ** Layer
    Layer (..),
    mkLayer,
    lLayerAvailability,
    lLayerDigest,
    lLayerSize,
    lMediaType,

    -- ** LayerFailure
    LayerFailure (..),
    mkLayerFailure,
    lfFailureCode,
    lfFailureReason,
    lfLayerDigest,

    -- ** ScanStatus
    ScanStatus (..),

    -- ** TagStatus
    TagStatus (..),

    -- ** LifecyclePolicyRuleAction
    LifecyclePolicyRuleAction (..),
    mkLifecyclePolicyRuleAction,
    lpraType,

    -- ** UploadId
    UploadId (..),

    -- ** LayerFailureCode
    LayerFailureCode (..),

    -- ** FindingSeverity
    FindingSeverity (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** PolicyText
    PolicyText (..),

    -- ** RepositoryArn
    RepositoryArn (..),

    -- ** RepositoryUri
    RepositoryUri (..),

    -- ** AuthorizationToken
    AuthorizationToken (..),

    -- ** Description
    Description (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.ECR.BatchCheckLayerAvailability
import Network.AWS.ECR.BatchDeleteImage
import Network.AWS.ECR.BatchGetImage
import Network.AWS.ECR.CompleteLayerUpload
import Network.AWS.ECR.CreateRepository
import Network.AWS.ECR.DeleteLifecyclePolicy
import Network.AWS.ECR.DeleteRepository
import Network.AWS.ECR.DeleteRepositoryPolicy
import Network.AWS.ECR.DescribeImageScanFindings
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.DescribeRepositories
import Network.AWS.ECR.GetAuthorizationToken
import Network.AWS.ECR.GetDownloadUrlForLayer
import Network.AWS.ECR.GetLifecyclePolicy
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.GetRepositoryPolicy
import Network.AWS.ECR.InitiateLayerUpload
import Network.AWS.ECR.ListImages
import Network.AWS.ECR.ListTagsForResource
import Network.AWS.ECR.PutImage
import Network.AWS.ECR.PutImageScanningConfiguration
import Network.AWS.ECR.PutImageTagMutability
import Network.AWS.ECR.PutLifecyclePolicy
import Network.AWS.ECR.SetRepositoryPolicy
import Network.AWS.ECR.StartImageScan
import Network.AWS.ECR.StartLifecyclePolicyPreview
import Network.AWS.ECR.TagResource
import Network.AWS.ECR.Types
import Network.AWS.ECR.UntagResource
import Network.AWS.ECR.UploadLayerPart
import Network.AWS.ECR.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ECR'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
