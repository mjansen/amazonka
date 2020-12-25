{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Lambda__
--
-- __Overview__
-- This is the /AWS Lambda API Reference/ . The AWS Lambda Developer Guide provides additional information. For the service overview, see <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html What is AWS Lambda> , and for information about how the service works, see <https://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works> in the __AWS Lambda Developer Guide__ .
module Network.AWS.Lambda
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** EC2ThrottledException
    _EC2ThrottledException,

    -- ** EFSMountConnectivityException
    _EFSMountConnectivityException,

    -- ** InvalidRuntimeException
    _InvalidRuntimeException,

    -- ** EFSMountFailureException
    _EFSMountFailureException,

    -- ** PolicyLengthExceededException
    _PolicyLengthExceededException,

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** EC2AccessDeniedException
    _EC2AccessDeniedException,

    -- ** InvalidSubnetIDException
    _InvalidSubnetIDException,

    -- ** CodeVerificationFailedException
    _CodeVerificationFailedException,

    -- ** UnsupportedMediaTypeException
    _UnsupportedMediaTypeException,

    -- ** InvalidRequestContentException
    _InvalidRequestContentException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** ENILimitReachedException
    _ENILimitReachedException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** RequestTooLargeException
    _RequestTooLargeException,

    -- ** InvalidCodeSignatureException
    _InvalidCodeSignatureException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InvalidSecurityGroupIDException
    _InvalidSecurityGroupIDException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- ** SubnetIPAddressLimitReachedException
    _SubnetIPAddressLimitReachedException,

    -- ** ServiceException
    _ServiceException,

    -- ** CodeStorageExceededException
    _CodeStorageExceededException,

    -- ** CodeSigningConfigNotFoundException
    _CodeSigningConfigNotFoundException,

    -- ** InvalidZipFileException
    _InvalidZipFileException,

    -- ** ProvisionedConcurrencyConfigNotFoundException
    _ProvisionedConcurrencyConfigNotFoundException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** EC2UnexpectedException
    _EC2UnexpectedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** EFSIOException
    _EFSIOException,

    -- ** EFSMountTimeoutException
    _EFSMountTimeoutException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** FunctionExists
    mkFunctionExists,

    -- ** FunctionActive
    mkFunctionActive,

    -- ** FunctionUpdated
    mkFunctionUpdated,

    -- * Operations
    -- $operations

    -- ** GetFunctionConfiguration
    module Network.AWS.Lambda.GetFunctionConfiguration,

    -- ** DeleteEventSourceMapping
    module Network.AWS.Lambda.DeleteEventSourceMapping,

    -- ** UpdateEventSourceMapping
    module Network.AWS.Lambda.UpdateEventSourceMapping,

    -- ** GetLayerVersion
    module Network.AWS.Lambda.GetLayerVersion,

    -- ** DeleteFunctionCodeSigningConfig
    module Network.AWS.Lambda.DeleteFunctionCodeSigningConfig,

    -- ** PutFunctionCodeSigningConfig
    module Network.AWS.Lambda.PutFunctionCodeSigningConfig,

    -- ** CreateAlias
    module Network.AWS.Lambda.CreateAlias,

    -- ** ListVersionsByFunction (Paginated)
    module Network.AWS.Lambda.ListVersionsByFunction,

    -- ** ListAliases (Paginated)
    module Network.AWS.Lambda.ListAliases,

    -- ** DeleteCodeSigningConfig
    module Network.AWS.Lambda.DeleteCodeSigningConfig,

    -- ** UpdateCodeSigningConfig
    module Network.AWS.Lambda.UpdateCodeSigningConfig,

    -- ** RemovePermission
    module Network.AWS.Lambda.RemovePermission,

    -- ** DeleteFunctionEventInvokeConfig
    module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig,

    -- ** UpdateFunctionEventInvokeConfig
    module Network.AWS.Lambda.UpdateFunctionEventInvokeConfig,

    -- ** PutFunctionEventInvokeConfig
    module Network.AWS.Lambda.PutFunctionEventInvokeConfig,

    -- ** Invoke
    module Network.AWS.Lambda.Invoke,

    -- ** DeleteLayerVersion
    module Network.AWS.Lambda.DeleteLayerVersion,

    -- ** GetAlias
    module Network.AWS.Lambda.GetAlias,

    -- ** PublishLayerVersion
    module Network.AWS.Lambda.PublishLayerVersion,

    -- ** GetEventSourceMapping
    module Network.AWS.Lambda.GetEventSourceMapping,

    -- ** AddLayerVersionPermission
    module Network.AWS.Lambda.AddLayerVersionPermission,

    -- ** ListProvisionedConcurrencyConfigs (Paginated)
    module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs,

    -- ** PutFunctionConcurrency
    module Network.AWS.Lambda.PutFunctionConcurrency,

    -- ** CreateFunction
    module Network.AWS.Lambda.CreateFunction,

    -- ** DeleteFunctionConcurrency
    module Network.AWS.Lambda.DeleteFunctionConcurrency,

    -- ** GetLayerVersionByArn
    module Network.AWS.Lambda.GetLayerVersionByArn,

    -- ** GetFunctionConcurrency
    module Network.AWS.Lambda.GetFunctionConcurrency,

    -- ** CreateEventSourceMapping
    module Network.AWS.Lambda.CreateEventSourceMapping,

    -- ** GetProvisionedConcurrencyConfig
    module Network.AWS.Lambda.GetProvisionedConcurrencyConfig,

    -- ** RemoveLayerVersionPermission
    module Network.AWS.Lambda.RemoveLayerVersionPermission,

    -- ** ListFunctionsByCodeSigningConfig (Paginated)
    module Network.AWS.Lambda.ListFunctionsByCodeSigningConfig,

    -- ** GetFunction
    module Network.AWS.Lambda.GetFunction,

    -- ** ListEventSourceMappings (Paginated)
    module Network.AWS.Lambda.ListEventSourceMappings,

    -- ** GetLayerVersionPolicy
    module Network.AWS.Lambda.GetLayerVersionPolicy,

    -- ** DeleteAlias
    module Network.AWS.Lambda.DeleteAlias,

    -- ** UpdateAlias
    module Network.AWS.Lambda.UpdateAlias,

    -- ** GetAccountSettings
    module Network.AWS.Lambda.GetAccountSettings,

    -- ** GetFunctionEventInvokeConfig
    module Network.AWS.Lambda.GetFunctionEventInvokeConfig,

    -- ** GetCodeSigningConfig
    module Network.AWS.Lambda.GetCodeSigningConfig,

    -- ** AddPermission
    module Network.AWS.Lambda.AddPermission,

    -- ** ListLayers (Paginated)
    module Network.AWS.Lambda.ListLayers,

    -- ** ListFunctionEventInvokeConfigs (Paginated)
    module Network.AWS.Lambda.ListFunctionEventInvokeConfigs,

    -- ** ListCodeSigningConfigs (Paginated)
    module Network.AWS.Lambda.ListCodeSigningConfigs,

    -- ** GetFunctionCodeSigningConfig
    module Network.AWS.Lambda.GetFunctionCodeSigningConfig,

    -- ** CreateCodeSigningConfig
    module Network.AWS.Lambda.CreateCodeSigningConfig,

    -- ** ListLayerVersions (Paginated)
    module Network.AWS.Lambda.ListLayerVersions,

    -- ** TagResource
    module Network.AWS.Lambda.TagResource,

    -- ** PublishVersion
    module Network.AWS.Lambda.PublishVersion,

    -- ** ListTags
    module Network.AWS.Lambda.ListTags,

    -- ** DeleteFunction
    module Network.AWS.Lambda.DeleteFunction,

    -- ** UntagResource
    module Network.AWS.Lambda.UntagResource,

    -- ** UpdateFunctionConfiguration
    module Network.AWS.Lambda.UpdateFunctionConfiguration,

    -- ** ListFunctions (Paginated)
    module Network.AWS.Lambda.ListFunctions,

    -- ** UpdateFunctionCode
    module Network.AWS.Lambda.UpdateFunctionCode,

    -- ** DeleteProvisionedConcurrencyConfig
    module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig,

    -- ** GetPolicy
    module Network.AWS.Lambda.GetPolicy,

    -- ** PutProvisionedConcurrencyConfig
    module Network.AWS.Lambda.PutProvisionedConcurrencyConfig,

    -- * Types

    -- ** LayerName
    LayerName (..),

    -- ** MasterRegion
    MasterRegion (..),

    -- ** AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alCodeSizeUnzipped,
    alCodeSizeZipped,
    alConcurrentExecutions,
    alTotalCodeSize,
    alUnreservedConcurrentExecutions,

    -- ** SourceOwner
    SourceOwner (..),

    -- ** CodeSigningConfig
    CodeSigningConfig (..),
    mkCodeSigningConfig,
    cscCodeSigningConfigId,
    cscCodeSigningConfigArn,
    cscAllowedPublishers,
    cscCodeSigningPolicies,
    cscLastModified,
    cscDescription,

    -- ** GetLayerVersionResponse
    GetLayerVersionResponse (..),
    mkGetLayerVersionResponse,
    glvrCompatibleRuntimes,
    glvrContent,
    glvrCreatedDate,
    glvrDescription,
    glvrLayerArn,
    glvrLayerVersionArn,
    glvrLicenseInfo,
    glvrVersion,

    -- ** EnvironmentVariableName
    EnvironmentVariableName (..),

    -- ** LayerVersionArn
    LayerVersionArn (..),

    -- ** Runtime
    Runtime (..),

    -- ** AccountUsage
    AccountUsage (..),
    mkAccountUsage,
    auFunctionCount,
    auTotalCodeSize,

    -- ** FunctionEventInvokeConfig
    FunctionEventInvokeConfig (..),
    mkFunctionEventInvokeConfig,
    feicDestinationConfig,
    feicFunctionArn,
    feicLastModified,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,

    -- ** State
    State (..),

    -- ** NamespacedStatementId
    NamespacedStatementId (..),

    -- ** TracingMode
    TracingMode (..),

    -- ** LastUpdateStatus
    LastUpdateStatus (..),

    -- ** S3ObjectVersion
    S3ObjectVersion (..),

    -- ** LayersListItem
    LayersListItem (..),
    mkLayersListItem,
    lliLatestMatchingVersion,
    lliLayerArn,
    lliLayerName,

    -- ** S3Key
    S3Key (..),

    -- ** FunctionArn
    FunctionArn (..),

    -- ** KMSKeyArn
    KMSKeyArn (..),

    -- ** EventSourceToken
    EventSourceToken (..),

    -- ** OnSuccess
    OnSuccess (..),
    mkOnSuccess,
    osDestination,

    -- ** LayerPermissionAllowedPrincipal
    LayerPermissionAllowedPrincipal (..),

    -- ** ProvisionedConcurrencyStatusEnum
    ProvisionedConcurrencyStatusEnum (..),

    -- ** TracingConfigResponse
    TracingConfigResponse (..),
    mkTracingConfigResponse,
    tcrMode,

    -- ** Environment
    Environment (..),
    mkEnvironment,
    eVariables,

    -- ** Arn
    Arn (..),

    -- ** AllowedPublishers
    AllowedPublishers (..),
    mkAllowedPublishers,
    apSigningProfileVersionArns,

    -- ** String
    String (..),

    -- ** LocalMountPath
    LocalMountPath (..),

    -- ** LayerVersionsListItem
    LayerVersionsListItem (..),
    mkLayerVersionsListItem,
    lvliCompatibleRuntimes,
    lvliCreatedDate,
    lvliDescription,
    lvliLayerVersionArn,
    lvliLicenseInfo,
    lvliVersion,

    -- ** EnvironmentVariableValue
    EnvironmentVariableValue (..),

    -- ** EventSourcePosition
    EventSourcePosition (..),

    -- ** VpcId
    VpcId (..),

    -- ** ProvisionedConcurrencyConfigListItem
    ProvisionedConcurrencyConfigListItem (..),
    mkProvisionedConcurrencyConfigListItem,
    pccliAllocatedProvisionedConcurrentExecutions,
    pccliAvailableProvisionedConcurrentExecutions,
    pccliFunctionArn,
    pccliLastModified,
    pccliRequestedProvisionedConcurrentExecutions,
    pccliStatus,
    pccliStatusReason,

    -- ** CodeSigningPolicies
    CodeSigningPolicies (..),
    mkCodeSigningPolicies,
    cspUntrustedArtifactOnDeployment,

    -- ** InvocationType
    InvocationType (..),

    -- ** DeadLetterConfig
    DeadLetterConfig (..),
    mkDeadLetterConfig,
    dlcTargetArn,

    -- ** AdditionalVersion
    AdditionalVersion (..),

    -- ** Action
    Action (..),

    -- ** Alias
    Alias (..),

    -- ** LayerVersionContentInput
    LayerVersionContentInput (..),
    mkLayerVersionContentInput,
    lvciS3Bucket,
    lvciS3Key,
    lvciS3ObjectVersion,
    lvciZipFile,

    -- ** FileSystemArn
    FileSystemArn (..),

    -- ** CodeSigningPolicy
    CodeSigningPolicy (..),

    -- ** SubnetId
    SubnetId (..),

    -- ** SourceAccessType
    SourceAccessType (..),

    -- ** TagValue
    TagValue (..),

    -- ** CodeSigningConfigArn
    CodeSigningConfigArn (..),

    -- ** Queue
    Queue (..),

    -- ** SensitiveString
    SensitiveString (..),

    -- ** Topic
    Topic (..),

    -- ** DestinationArn
    DestinationArn (..),

    -- ** FileSystemConfig
    FileSystemConfig (..),
    mkFileSystemConfig,
    fscArn,
    fscLocalMountPath,

    -- ** SecurityGroupId
    SecurityGroupId (..),

    -- ** LogType
    LogType (..),

    -- ** VpcConfigResponse
    VpcConfigResponse (..),
    mkVpcConfigResponse,
    vcrSecurityGroupIds,
    vcrSubnetIds,
    vcrVpcId,

    -- ** VpcConfig
    VpcConfig (..),
    mkVpcConfig,
    vcSecurityGroupIds,
    vcSubnetIds,

    -- ** FunctionCode
    FunctionCode (..),
    mkFunctionCode,
    fcS3Bucket,
    fcS3Key,
    fcS3ObjectVersion,
    fcZipFile,

    -- ** LayerVersionContentOutput
    LayerVersionContentOutput (..),
    mkLayerVersionContentOutput,
    lvcoCodeSha256,
    lvcoCodeSize,
    lvcoLocation,
    lvcoSigningJobArn,
    lvcoSigningProfileVersionArn,

    -- ** Principal
    Principal (..),

    -- ** FunctionCodeLocation
    FunctionCodeLocation (..),
    mkFunctionCodeLocation,
    fclLocation,
    fclRepositoryType,

    -- ** Version
    Version (..),

    -- ** FunctionConfiguration
    FunctionConfiguration (..),
    mkFunctionConfiguration,
    fcCodeSha256,
    fcCodeSize,
    fcDeadLetterConfig,
    fcDescription,
    fcEnvironment,
    fcFileSystemConfigs,
    fcFunctionArn,
    fcFunctionName,
    fcHandler,
    fcKMSKeyArn,
    fcLastModified,
    fcLastUpdateStatus,
    fcLastUpdateStatusReason,
    fcLastUpdateStatusReasonCode,
    fcLayers,
    fcMasterArn,
    fcMemorySize,
    fcRevisionId,
    fcRole,
    fcRuntime,
    fcSigningJobArn,
    fcSigningProfileVersionArn,
    fcState,
    fcStateReason,
    fcStateReasonCode,
    fcTimeout,
    fcTracingConfig,
    fcVersion,
    fcVpcConfig,

    -- ** AliasRoutingConfiguration
    AliasRoutingConfiguration (..),
    mkAliasRoutingConfiguration,
    arcAdditionalVersionWeights,

    -- ** NamespacedFunctionName
    NamespacedFunctionName (..),

    -- ** FunctionName
    FunctionName (..),

    -- ** EnvironmentError
    EnvironmentError (..),
    mkEnvironmentError,
    eeErrorCode,
    eeMessage,

    -- ** EventSourceMappingConfiguration
    EventSourceMappingConfiguration (..),
    mkEventSourceMappingConfiguration,
    esmcBatchSize,
    esmcBisectBatchOnFunctionError,
    esmcDestinationConfig,
    esmcEventSourceArn,
    esmcFunctionArn,
    esmcLastModified,
    esmcLastProcessingResult,
    esmcMaximumBatchingWindowInSeconds,
    esmcMaximumRecordAgeInSeconds,
    esmcMaximumRetryAttempts,
    esmcParallelizationFactor,
    esmcQueues,
    esmcSourceAccessConfigurations,
    esmcStartingPosition,
    esmcStartingPositionTimestamp,
    esmcState,
    esmcStateTransitionReason,
    esmcTopics,
    esmcUUID,

    -- ** Concurrency
    Concurrency (..),
    mkConcurrency,
    cReservedConcurrentExecutions,

    -- ** LicenseInfo
    LicenseInfo (..),

    -- ** TagKey
    TagKey (..),

    -- ** Qualifier
    Qualifier (..),

    -- ** FunctionVersion
    FunctionVersion (..),

    -- ** Handler
    Handler (..),

    -- ** StatementId
    StatementId (..),

    -- ** LastUpdateStatusReason
    LastUpdateStatusReason (..),

    -- ** StateReason
    StateReason (..),

    -- ** Layer
    Layer (..),
    mkLayer,
    lArn,
    lCodeSize,
    lSigningJobArn,
    lSigningProfileVersionArn,

    -- ** LayerArn
    LayerArn (..),

    -- ** TracingConfig
    TracingConfig (..),
    mkTracingConfig,
    tcMode,

    -- ** StateReasonCode
    StateReasonCode (..),

    -- ** Description
    Description (..),

    -- ** LayerPermissionAllowedAction
    LayerPermissionAllowedAction (..),

    -- ** S3Bucket
    S3Bucket (..),

    -- ** LastUpdateStatusReasonCode
    LastUpdateStatusReasonCode (..),

    -- ** AliasConfiguration
    AliasConfiguration (..),
    mkAliasConfiguration,
    acAliasArn,
    acDescription,
    acFunctionVersion,
    acName,
    acRevisionId,
    acRoutingConfig,

    -- ** DestinationConfig
    DestinationConfig (..),
    mkDestinationConfig,
    dcOnFailure,
    dcOnSuccess,

    -- ** EnvironmentResponse
    EnvironmentResponse (..),
    mkEnvironmentResponse,
    erError,
    erVariables,

    -- ** OrganizationId
    OrganizationId (..),

    -- ** OnFailure
    OnFailure (..),
    mkOnFailure,
    ofDestination,

    -- ** SourceAccessConfiguration
    SourceAccessConfiguration (..),
    mkSourceAccessConfiguration,
    sacType,
    sacURI,

    -- ** CodeSigningConfigId
    CodeSigningConfigId (..),

    -- ** RevisionId
    RevisionId (..),

    -- ** LastModified
    LastModified (..),

    -- ** CreatedDate
    CreatedDate (..),

    -- ** Role
    Role (..),

    -- ** Marker
    Marker (..),

    -- ** Destination
    Destination (..),

    -- ** StatusReason
    StatusReason (..),

    -- ** UUID
    UUID (..),

    -- ** EventSourceArn
    EventSourceArn (..),

    -- ** TargetArn
    TargetArn (..),

    -- ** Name
    Name (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.Lambda.AddLayerVersionPermission
import Network.AWS.Lambda.AddPermission
import Network.AWS.Lambda.CreateAlias
import Network.AWS.Lambda.CreateCodeSigningConfig
import Network.AWS.Lambda.CreateEventSourceMapping
import Network.AWS.Lambda.CreateFunction
import Network.AWS.Lambda.DeleteAlias
import Network.AWS.Lambda.DeleteCodeSigningConfig
import Network.AWS.Lambda.DeleteEventSourceMapping
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
import Network.AWS.Lambda.DeleteFunctionConcurrency
import Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
import Network.AWS.Lambda.DeleteLayerVersion
import Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
import Network.AWS.Lambda.GetAccountSettings
import Network.AWS.Lambda.GetAlias
import Network.AWS.Lambda.GetCodeSigningConfig
import Network.AWS.Lambda.GetEventSourceMapping
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionCodeSigningConfig
import Network.AWS.Lambda.GetFunctionConcurrency
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.GetFunctionEventInvokeConfig
import Network.AWS.Lambda.GetLayerVersion
import Network.AWS.Lambda.GetLayerVersionByArn
import Network.AWS.Lambda.GetLayerVersionPolicy
import Network.AWS.Lambda.GetPolicy
import Network.AWS.Lambda.GetProvisionedConcurrencyConfig
import Network.AWS.Lambda.Invoke
import Network.AWS.Lambda.ListAliases
import Network.AWS.Lambda.ListCodeSigningConfigs
import Network.AWS.Lambda.ListEventSourceMappings
import Network.AWS.Lambda.ListFunctionEventInvokeConfigs
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
import Network.AWS.Lambda.ListLayerVersions
import Network.AWS.Lambda.ListLayers
import Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
import Network.AWS.Lambda.ListTags
import Network.AWS.Lambda.ListVersionsByFunction
import Network.AWS.Lambda.PublishLayerVersion
import Network.AWS.Lambda.PublishVersion
import Network.AWS.Lambda.PutFunctionCodeSigningConfig
import Network.AWS.Lambda.PutFunctionConcurrency
import Network.AWS.Lambda.PutFunctionEventInvokeConfig
import Network.AWS.Lambda.PutProvisionedConcurrencyConfig
import Network.AWS.Lambda.RemoveLayerVersionPermission
import Network.AWS.Lambda.RemovePermission
import Network.AWS.Lambda.TagResource
import Network.AWS.Lambda.Types
import Network.AWS.Lambda.UntagResource
import Network.AWS.Lambda.UpdateAlias
import Network.AWS.Lambda.UpdateCodeSigningConfig
import Network.AWS.Lambda.UpdateEventSourceMapping
import Network.AWS.Lambda.UpdateFunctionCode
import Network.AWS.Lambda.UpdateFunctionConfiguration
import Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
import Network.AWS.Lambda.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Lambda'.

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
