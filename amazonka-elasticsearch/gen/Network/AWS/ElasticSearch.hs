{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elasticsearch Configuration Service__
--
-- Use the Amazon Elasticsearch Configuration API to create, configure, and manage Elasticsearch domains.
--
-- For sample code that uses the Configuration API, see the <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-configuration-samples.html Amazon Elasticsearch Service Developer Guide> . The guide also contains <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-request-signing.html sample code for sending signed HTTP requests to the Elasticsearch APIs> .
--
-- The endpoint for configuration service requests is region-specific: es./region/ .amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions Regions and Endpoints> .
--
module Network.AWS.ElasticSearch
    (
    -- * Service Configuration
      elasticSearch

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ConflictException
    , _ConflictException

    -- ** BaseException
    , _BaseException

    -- ** DisabledOperationException
    , _DisabledOperationException

    -- ** InternalException
    , _InternalException

    -- ** InvalidTypeException
    , _InvalidTypeException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateOutboundCrossClusterSearchConnection
    , module Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection

    -- ** DescribeInboundCrossClusterSearchConnections
    , module Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections

    -- ** CreateElasticsearchDomain
    , module Network.AWS.ElasticSearch.CreateElasticsearchDomain

    -- ** RemoveTags
    , module Network.AWS.ElasticSearch.RemoveTags

    -- ** GetCompatibleElasticsearchVersions
    , module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions

    -- ** DescribeElasticsearchDomains
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomains

    -- ** ListDomainsForPackage
    , module Network.AWS.ElasticSearch.ListDomainsForPackage

    -- ** ListPackagesForDomain
    , module Network.AWS.ElasticSearch.ListPackagesForDomain

    -- ** StartElasticsearchServiceSoftwareUpdate
    , module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate

    -- ** ListElasticsearchInstanceTypes (Paginated)
    , module Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes

    -- ** DeleteElasticsearchServiceRole
    , module Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole

    -- ** DescribeElasticsearchDomain
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomain

    -- ** ListDomainNames
    , module Network.AWS.ElasticSearch.ListDomainNames

    -- ** AssociatePackage
    , module Network.AWS.ElasticSearch.AssociatePackage

    -- ** DeleteOutboundCrossClusterSearchConnection
    , module Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection

    -- ** DescribeElasticsearchInstanceTypeLimits
    , module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits

    -- ** GetPackageVersionHistory
    , module Network.AWS.ElasticSearch.GetPackageVersionHistory

    -- ** GetUpgradeHistory (Paginated)
    , module Network.AWS.ElasticSearch.GetUpgradeHistory

    -- ** DescribePackages
    , module Network.AWS.ElasticSearch.DescribePackages

    -- ** DescribeElasticsearchDomainConfig
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig

    -- ** GetUpgradeStatus
    , module Network.AWS.ElasticSearch.GetUpgradeStatus

    -- ** DeleteElasticsearchDomain
    , module Network.AWS.ElasticSearch.DeleteElasticsearchDomain

    -- ** DissociatePackage
    , module Network.AWS.ElasticSearch.DissociatePackage

    -- ** PurchaseReservedElasticsearchInstanceOffering
    , module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering

    -- ** DescribeReservedElasticsearchInstances (Paginated)
    , module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances

    -- ** UpdateElasticsearchDomainConfig
    , module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig

    -- ** ListElasticsearchVersions (Paginated)
    , module Network.AWS.ElasticSearch.ListElasticsearchVersions

    -- ** AddTags
    , module Network.AWS.ElasticSearch.AddTags

    -- ** DeleteInboundCrossClusterSearchConnection
    , module Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection

    -- ** DescribeReservedElasticsearchInstanceOfferings (Paginated)
    , module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings

    -- ** UpgradeElasticsearchDomain
    , module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain

    -- ** ListTags
    , module Network.AWS.ElasticSearch.ListTags

    -- ** DeletePackage
    , module Network.AWS.ElasticSearch.DeletePackage

    -- ** UpdatePackage
    , module Network.AWS.ElasticSearch.UpdatePackage

    -- ** CancelElasticsearchServiceSoftwareUpdate
    , module Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate

    -- ** CreatePackage
    , module Network.AWS.ElasticSearch.CreatePackage

    -- ** RejectInboundCrossClusterSearchConnection
    , module Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection

    -- ** DescribeOutboundCrossClusterSearchConnections
    , module Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections

    -- ** AcceptInboundCrossClusterSearchConnection
    , module Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection

    -- * Types

    -- ** DeploymentStatus
    , DeploymentStatus (..)

    -- ** DescribePackagesFilterName
    , DescribePackagesFilterName (..)

    -- ** DomainPackageStatus
    , DomainPackageStatus (..)

    -- ** ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- ** ESWarmPartitionInstanceType
    , ESWarmPartitionInstanceType (..)

    -- ** InboundCrossClusterSearchConnectionStatusCode
    , InboundCrossClusterSearchConnectionStatusCode (..)

    -- ** LogType
    , LogType (..)

    -- ** OptionState
    , OptionState (..)

    -- ** OutboundCrossClusterSearchConnectionStatusCode
    , OutboundCrossClusterSearchConnectionStatusCode (..)

    -- ** PackageStatus
    , PackageStatus (..)

    -- ** PackageType
    , PackageType (..)

    -- ** ReservedElasticsearchInstancePaymentOption
    , ReservedElasticsearchInstancePaymentOption (..)

    -- ** TLSSecurityPolicy
    , TLSSecurityPolicy (..)

    -- ** UpgradeStatus
    , UpgradeStatus (..)

    -- ** UpgradeStep
    , UpgradeStep (..)

    -- ** VolumeType
    , VolumeType (..)

    -- ** AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- ** AdditionalLimit
    , AdditionalLimit
    , additionalLimit
    , alLimitName
    , alLimitValues

    -- ** AdvancedOptionsStatus
    , AdvancedOptionsStatus
    , advancedOptionsStatus
    , aosOptions
    , aosStatus

    -- ** AdvancedSecurityOptions
    , AdvancedSecurityOptions
    , advancedSecurityOptions
    , asoEnabled
    , asoInternalUserDatabaseEnabled
    , asoSAMLOptions

    -- ** AdvancedSecurityOptionsInput
    , AdvancedSecurityOptionsInput
    , advancedSecurityOptionsInput
    , asoiEnabled
    , asoiInternalUserDatabaseEnabled
    , asoiMasterUserOptions
    , asoiSAMLOptions

    -- ** AdvancedSecurityOptionsStatus
    , AdvancedSecurityOptionsStatus
    , advancedSecurityOptionsStatus
    , asosOptions
    , asosStatus

    -- ** CognitoOptions
    , CognitoOptions
    , cognitoOptions
    , coIdentityPoolId
    , coEnabled
    , coUserPoolId
    , coRoleARN

    -- ** CognitoOptionsStatus
    , CognitoOptionsStatus
    , cognitoOptionsStatus
    , cosOptions
    , cosStatus

    -- ** CompatibleVersionsMap
    , CompatibleVersionsMap
    , compatibleVersionsMap
    , cvmSourceVersion
    , cvmTargetVersions

    -- ** DescribePackagesFilter
    , DescribePackagesFilter
    , describePackagesFilter
    , dpfValue
    , dpfName

    -- ** DomainEndpointOptions
    , DomainEndpointOptions
    , domainEndpointOptions
    , deoEnforceHTTPS
    , deoTLSSecurityPolicy
    , deoCustomEndpointEnabled
    , deoCustomEndpoint
    , deoCustomEndpointCertificateARN

    -- ** DomainEndpointOptionsStatus
    , DomainEndpointOptionsStatus
    , domainEndpointOptionsStatus
    , deosOptions
    , deosStatus

    -- ** DomainInfo
    , DomainInfo
    , domainInfo
    , dDomainName

    -- ** DomainInformation
    , DomainInformation
    , domainInformation
    , diOwnerId
    , diRegion
    , diDomainName

    -- ** DomainPackageDetails
    , DomainPackageDetails
    , domainPackageDetails
    , dpdLastUpdated
    , dpdPackageId
    , dpdPackageType
    , dpdPackageName
    , dpdPackageVersion
    , dpdDomainPackageStatus
    , dpdDomainName
    , dpdErrorDetails
    , dpdReferencePath

    -- ** EBSOptions
    , EBSOptions
    , ebsOptions
    , eoVolumeSize
    , eoIOPS
    , eoVolumeType
    , eoEBSEnabled

    -- ** EBSOptionsStatus
    , EBSOptionsStatus
    , ebsOptionsStatus
    , eosOptions
    , eosStatus

    -- ** ElasticsearchClusterConfig
    , ElasticsearchClusterConfig
    , elasticsearchClusterConfig
    , eccDedicatedMasterCount
    , eccDedicatedMasterType
    , eccDedicatedMasterEnabled
    , eccInstanceCount
    , eccZoneAwarenessEnabled
    , eccInstanceType
    , eccWarmEnabled
    , eccZoneAwarenessConfig
    , eccWarmCount
    , eccWarmType

    -- ** ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus
    , elasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- ** ElasticsearchDomainConfig
    , ElasticsearchDomainConfig
    , elasticsearchDomainConfig
    , edcEBSOptions
    , edcNodeToNodeEncryptionOptions
    , edcAccessPolicies
    , edcLogPublishingOptions
    , edcAdvancedSecurityOptions
    , edcElasticsearchClusterConfig
    , edcSnapshotOptions
    , edcCognitoOptions
    , edcEncryptionAtRestOptions
    , edcVPCOptions
    , edcDomainEndpointOptions
    , edcAdvancedOptions
    , edcElasticsearchVersion

    -- ** ElasticsearchDomainStatus
    , ElasticsearchDomainStatus
    , elasticsearchDomainStatus
    , edsEBSOptions
    , edsNodeToNodeEncryptionOptions
    , edsAccessPolicies
    , edsServiceSoftwareOptions
    , edsLogPublishingOptions
    , edsAdvancedSecurityOptions
    , edsCreated
    , edsSnapshotOptions
    , edsCognitoOptions
    , edsEncryptionAtRestOptions
    , edsDeleted
    , edsVPCOptions
    , edsEndpoints
    , edsDomainEndpointOptions
    , edsProcessing
    , edsEndpoint
    , edsUpgradeProcessing
    , edsAdvancedOptions
    , edsElasticsearchVersion
    , edsDomainId
    , edsDomainName
    , edsARN
    , edsElasticsearchClusterConfig

    -- ** ElasticsearchVersionStatus
    , ElasticsearchVersionStatus
    , elasticsearchVersionStatus
    , evsOptions
    , evsStatus

    -- ** EncryptionAtRestOptions
    , EncryptionAtRestOptions
    , encryptionAtRestOptions
    , earoEnabled
    , earoKMSKeyId

    -- ** EncryptionAtRestOptionsStatus
    , EncryptionAtRestOptionsStatus
    , encryptionAtRestOptionsStatus
    , earosOptions
    , earosStatus

    -- ** ErrorDetails
    , ErrorDetails
    , errorDetails
    , edErrorType
    , edErrorMessage

    -- ** Filter
    , Filter
    , filter'
    , fValues
    , fName

    -- ** InboundCrossClusterSearchConnection
    , InboundCrossClusterSearchConnection
    , inboundCrossClusterSearchConnection
    , iccscDestinationDomainInfo
    , iccscCrossClusterSearchConnectionId
    , iccscConnectionStatus
    , iccscSourceDomainInfo

    -- ** InboundCrossClusterSearchConnectionStatus
    , InboundCrossClusterSearchConnectionStatus
    , inboundCrossClusterSearchConnectionStatus
    , iccscsMessage
    , iccscsStatusCode

    -- ** InstanceCountLimits
    , InstanceCountLimits
    , instanceCountLimits
    , iclMaximumInstanceCount
    , iclMinimumInstanceCount

    -- ** InstanceLimits
    , InstanceLimits
    , instanceLimits
    , ilInstanceCountLimits

    -- ** Limits
    , Limits
    , limits
    , lInstanceLimits
    , lAdditionalLimits
    , lStorageTypes

    -- ** LogPublishingOption
    , LogPublishingOption
    , logPublishingOption
    , lpoEnabled
    , lpoCloudWatchLogsLogGroupARN

    -- ** LogPublishingOptionsStatus
    , LogPublishingOptionsStatus
    , logPublishingOptionsStatus
    , lposStatus
    , lposOptions

    -- ** MasterUserOptions
    , MasterUserOptions
    , masterUserOptions
    , muoMasterUserPassword
    , muoMasterUserName
    , muoMasterUserARN

    -- ** NodeToNodeEncryptionOptions
    , NodeToNodeEncryptionOptions
    , nodeToNodeEncryptionOptions
    , ntneoEnabled

    -- ** NodeToNodeEncryptionOptionsStatus
    , NodeToNodeEncryptionOptionsStatus
    , nodeToNodeEncryptionOptionsStatus
    , ntneosOptions
    , ntneosStatus

    -- ** OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- ** OutboundCrossClusterSearchConnection
    , OutboundCrossClusterSearchConnection
    , outboundCrossClusterSearchConnection
    , occscDestinationDomainInfo
    , occscConnectionAlias
    , occscCrossClusterSearchConnectionId
    , occscConnectionStatus
    , occscSourceDomainInfo

    -- ** OutboundCrossClusterSearchConnectionStatus
    , OutboundCrossClusterSearchConnectionStatus
    , outboundCrossClusterSearchConnectionStatus
    , occscsMessage
    , occscsStatusCode

    -- ** PackageDetails
    , PackageDetails
    , packageDetails
    , pdPackageId
    , pdPackageType
    , pdLastUpdatedAt
    , pdCreatedAt
    , pdPackageName
    , pdPackageStatus
    , pdPackageDescription
    , pdErrorDetails
    , pdAvailablePackageVersion

    -- ** PackageSource
    , PackageSource
    , packageSource
    , psS3Key
    , psS3BucketName

    -- ** PackageVersionHistory
    , PackageVersionHistory
    , packageVersionHistory
    , pvhCreatedAt
    , pvhPackageVersion
    , pvhCommitMessage

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** ReservedElasticsearchInstance
    , ReservedElasticsearchInstance
    , reservedElasticsearchInstance
    , reiState
    , reiCurrencyCode
    , reiStartTime
    , reiReservedElasticsearchInstanceOfferingId
    , reiReservedElasticsearchInstanceId
    , reiElasticsearchInstanceCount
    , reiReservationName
    , reiElasticsearchInstanceType
    , reiRecurringCharges
    , reiUsagePrice
    , reiFixedPrice
    , reiDuration
    , reiPaymentOption

    -- ** ReservedElasticsearchInstanceOffering
    , ReservedElasticsearchInstanceOffering
    , reservedElasticsearchInstanceOffering
    , reioCurrencyCode
    , reioReservedElasticsearchInstanceOfferingId
    , reioElasticsearchInstanceType
    , reioRecurringCharges
    , reioUsagePrice
    , reioFixedPrice
    , reioDuration
    , reioPaymentOption

    -- ** SAMLIdp
    , SAMLIdp
    , sAMLIdp
    , samliMetadataContent
    , samliEntityId

    -- ** SAMLOptionsInput
    , SAMLOptionsInput
    , sAMLOptionsInput
    , samloiMasterUserName
    , samloiEnabled
    , samloiIdp
    , samloiRolesKey
    , samloiMasterBackendRole
    , samloiSessionTimeoutMinutes
    , samloiSubjectKey

    -- ** SAMLOptionsOutput
    , SAMLOptionsOutput
    , sAMLOptionsOutput
    , samlooEnabled
    , samlooIdp
    , samlooRolesKey
    , samlooSessionTimeoutMinutes
    , samlooSubjectKey

    -- ** ServiceSoftwareOptions
    , ServiceSoftwareOptions
    , serviceSoftwareOptions
    , ssoAutomatedUpdateDate
    , ssoCurrentVersion
    , ssoOptionalDeployment
    , ssoUpdateStatus
    , ssoCancellable
    , ssoUpdateAvailable
    , ssoDescription
    , ssoNewVersion

    -- ** SnapshotOptions
    , SnapshotOptions
    , snapshotOptions
    , soAutomatedSnapshotStartHour

    -- ** SnapshotOptionsStatus
    , SnapshotOptionsStatus
    , snapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- ** StorageType
    , StorageType
    , storageType
    , stStorageTypeLimits
    , stStorageSubTypeName
    , stStorageTypeName

    -- ** StorageTypeLimit
    , StorageTypeLimit
    , storageTypeLimit
    , stlLimitName
    , stlLimitValues

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** UpgradeHistory
    , UpgradeHistory
    , upgradeHistory
    , uhUpgradeStatus
    , uhStepsList
    , uhUpgradeName
    , uhStartTimestamp

    -- ** UpgradeStepItem
    , UpgradeStepItem
    , upgradeStepItem
    , usiUpgradeStepStatus
    , usiProgressPercent
    , usiIssues
    , usiUpgradeStep

    -- ** VPCDerivedInfo
    , VPCDerivedInfo
    , vpcDerivedInfo
    , vdiSecurityGroupIds
    , vdiSubnetIds
    , vdiVPCId
    , vdiAvailabilityZones

    -- ** VPCDerivedInfoStatus
    , VPCDerivedInfoStatus
    , vpcDerivedInfoStatus
    , vdisOptions
    , vdisStatus

    -- ** VPCOptions
    , VPCOptions
    , vpcOptions
    , voSecurityGroupIds
    , voSubnetIds

    -- ** ZoneAwarenessConfig
    , ZoneAwarenessConfig
    , zoneAwarenessConfig
    , zacAvailabilityZoneCount
    ) where

import Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.AddTags
import Network.AWS.ElasticSearch.AssociatePackage
import Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.CreateElasticsearchDomain
import Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.CreatePackage
import Network.AWS.ElasticSearch.DeleteElasticsearchDomain
import Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
import Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeletePackage
import Network.AWS.ElasticSearch.DescribeElasticsearchDomain
import Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
import Network.AWS.ElasticSearch.DescribeElasticsearchDomains
import Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribePackages
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
import Network.AWS.ElasticSearch.DissociatePackage
import Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
import Network.AWS.ElasticSearch.GetPackageVersionHistory
import Network.AWS.ElasticSearch.GetUpgradeHistory
import Network.AWS.ElasticSearch.GetUpgradeStatus
import Network.AWS.ElasticSearch.ListDomainNames
import Network.AWS.ElasticSearch.ListDomainsForPackage
import Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
import Network.AWS.ElasticSearch.ListElasticsearchVersions
import Network.AWS.ElasticSearch.ListPackagesForDomain
import Network.AWS.ElasticSearch.ListTags
import Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.RemoveTags
import Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
import Network.AWS.ElasticSearch.UpdatePackage
import Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
import Network.AWS.ElasticSearch.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElasticSearch'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
