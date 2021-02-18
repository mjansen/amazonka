{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Sum where

import Network.AWS.Prelude

data DeploymentStatus
  = Completed
  | Eligible
  | InProgress
  | NotEligible
  | PendingUpdate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "eligible" -> pure Eligible
        "in_progress" -> pure InProgress
        "not_eligible" -> pure NotEligible
        "pending_update" -> pure PendingUpdate
        e -> fromTextError $ "Failure parsing DeploymentStatus from value: '" <> e
           <> "'. Accepted values: completed, eligible, in_progress, not_eligible, pending_update"

instance ToText DeploymentStatus where
    toText = \case
        Completed -> "COMPLETED"
        Eligible -> "ELIGIBLE"
        InProgress -> "IN_PROGRESS"
        NotEligible -> "NOT_ELIGIBLE"
        PendingUpdate -> "PENDING_UPDATE"

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

data DescribePackagesFilterName
  = PackageId
  | PackageName
  | PackageStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DescribePackagesFilterName where
    parser = takeLowerText >>= \case
        "packageid" -> pure PackageId
        "packagename" -> pure PackageName
        "packagestatus" -> pure PackageStatus
        e -> fromTextError $ "Failure parsing DescribePackagesFilterName from value: '" <> e
           <> "'. Accepted values: packageid, packagename, packagestatus"

instance ToText DescribePackagesFilterName where
    toText = \case
        PackageId -> "PackageID"
        PackageName -> "PackageName"
        PackageStatus -> "PackageStatus"

instance Hashable     DescribePackagesFilterName
instance NFData       DescribePackagesFilterName
instance ToByteString DescribePackagesFilterName
instance ToQuery      DescribePackagesFilterName
instance ToHeader     DescribePackagesFilterName

instance ToJSON DescribePackagesFilterName where
    toJSON = toJSONText

data DomainPackageStatus
  = DPSActive
  | DPSAssociating
  | DPSAssociationFailed
  | DPSDissociating
  | DPSDissociationFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainPackageStatus where
    parser = takeLowerText >>= \case
        "active" -> pure DPSActive
        "associating" -> pure DPSAssociating
        "association_failed" -> pure DPSAssociationFailed
        "dissociating" -> pure DPSDissociating
        "dissociation_failed" -> pure DPSDissociationFailed
        e -> fromTextError $ "Failure parsing DomainPackageStatus from value: '" <> e
           <> "'. Accepted values: active, associating, association_failed, dissociating, dissociation_failed"

instance ToText DomainPackageStatus where
    toText = \case
        DPSActive -> "ACTIVE"
        DPSAssociating -> "ASSOCIATING"
        DPSAssociationFailed -> "ASSOCIATION_FAILED"
        DPSDissociating -> "DISSOCIATING"
        DPSDissociationFailed -> "DISSOCIATION_FAILED"

instance Hashable     DomainPackageStatus
instance NFData       DomainPackageStatus
instance ToByteString DomainPackageStatus
instance ToQuery      DomainPackageStatus
instance ToHeader     DomainPackageStatus

instance FromJSON DomainPackageStatus where
    parseJSON = parseJSONText "DomainPackageStatus"

data ESPartitionInstanceType
  = C4_2XLarge_Elasticsearch
  | C4_4XLarge_Elasticsearch
  | C4_8XLarge_Elasticsearch
  | C4_Large_Elasticsearch
  | C4_XLarge_Elasticsearch
  | C5_18XLarge_Elasticsearch
  | C5_2XLarge_Elasticsearch
  | C5_4XLarge_Elasticsearch
  | C5_9XLarge_Elasticsearch
  | C5_Large_Elasticsearch
  | C5_XLarge_Elasticsearch
  | D2_2XLarge_Elasticsearch
  | D2_4XLarge_Elasticsearch
  | D2_8XLarge_Elasticsearch
  | D2_XLarge_Elasticsearch
  | I2_2XLarge_Elasticsearch
  | I2_XLarge_Elasticsearch
  | I3_16XLarge_Elasticsearch
  | I3_2XLarge_Elasticsearch
  | I3_4XLarge_Elasticsearch
  | I3_8XLarge_Elasticsearch
  | I3_Large_Elasticsearch
  | I3_XLarge_Elasticsearch
  | M3_2XLarge_Elasticsearch
  | M3_Large_Elasticsearch
  | M3_Medium_Elasticsearch
  | M3_XLarge_Elasticsearch
  | M4_10XLarge_Elasticsearch
  | M4_2XLarge_Elasticsearch
  | M4_4XLarge_Elasticsearch
  | M4_Large_Elasticsearch
  | M4_XLarge_Elasticsearch
  | M5_12XLarge_Elasticsearch
  | M5_2XLarge_Elasticsearch
  | M5_4XLarge_Elasticsearch
  | M5_Large_Elasticsearch
  | M5_XLarge_Elasticsearch
  | R3_2XLarge_Elasticsearch
  | R3_4XLarge_Elasticsearch
  | R3_8XLarge_Elasticsearch
  | R3_Large_Elasticsearch
  | R3_XLarge_Elasticsearch
  | R4_16XLarge_Elasticsearch
  | R4_2XLarge_Elasticsearch
  | R4_4XLarge_Elasticsearch
  | R4_8XLarge_Elasticsearch
  | R4_Large_Elasticsearch
  | R4_XLarge_Elasticsearch
  | R5_12XLarge_Elasticsearch
  | R5_2XLarge_Elasticsearch
  | R5_4XLarge_Elasticsearch
  | R5_Large_Elasticsearch
  | R5_XLarge_Elasticsearch
  | T2_Medium_Elasticsearch
  | T2_Micro_Elasticsearch
  | T2_Small_Elasticsearch
  | ULTRAWARM1_Large_Elasticsearch
  | ULTRAWARM1_Medium_Elasticsearch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ESPartitionInstanceType where
    parser = takeLowerText >>= \case
        "c4.2xlarge.elasticsearch" -> pure C4_2XLarge_Elasticsearch
        "c4.4xlarge.elasticsearch" -> pure C4_4XLarge_Elasticsearch
        "c4.8xlarge.elasticsearch" -> pure C4_8XLarge_Elasticsearch
        "c4.large.elasticsearch" -> pure C4_Large_Elasticsearch
        "c4.xlarge.elasticsearch" -> pure C4_XLarge_Elasticsearch
        "c5.18xlarge.elasticsearch" -> pure C5_18XLarge_Elasticsearch
        "c5.2xlarge.elasticsearch" -> pure C5_2XLarge_Elasticsearch
        "c5.4xlarge.elasticsearch" -> pure C5_4XLarge_Elasticsearch
        "c5.9xlarge.elasticsearch" -> pure C5_9XLarge_Elasticsearch
        "c5.large.elasticsearch" -> pure C5_Large_Elasticsearch
        "c5.xlarge.elasticsearch" -> pure C5_XLarge_Elasticsearch
        "d2.2xlarge.elasticsearch" -> pure D2_2XLarge_Elasticsearch
        "d2.4xlarge.elasticsearch" -> pure D2_4XLarge_Elasticsearch
        "d2.8xlarge.elasticsearch" -> pure D2_8XLarge_Elasticsearch
        "d2.xlarge.elasticsearch" -> pure D2_XLarge_Elasticsearch
        "i2.2xlarge.elasticsearch" -> pure I2_2XLarge_Elasticsearch
        "i2.xlarge.elasticsearch" -> pure I2_XLarge_Elasticsearch
        "i3.16xlarge.elasticsearch" -> pure I3_16XLarge_Elasticsearch
        "i3.2xlarge.elasticsearch" -> pure I3_2XLarge_Elasticsearch
        "i3.4xlarge.elasticsearch" -> pure I3_4XLarge_Elasticsearch
        "i3.8xlarge.elasticsearch" -> pure I3_8XLarge_Elasticsearch
        "i3.large.elasticsearch" -> pure I3_Large_Elasticsearch
        "i3.xlarge.elasticsearch" -> pure I3_XLarge_Elasticsearch
        "m3.2xlarge.elasticsearch" -> pure M3_2XLarge_Elasticsearch
        "m3.large.elasticsearch" -> pure M3_Large_Elasticsearch
        "m3.medium.elasticsearch" -> pure M3_Medium_Elasticsearch
        "m3.xlarge.elasticsearch" -> pure M3_XLarge_Elasticsearch
        "m4.10xlarge.elasticsearch" -> pure M4_10XLarge_Elasticsearch
        "m4.2xlarge.elasticsearch" -> pure M4_2XLarge_Elasticsearch
        "m4.4xlarge.elasticsearch" -> pure M4_4XLarge_Elasticsearch
        "m4.large.elasticsearch" -> pure M4_Large_Elasticsearch
        "m4.xlarge.elasticsearch" -> pure M4_XLarge_Elasticsearch
        "m5.12xlarge.elasticsearch" -> pure M5_12XLarge_Elasticsearch
        "m5.2xlarge.elasticsearch" -> pure M5_2XLarge_Elasticsearch
        "m5.4xlarge.elasticsearch" -> pure M5_4XLarge_Elasticsearch
        "m5.large.elasticsearch" -> pure M5_Large_Elasticsearch
        "m5.xlarge.elasticsearch" -> pure M5_XLarge_Elasticsearch
        "r3.2xlarge.elasticsearch" -> pure R3_2XLarge_Elasticsearch
        "r3.4xlarge.elasticsearch" -> pure R3_4XLarge_Elasticsearch
        "r3.8xlarge.elasticsearch" -> pure R3_8XLarge_Elasticsearch
        "r3.large.elasticsearch" -> pure R3_Large_Elasticsearch
        "r3.xlarge.elasticsearch" -> pure R3_XLarge_Elasticsearch
        "r4.16xlarge.elasticsearch" -> pure R4_16XLarge_Elasticsearch
        "r4.2xlarge.elasticsearch" -> pure R4_2XLarge_Elasticsearch
        "r4.4xlarge.elasticsearch" -> pure R4_4XLarge_Elasticsearch
        "r4.8xlarge.elasticsearch" -> pure R4_8XLarge_Elasticsearch
        "r4.large.elasticsearch" -> pure R4_Large_Elasticsearch
        "r4.xlarge.elasticsearch" -> pure R4_XLarge_Elasticsearch
        "r5.12xlarge.elasticsearch" -> pure R5_12XLarge_Elasticsearch
        "r5.2xlarge.elasticsearch" -> pure R5_2XLarge_Elasticsearch
        "r5.4xlarge.elasticsearch" -> pure R5_4XLarge_Elasticsearch
        "r5.large.elasticsearch" -> pure R5_Large_Elasticsearch
        "r5.xlarge.elasticsearch" -> pure R5_XLarge_Elasticsearch
        "t2.medium.elasticsearch" -> pure T2_Medium_Elasticsearch
        "t2.micro.elasticsearch" -> pure T2_Micro_Elasticsearch
        "t2.small.elasticsearch" -> pure T2_Small_Elasticsearch
        "ultrawarm1.large.elasticsearch" -> pure ULTRAWARM1_Large_Elasticsearch
        "ultrawarm1.medium.elasticsearch" -> pure ULTRAWARM1_Medium_Elasticsearch
        e -> fromTextError $ "Failure parsing ESPartitionInstanceType from value: '" <> e
           <> "'. Accepted values: c4.2xlarge.elasticsearch, c4.4xlarge.elasticsearch, c4.8xlarge.elasticsearch, c4.large.elasticsearch, c4.xlarge.elasticsearch, c5.18xlarge.elasticsearch, c5.2xlarge.elasticsearch, c5.4xlarge.elasticsearch, c5.9xlarge.elasticsearch, c5.large.elasticsearch, c5.xlarge.elasticsearch, d2.2xlarge.elasticsearch, d2.4xlarge.elasticsearch, d2.8xlarge.elasticsearch, d2.xlarge.elasticsearch, i2.2xlarge.elasticsearch, i2.xlarge.elasticsearch, i3.16xlarge.elasticsearch, i3.2xlarge.elasticsearch, i3.4xlarge.elasticsearch, i3.8xlarge.elasticsearch, i3.large.elasticsearch, i3.xlarge.elasticsearch, m3.2xlarge.elasticsearch, m3.large.elasticsearch, m3.medium.elasticsearch, m3.xlarge.elasticsearch, m4.10xlarge.elasticsearch, m4.2xlarge.elasticsearch, m4.4xlarge.elasticsearch, m4.large.elasticsearch, m4.xlarge.elasticsearch, m5.12xlarge.elasticsearch, m5.2xlarge.elasticsearch, m5.4xlarge.elasticsearch, m5.large.elasticsearch, m5.xlarge.elasticsearch, r3.2xlarge.elasticsearch, r3.4xlarge.elasticsearch, r3.8xlarge.elasticsearch, r3.large.elasticsearch, r3.xlarge.elasticsearch, r4.16xlarge.elasticsearch, r4.2xlarge.elasticsearch, r4.4xlarge.elasticsearch, r4.8xlarge.elasticsearch, r4.large.elasticsearch, r4.xlarge.elasticsearch, r5.12xlarge.elasticsearch, r5.2xlarge.elasticsearch, r5.4xlarge.elasticsearch, r5.large.elasticsearch, r5.xlarge.elasticsearch, t2.medium.elasticsearch, t2.micro.elasticsearch, t2.small.elasticsearch, ultrawarm1.large.elasticsearch, ultrawarm1.medium.elasticsearch"

instance ToText ESPartitionInstanceType where
    toText = \case
        C4_2XLarge_Elasticsearch -> "c4.2xlarge.elasticsearch"
        C4_4XLarge_Elasticsearch -> "c4.4xlarge.elasticsearch"
        C4_8XLarge_Elasticsearch -> "c4.8xlarge.elasticsearch"
        C4_Large_Elasticsearch -> "c4.large.elasticsearch"
        C4_XLarge_Elasticsearch -> "c4.xlarge.elasticsearch"
        C5_18XLarge_Elasticsearch -> "c5.18xlarge.elasticsearch"
        C5_2XLarge_Elasticsearch -> "c5.2xlarge.elasticsearch"
        C5_4XLarge_Elasticsearch -> "c5.4xlarge.elasticsearch"
        C5_9XLarge_Elasticsearch -> "c5.9xlarge.elasticsearch"
        C5_Large_Elasticsearch -> "c5.large.elasticsearch"
        C5_XLarge_Elasticsearch -> "c5.xlarge.elasticsearch"
        D2_2XLarge_Elasticsearch -> "d2.2xlarge.elasticsearch"
        D2_4XLarge_Elasticsearch -> "d2.4xlarge.elasticsearch"
        D2_8XLarge_Elasticsearch -> "d2.8xlarge.elasticsearch"
        D2_XLarge_Elasticsearch -> "d2.xlarge.elasticsearch"
        I2_2XLarge_Elasticsearch -> "i2.2xlarge.elasticsearch"
        I2_XLarge_Elasticsearch -> "i2.xlarge.elasticsearch"
        I3_16XLarge_Elasticsearch -> "i3.16xlarge.elasticsearch"
        I3_2XLarge_Elasticsearch -> "i3.2xlarge.elasticsearch"
        I3_4XLarge_Elasticsearch -> "i3.4xlarge.elasticsearch"
        I3_8XLarge_Elasticsearch -> "i3.8xlarge.elasticsearch"
        I3_Large_Elasticsearch -> "i3.large.elasticsearch"
        I3_XLarge_Elasticsearch -> "i3.xlarge.elasticsearch"
        M3_2XLarge_Elasticsearch -> "m3.2xlarge.elasticsearch"
        M3_Large_Elasticsearch -> "m3.large.elasticsearch"
        M3_Medium_Elasticsearch -> "m3.medium.elasticsearch"
        M3_XLarge_Elasticsearch -> "m3.xlarge.elasticsearch"
        M4_10XLarge_Elasticsearch -> "m4.10xlarge.elasticsearch"
        M4_2XLarge_Elasticsearch -> "m4.2xlarge.elasticsearch"
        M4_4XLarge_Elasticsearch -> "m4.4xlarge.elasticsearch"
        M4_Large_Elasticsearch -> "m4.large.elasticsearch"
        M4_XLarge_Elasticsearch -> "m4.xlarge.elasticsearch"
        M5_12XLarge_Elasticsearch -> "m5.12xlarge.elasticsearch"
        M5_2XLarge_Elasticsearch -> "m5.2xlarge.elasticsearch"
        M5_4XLarge_Elasticsearch -> "m5.4xlarge.elasticsearch"
        M5_Large_Elasticsearch -> "m5.large.elasticsearch"
        M5_XLarge_Elasticsearch -> "m5.xlarge.elasticsearch"
        R3_2XLarge_Elasticsearch -> "r3.2xlarge.elasticsearch"
        R3_4XLarge_Elasticsearch -> "r3.4xlarge.elasticsearch"
        R3_8XLarge_Elasticsearch -> "r3.8xlarge.elasticsearch"
        R3_Large_Elasticsearch -> "r3.large.elasticsearch"
        R3_XLarge_Elasticsearch -> "r3.xlarge.elasticsearch"
        R4_16XLarge_Elasticsearch -> "r4.16xlarge.elasticsearch"
        R4_2XLarge_Elasticsearch -> "r4.2xlarge.elasticsearch"
        R4_4XLarge_Elasticsearch -> "r4.4xlarge.elasticsearch"
        R4_8XLarge_Elasticsearch -> "r4.8xlarge.elasticsearch"
        R4_Large_Elasticsearch -> "r4.large.elasticsearch"
        R4_XLarge_Elasticsearch -> "r4.xlarge.elasticsearch"
        R5_12XLarge_Elasticsearch -> "r5.12xlarge.elasticsearch"
        R5_2XLarge_Elasticsearch -> "r5.2xlarge.elasticsearch"
        R5_4XLarge_Elasticsearch -> "r5.4xlarge.elasticsearch"
        R5_Large_Elasticsearch -> "r5.large.elasticsearch"
        R5_XLarge_Elasticsearch -> "r5.xlarge.elasticsearch"
        T2_Medium_Elasticsearch -> "t2.medium.elasticsearch"
        T2_Micro_Elasticsearch -> "t2.micro.elasticsearch"
        T2_Small_Elasticsearch -> "t2.small.elasticsearch"
        ULTRAWARM1_Large_Elasticsearch -> "ultrawarm1.large.elasticsearch"
        ULTRAWARM1_Medium_Elasticsearch -> "ultrawarm1.medium.elasticsearch"

instance Hashable     ESPartitionInstanceType
instance NFData       ESPartitionInstanceType
instance ToByteString ESPartitionInstanceType
instance ToQuery      ESPartitionInstanceType
instance ToHeader     ESPartitionInstanceType

instance ToJSON ESPartitionInstanceType where
    toJSON = toJSONText

instance FromJSON ESPartitionInstanceType where
    parseJSON = parseJSONText "ESPartitionInstanceType"

data ESWarmPartitionInstanceType
  = ESWPITULTRAWARM1_Large_Elasticsearch
  | ESWPITULTRAWARM1_Medium_Elasticsearch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ESWarmPartitionInstanceType where
    parser = takeLowerText >>= \case
        "ultrawarm1.large.elasticsearch" -> pure ESWPITULTRAWARM1_Large_Elasticsearch
        "ultrawarm1.medium.elasticsearch" -> pure ESWPITULTRAWARM1_Medium_Elasticsearch
        e -> fromTextError $ "Failure parsing ESWarmPartitionInstanceType from value: '" <> e
           <> "'. Accepted values: ultrawarm1.large.elasticsearch, ultrawarm1.medium.elasticsearch"

instance ToText ESWarmPartitionInstanceType where
    toText = \case
        ESWPITULTRAWARM1_Large_Elasticsearch -> "ultrawarm1.large.elasticsearch"
        ESWPITULTRAWARM1_Medium_Elasticsearch -> "ultrawarm1.medium.elasticsearch"

instance Hashable     ESWarmPartitionInstanceType
instance NFData       ESWarmPartitionInstanceType
instance ToByteString ESWarmPartitionInstanceType
instance ToQuery      ESWarmPartitionInstanceType
instance ToHeader     ESWarmPartitionInstanceType

instance ToJSON ESWarmPartitionInstanceType where
    toJSON = toJSONText

instance FromJSON ESWarmPartitionInstanceType where
    parseJSON = parseJSONText "ESWarmPartitionInstanceType"

data InboundCrossClusterSearchConnectionStatusCode
  = Approved
  | Deleted
  | Deleting
  | PendingAcceptance
  | Rejected
  | Rejecting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InboundCrossClusterSearchConnectionStatusCode where
    parser = takeLowerText >>= \case
        "approved" -> pure Approved
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending_acceptance" -> pure PendingAcceptance
        "rejected" -> pure Rejected
        "rejecting" -> pure Rejecting
        e -> fromTextError $ "Failure parsing InboundCrossClusterSearchConnectionStatusCode from value: '" <> e
           <> "'. Accepted values: approved, deleted, deleting, pending_acceptance, rejected, rejecting"

instance ToText InboundCrossClusterSearchConnectionStatusCode where
    toText = \case
        Approved -> "APPROVED"
        Deleted -> "DELETED"
        Deleting -> "DELETING"
        PendingAcceptance -> "PENDING_ACCEPTANCE"
        Rejected -> "REJECTED"
        Rejecting -> "REJECTING"

instance Hashable     InboundCrossClusterSearchConnectionStatusCode
instance NFData       InboundCrossClusterSearchConnectionStatusCode
instance ToByteString InboundCrossClusterSearchConnectionStatusCode
instance ToQuery      InboundCrossClusterSearchConnectionStatusCode
instance ToHeader     InboundCrossClusterSearchConnectionStatusCode

instance FromJSON InboundCrossClusterSearchConnectionStatusCode where
    parseJSON = parseJSONText "InboundCrossClusterSearchConnectionStatusCode"

-- | Type of Log File, it can be one of the following:     * INDEX_SLOW_LOGS: Index slow logs contain insert requests that took more time than configured index query log threshold to execute.    * SEARCH_SLOW_LOGS: Search slow logs contain search queries that took more time than configured search query log threshold to execute.    * ES_APPLICATION_LOGS: Elasticsearch application logs contain information about errors and warnings raised during the operation of the service and can be useful for troubleshooting.    * AUDIT_LOGS: Audit logs contain records of user requests for access from the domain.
--
--
--
--
data LogType
  = AuditLogs
  | EsApplicationLogs
  | IndexSlowLogs
  | SearchSlowLogs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogType where
    parser = takeLowerText >>= \case
        "audit_logs" -> pure AuditLogs
        "es_application_logs" -> pure EsApplicationLogs
        "index_slow_logs" -> pure IndexSlowLogs
        "search_slow_logs" -> pure SearchSlowLogs
        e -> fromTextError $ "Failure parsing LogType from value: '" <> e
           <> "'. Accepted values: audit_logs, es_application_logs, index_slow_logs, search_slow_logs"

instance ToText LogType where
    toText = \case
        AuditLogs -> "AUDIT_LOGS"
        EsApplicationLogs -> "ES_APPLICATION_LOGS"
        IndexSlowLogs -> "INDEX_SLOW_LOGS"
        SearchSlowLogs -> "SEARCH_SLOW_LOGS"

instance Hashable     LogType
instance NFData       LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
    toJSON = toJSONText

instance FromJSON LogType where
    parseJSON = parseJSONText "LogType"

-- | The state of a requested change. One of the following:
--
--
--     * Processing: The request change is still in-process.    * Active: The request change is processed and deployed to the Elasticsearch domain.
--
data OptionState
  = Active
  | Processing
  | RequiresIndexDocuments
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OptionState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "processing" -> pure Processing
        "requiresindexdocuments" -> pure RequiresIndexDocuments
        e -> fromTextError $ "Failure parsing OptionState from value: '" <> e
           <> "'. Accepted values: active, processing, requiresindexdocuments"

instance ToText OptionState where
    toText = \case
        Active -> "Active"
        Processing -> "Processing"
        RequiresIndexDocuments -> "RequiresIndexDocuments"

instance Hashable     OptionState
instance NFData       OptionState
instance ToByteString OptionState
instance ToQuery      OptionState
instance ToHeader     OptionState

instance FromJSON OptionState where
    parseJSON = parseJSONText "OptionState"

data OutboundCrossClusterSearchConnectionStatusCode
  = OCCSCSCActive
  | OCCSCSCDeleted
  | OCCSCSCDeleting
  | OCCSCSCPendingAcceptance
  | OCCSCSCProvisioning
  | OCCSCSCRejected
  | OCCSCSCValidating
  | OCCSCSCValidationFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutboundCrossClusterSearchConnectionStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure OCCSCSCActive
        "deleted" -> pure OCCSCSCDeleted
        "deleting" -> pure OCCSCSCDeleting
        "pending_acceptance" -> pure OCCSCSCPendingAcceptance
        "provisioning" -> pure OCCSCSCProvisioning
        "rejected" -> pure OCCSCSCRejected
        "validating" -> pure OCCSCSCValidating
        "validation_failed" -> pure OCCSCSCValidationFailed
        e -> fromTextError $ "Failure parsing OutboundCrossClusterSearchConnectionStatusCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleting, pending_acceptance, provisioning, rejected, validating, validation_failed"

instance ToText OutboundCrossClusterSearchConnectionStatusCode where
    toText = \case
        OCCSCSCActive -> "ACTIVE"
        OCCSCSCDeleted -> "DELETED"
        OCCSCSCDeleting -> "DELETING"
        OCCSCSCPendingAcceptance -> "PENDING_ACCEPTANCE"
        OCCSCSCProvisioning -> "PROVISIONING"
        OCCSCSCRejected -> "REJECTED"
        OCCSCSCValidating -> "VALIDATING"
        OCCSCSCValidationFailed -> "VALIDATION_FAILED"

instance Hashable     OutboundCrossClusterSearchConnectionStatusCode
instance NFData       OutboundCrossClusterSearchConnectionStatusCode
instance ToByteString OutboundCrossClusterSearchConnectionStatusCode
instance ToQuery      OutboundCrossClusterSearchConnectionStatusCode
instance ToHeader     OutboundCrossClusterSearchConnectionStatusCode

instance FromJSON OutboundCrossClusterSearchConnectionStatusCode where
    parseJSON = parseJSONText "OutboundCrossClusterSearchConnectionStatusCode"

data PackageStatus
  = PSAvailable
  | PSCopyFailed
  | PSCopying
  | PSDeleteFailed
  | PSDeleted
  | PSDeleting
  | PSValidating
  | PSValidationFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PackageStatus where
    parser = takeLowerText >>= \case
        "available" -> pure PSAvailable
        "copy_failed" -> pure PSCopyFailed
        "copying" -> pure PSCopying
        "delete_failed" -> pure PSDeleteFailed
        "deleted" -> pure PSDeleted
        "deleting" -> pure PSDeleting
        "validating" -> pure PSValidating
        "validation_failed" -> pure PSValidationFailed
        e -> fromTextError $ "Failure parsing PackageStatus from value: '" <> e
           <> "'. Accepted values: available, copy_failed, copying, delete_failed, deleted, deleting, validating, validation_failed"

instance ToText PackageStatus where
    toText = \case
        PSAvailable -> "AVAILABLE"
        PSCopyFailed -> "COPY_FAILED"
        PSCopying -> "COPYING"
        PSDeleteFailed -> "DELETE_FAILED"
        PSDeleted -> "DELETED"
        PSDeleting -> "DELETING"
        PSValidating -> "VALIDATING"
        PSValidationFailed -> "VALIDATION_FAILED"

instance Hashable     PackageStatus
instance NFData       PackageStatus
instance ToByteString PackageStatus
instance ToQuery      PackageStatus
instance ToHeader     PackageStatus

instance FromJSON PackageStatus where
    parseJSON = parseJSONText "PackageStatus"

data PackageType =
  TxtDictionary
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PackageType where
    parser = takeLowerText >>= \case
        "txt-dictionary" -> pure TxtDictionary
        e -> fromTextError $ "Failure parsing PackageType from value: '" <> e
           <> "'. Accepted values: txt-dictionary"

instance ToText PackageType where
    toText = \case
        TxtDictionary -> "TXT-DICTIONARY"

instance Hashable     PackageType
instance NFData       PackageType
instance ToByteString PackageType
instance ToQuery      PackageType
instance ToHeader     PackageType

instance ToJSON PackageType where
    toJSON = toJSONText

instance FromJSON PackageType where
    parseJSON = parseJSONText "PackageType"

data ReservedElasticsearchInstancePaymentOption
  = AllUpfront
  | NoUpfront
  | PartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservedElasticsearchInstancePaymentOption where
    parser = takeLowerText >>= \case
        "all_upfront" -> pure AllUpfront
        "no_upfront" -> pure NoUpfront
        "partial_upfront" -> pure PartialUpfront
        e -> fromTextError $ "Failure parsing ReservedElasticsearchInstancePaymentOption from value: '" <> e
           <> "'. Accepted values: all_upfront, no_upfront, partial_upfront"

instance ToText ReservedElasticsearchInstancePaymentOption where
    toText = \case
        AllUpfront -> "ALL_UPFRONT"
        NoUpfront -> "NO_UPFRONT"
        PartialUpfront -> "PARTIAL_UPFRONT"

instance Hashable     ReservedElasticsearchInstancePaymentOption
instance NFData       ReservedElasticsearchInstancePaymentOption
instance ToByteString ReservedElasticsearchInstancePaymentOption
instance ToQuery      ReservedElasticsearchInstancePaymentOption
instance ToHeader     ReservedElasticsearchInstancePaymentOption

instance FromJSON ReservedElasticsearchInstancePaymentOption where
    parseJSON = parseJSONText "ReservedElasticsearchInstancePaymentOption"

data TLSSecurityPolicy
  = PolicyMinTLS10201907
  | PolicyMinTLS12201907
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TLSSecurityPolicy where
    parser = takeLowerText >>= \case
        "policy-min-tls-1-0-2019-07" -> pure PolicyMinTLS10201907
        "policy-min-tls-1-2-2019-07" -> pure PolicyMinTLS12201907
        e -> fromTextError $ "Failure parsing TLSSecurityPolicy from value: '" <> e
           <> "'. Accepted values: policy-min-tls-1-0-2019-07, policy-min-tls-1-2-2019-07"

instance ToText TLSSecurityPolicy where
    toText = \case
        PolicyMinTLS10201907 -> "Policy-Min-TLS-1-0-2019-07"
        PolicyMinTLS12201907 -> "Policy-Min-TLS-1-2-2019-07"

instance Hashable     TLSSecurityPolicy
instance NFData       TLSSecurityPolicy
instance ToByteString TLSSecurityPolicy
instance ToQuery      TLSSecurityPolicy
instance ToHeader     TLSSecurityPolicy

instance ToJSON TLSSecurityPolicy where
    toJSON = toJSONText

instance FromJSON TLSSecurityPolicy where
    parseJSON = parseJSONText "TLSSecurityPolicy"

data UpgradeStatus
  = USFailed
  | USInProgress
  | USSucceeded
  | USSucceededWithIssues
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpgradeStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure USFailed
        "in_progress" -> pure USInProgress
        "succeeded" -> pure USSucceeded
        "succeeded_with_issues" -> pure USSucceededWithIssues
        e -> fromTextError $ "Failure parsing UpgradeStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded, succeeded_with_issues"

instance ToText UpgradeStatus where
    toText = \case
        USFailed -> "FAILED"
        USInProgress -> "IN_PROGRESS"
        USSucceeded -> "SUCCEEDED"
        USSucceededWithIssues -> "SUCCEEDED_WITH_ISSUES"

instance Hashable     UpgradeStatus
instance NFData       UpgradeStatus
instance ToByteString UpgradeStatus
instance ToQuery      UpgradeStatus
instance ToHeader     UpgradeStatus

instance FromJSON UpgradeStatus where
    parseJSON = parseJSONText "UpgradeStatus"

data UpgradeStep
  = PreUpgradeCheck
  | Snapshot
  | Upgrade
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpgradeStep where
    parser = takeLowerText >>= \case
        "pre_upgrade_check" -> pure PreUpgradeCheck
        "snapshot" -> pure Snapshot
        "upgrade" -> pure Upgrade
        e -> fromTextError $ "Failure parsing UpgradeStep from value: '" <> e
           <> "'. Accepted values: pre_upgrade_check, snapshot, upgrade"

instance ToText UpgradeStep where
    toText = \case
        PreUpgradeCheck -> "PRE_UPGRADE_CHECK"
        Snapshot -> "SNAPSHOT"
        Upgrade -> "UPGRADE"

instance Hashable     UpgradeStep
instance NFData       UpgradeStep
instance ToByteString UpgradeStep
instance ToQuery      UpgradeStep
instance ToHeader     UpgradeStep

instance FromJSON UpgradeStep where
    parseJSON = parseJSONText "UpgradeStep"

-- | The type of EBS volume, standard, gp2, or io1. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
--
--
data VolumeType
  = GP2
  | IO1
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, io1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable     VolumeType
instance NFData       VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance ToJSON VolumeType where
    toJSON = toJSONText

instance FromJSON VolumeType where
    parseJSON = parseJSONText "VolumeType"
