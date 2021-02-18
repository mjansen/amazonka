{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Sum where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AccountAttributeName
  = DefaultVPC
  | SupportedPlatforms
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountAttributeName where
    parser = takeLowerText >>= \case
        "default-vpc" -> pure DefaultVPC
        "supported-platforms" -> pure SupportedPlatforms
        e -> fromTextError $ "Failure parsing AccountAttributeName from value: '" <> e
           <> "'. Accepted values: default-vpc, supported-platforms"

instance ToText AccountAttributeName where
    toText = \case
        DefaultVPC -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance Hashable     AccountAttributeName
instance NFData       AccountAttributeName
instance ToByteString AccountAttributeName
instance ToQuery      AccountAttributeName
instance ToHeader     AccountAttributeName

data ActivityStatus
  = ASError'
  | ASFulfilled
  | ASPendingFulfillment
  | ASPendingTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActivityStatus where
    parser = takeLowerText >>= \case
        "error" -> pure ASError'
        "fulfilled" -> pure ASFulfilled
        "pending_fulfillment" -> pure ASPendingFulfillment
        "pending_termination" -> pure ASPendingTermination
        e -> fromTextError $ "Failure parsing ActivityStatus from value: '" <> e
           <> "'. Accepted values: error, fulfilled, pending_fulfillment, pending_termination"

instance ToText ActivityStatus where
    toText = \case
        ASError' -> "error"
        ASFulfilled -> "fulfilled"
        ASPendingFulfillment -> "pending_fulfillment"
        ASPendingTermination -> "pending_termination"

instance Hashable     ActivityStatus
instance NFData       ActivityStatus
instance ToByteString ActivityStatus
instance ToQuery      ActivityStatus
instance ToHeader     ActivityStatus

instance FromXML ActivityStatus where
    parseXML = parseXMLText "ActivityStatus"

data AddressAttributeName =
  DomainName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AddressAttributeName where
    parser = takeLowerText >>= \case
        "domain-name" -> pure DomainName
        e -> fromTextError $ "Failure parsing AddressAttributeName from value: '" <> e
           <> "'. Accepted values: domain-name"

instance ToText AddressAttributeName where
    toText = \case
        DomainName -> "domain-name"

instance Hashable     AddressAttributeName
instance NFData       AddressAttributeName
instance ToByteString AddressAttributeName
instance ToQuery      AddressAttributeName
instance ToHeader     AddressAttributeName

data AddressStatus
  = InClassic
  | InVPC
  | MoveInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AddressStatus where
    parser = takeLowerText >>= \case
        "inclassic" -> pure InClassic
        "invpc" -> pure InVPC
        "moveinprogress" -> pure MoveInProgress
        e -> fromTextError $ "Failure parsing AddressStatus from value: '" <> e
           <> "'. Accepted values: inclassic, invpc, moveinprogress"

instance ToText AddressStatus where
    toText = \case
        InClassic -> "InClassic"
        InVPC -> "InVpc"
        MoveInProgress -> "MoveInProgress"

instance Hashable     AddressStatus
instance NFData       AddressStatus
instance ToByteString AddressStatus
instance ToQuery      AddressStatus
instance ToHeader     AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"

data Affinity
  = ADefault
  | AHost
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Affinity where
    parser = takeLowerText >>= \case
        "default" -> pure ADefault
        "host" -> pure AHost
        e -> fromTextError $ "Failure parsing Affinity from value: '" <> e
           <> "'. Accepted values: default, host"

instance ToText Affinity where
    toText = \case
        ADefault -> "default"
        AHost -> "host"

instance Hashable     Affinity
instance NFData       Affinity
instance ToByteString Affinity
instance ToQuery      Affinity
instance ToHeader     Affinity

data AllocationState
  = ASAvailable
  | ASPending
  | ASPermanentFailure
  | ASReleased
  | ASReleasedPermanentFailure
  | ASUnderAssessment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AllocationState where
    parser = takeLowerText >>= \case
        "available" -> pure ASAvailable
        "pending" -> pure ASPending
        "permanent-failure" -> pure ASPermanentFailure
        "released" -> pure ASReleased
        "released-permanent-failure" -> pure ASReleasedPermanentFailure
        "under-assessment" -> pure ASUnderAssessment
        e -> fromTextError $ "Failure parsing AllocationState from value: '" <> e
           <> "'. Accepted values: available, pending, permanent-failure, released, released-permanent-failure, under-assessment"

instance ToText AllocationState where
    toText = \case
        ASAvailable -> "available"
        ASPending -> "pending"
        ASPermanentFailure -> "permanent-failure"
        ASReleased -> "released"
        ASReleasedPermanentFailure -> "released-permanent-failure"
        ASUnderAssessment -> "under-assessment"

instance Hashable     AllocationState
instance NFData       AllocationState
instance ToByteString AllocationState
instance ToQuery      AllocationState
instance ToHeader     AllocationState

instance FromXML AllocationState where
    parseXML = parseXMLText "AllocationState"

data AllocationStrategy
  = ASCapacityOptimized
  | ASDiversified
  | ASLowestPrice
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AllocationStrategy where
    parser = takeLowerText >>= \case
        "capacityoptimized" -> pure ASCapacityOptimized
        "diversified" -> pure ASDiversified
        "lowestprice" -> pure ASLowestPrice
        e -> fromTextError $ "Failure parsing AllocationStrategy from value: '" <> e
           <> "'. Accepted values: capacityoptimized, diversified, lowestprice"

instance ToText AllocationStrategy where
    toText = \case
        ASCapacityOptimized -> "capacityOptimized"
        ASDiversified -> "diversified"
        ASLowestPrice -> "lowestPrice"

instance Hashable     AllocationStrategy
instance NFData       AllocationStrategy
instance ToByteString AllocationStrategy
instance ToQuery      AllocationStrategy
instance ToHeader     AllocationStrategy

instance FromXML AllocationStrategy where
    parseXML = parseXMLText "AllocationStrategy"

data AllowsMultipleInstanceTypes
  = ON
  | Off
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AllowsMultipleInstanceTypes where
    parser = takeLowerText >>= \case
        "on" -> pure ON
        "off" -> pure Off
        e -> fromTextError $ "Failure parsing AllowsMultipleInstanceTypes from value: '" <> e
           <> "'. Accepted values: on, off"

instance ToText AllowsMultipleInstanceTypes where
    toText = \case
        ON -> "on"
        Off -> "off"

instance Hashable     AllowsMultipleInstanceTypes
instance NFData       AllowsMultipleInstanceTypes
instance ToByteString AllowsMultipleInstanceTypes
instance ToQuery      AllowsMultipleInstanceTypes
instance ToHeader     AllowsMultipleInstanceTypes

instance FromXML AllowsMultipleInstanceTypes where
    parseXML = parseXMLText "AllowsMultipleInstanceTypes"

data AnalysisStatus
  = ASFailed
  | ASRunning
  | ASSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AnalysisStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure ASFailed
        "running" -> pure ASRunning
        "succeeded" -> pure ASSucceeded
        e -> fromTextError $ "Failure parsing AnalysisStatus from value: '" <> e
           <> "'. Accepted values: failed, running, succeeded"

instance ToText AnalysisStatus where
    toText = \case
        ASFailed -> "failed"
        ASRunning -> "running"
        ASSucceeded -> "succeeded"

instance Hashable     AnalysisStatus
instance NFData       AnalysisStatus
instance ToByteString AnalysisStatus
instance ToQuery      AnalysisStatus
instance ToHeader     AnalysisStatus

instance FromXML AnalysisStatus where
    parseXML = parseXMLText "AnalysisStatus"

data ApplianceModeSupportValue
  = AMSVDisable
  | AMSVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ApplianceModeSupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure AMSVDisable
        "enable" -> pure AMSVEnable
        e -> fromTextError $ "Failure parsing ApplianceModeSupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText ApplianceModeSupportValue where
    toText = \case
        AMSVDisable -> "disable"
        AMSVEnable -> "enable"

instance Hashable     ApplianceModeSupportValue
instance NFData       ApplianceModeSupportValue
instance ToByteString ApplianceModeSupportValue
instance ToQuery      ApplianceModeSupportValue
instance ToHeader     ApplianceModeSupportValue

instance FromXML ApplianceModeSupportValue where
    parseXML = parseXMLText "ApplianceModeSupportValue"

data ArchitectureType
  = ATARM64
  | ATI386
  | ATX86_64
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArchitectureType where
    parser = takeLowerText >>= \case
        "arm64" -> pure ATARM64
        "i386" -> pure ATI386
        "x86_64" -> pure ATX86_64
        e -> fromTextError $ "Failure parsing ArchitectureType from value: '" <> e
           <> "'. Accepted values: arm64, i386, x86_64"

instance ToText ArchitectureType where
    toText = \case
        ATARM64 -> "arm64"
        ATI386 -> "i386"
        ATX86_64 -> "x86_64"

instance Hashable     ArchitectureType
instance NFData       ArchitectureType
instance ToByteString ArchitectureType
instance ToQuery      ArchitectureType
instance ToHeader     ArchitectureType

instance FromXML ArchitectureType where
    parseXML = parseXMLText "ArchitectureType"

data ArchitectureValues
  = ARM64
  | I386
  | X86_64
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArchitectureValues where
    parser = takeLowerText >>= \case
        "arm64" -> pure ARM64
        "i386" -> pure I386
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing ArchitectureValues from value: '" <> e
           <> "'. Accepted values: arm64, i386, x86_64"

instance ToText ArchitectureValues where
    toText = \case
        ARM64 -> "arm64"
        I386 -> "i386"
        X86_64 -> "x86_64"

instance Hashable     ArchitectureValues
instance NFData       ArchitectureValues
instance ToByteString ArchitectureValues
instance ToQuery      ArchitectureValues
instance ToHeader     ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"

data AssociatedNetworkType =
  ANTVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssociatedNetworkType where
    parser = takeLowerText >>= \case
        "vpc" -> pure ANTVPC
        e -> fromTextError $ "Failure parsing AssociatedNetworkType from value: '" <> e
           <> "'. Accepted values: vpc"

instance ToText AssociatedNetworkType where
    toText = \case
        ANTVPC -> "vpc"

instance Hashable     AssociatedNetworkType
instance NFData       AssociatedNetworkType
instance ToByteString AssociatedNetworkType
instance ToQuery      AssociatedNetworkType
instance ToHeader     AssociatedNetworkType

instance FromXML AssociatedNetworkType where
    parseXML = parseXMLText "AssociatedNetworkType"

data AssociationStatusCode
  = ASCAssociated
  | ASCAssociating
  | ASCAssociationFailed
  | ASCDisassociated
  | ASCDisassociating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssociationStatusCode where
    parser = takeLowerText >>= \case
        "associated" -> pure ASCAssociated
        "associating" -> pure ASCAssociating
        "association-failed" -> pure ASCAssociationFailed
        "disassociated" -> pure ASCDisassociated
        "disassociating" -> pure ASCDisassociating
        e -> fromTextError $ "Failure parsing AssociationStatusCode from value: '" <> e
           <> "'. Accepted values: associated, associating, association-failed, disassociated, disassociating"

instance ToText AssociationStatusCode where
    toText = \case
        ASCAssociated -> "associated"
        ASCAssociating -> "associating"
        ASCAssociationFailed -> "association-failed"
        ASCDisassociated -> "disassociated"
        ASCDisassociating -> "disassociating"

instance Hashable     AssociationStatusCode
instance NFData       AssociationStatusCode
instance ToByteString AssociationStatusCode
instance ToQuery      AssociationStatusCode
instance ToHeader     AssociationStatusCode

instance FromXML AssociationStatusCode where
    parseXML = parseXMLText "AssociationStatusCode"

data AttachmentStatus
  = AAttached
  | AAttaching
  | AAvailable
  | ABusy
  | ADetached
  | ADetaching
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AttachmentStatus where
    parser = takeLowerText >>= \case
        "attached" -> pure AAttached
        "attaching" -> pure AAttaching
        "available" -> pure AAvailable
        "busy" -> pure ABusy
        "detached" -> pure ADetached
        "detaching" -> pure ADetaching
        e -> fromTextError $ "Failure parsing AttachmentStatus from value: '" <> e
           <> "'. Accepted values: attached, attaching, available, busy, detached, detaching"

instance ToText AttachmentStatus where
    toText = \case
        AAttached -> "attached"
        AAttaching -> "attaching"
        AAvailable -> "available"
        ABusy -> "busy"
        ADetached -> "detached"
        ADetaching -> "detaching"

instance Hashable     AttachmentStatus
instance NFData       AttachmentStatus
instance ToByteString AttachmentStatus
instance ToQuery      AttachmentStatus
instance ToHeader     AttachmentStatus

instance FromXML AttachmentStatus where
    parseXML = parseXMLText "AttachmentStatus"

data AutoAcceptSharedAssociationsValue
  = AASAVDisable
  | AASAVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoAcceptSharedAssociationsValue where
    parser = takeLowerText >>= \case
        "disable" -> pure AASAVDisable
        "enable" -> pure AASAVEnable
        e -> fromTextError $ "Failure parsing AutoAcceptSharedAssociationsValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText AutoAcceptSharedAssociationsValue where
    toText = \case
        AASAVDisable -> "disable"
        AASAVEnable -> "enable"

instance Hashable     AutoAcceptSharedAssociationsValue
instance NFData       AutoAcceptSharedAssociationsValue
instance ToByteString AutoAcceptSharedAssociationsValue
instance ToQuery      AutoAcceptSharedAssociationsValue
instance ToHeader     AutoAcceptSharedAssociationsValue

instance FromXML AutoAcceptSharedAssociationsValue where
    parseXML = parseXMLText "AutoAcceptSharedAssociationsValue"

data AutoAcceptSharedAttachmentsValue
  = Disable
  | Enable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoAcceptSharedAttachmentsValue where
    parser = takeLowerText >>= \case
        "disable" -> pure Disable
        "enable" -> pure Enable
        e -> fromTextError $ "Failure parsing AutoAcceptSharedAttachmentsValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText AutoAcceptSharedAttachmentsValue where
    toText = \case
        Disable -> "disable"
        Enable -> "enable"

instance Hashable     AutoAcceptSharedAttachmentsValue
instance NFData       AutoAcceptSharedAttachmentsValue
instance ToByteString AutoAcceptSharedAttachmentsValue
instance ToQuery      AutoAcceptSharedAttachmentsValue
instance ToHeader     AutoAcceptSharedAttachmentsValue

instance FromXML AutoAcceptSharedAttachmentsValue where
    parseXML = parseXMLText "AutoAcceptSharedAttachmentsValue"

data AutoPlacement
  = APON
  | APOff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoPlacement where
    parser = takeLowerText >>= \case
        "on" -> pure APON
        "off" -> pure APOff
        e -> fromTextError $ "Failure parsing AutoPlacement from value: '" <> e
           <> "'. Accepted values: on, off"

instance ToText AutoPlacement where
    toText = \case
        APON -> "on"
        APOff -> "off"

instance Hashable     AutoPlacement
instance NFData       AutoPlacement
instance ToByteString AutoPlacement
instance ToQuery      AutoPlacement
instance ToHeader     AutoPlacement

instance FromXML AutoPlacement where
    parseXML = parseXMLText "AutoPlacement"

data AvailabilityZoneOptInStatus
  = AZOISNotOptedIn
  | AZOISOptInNotRequired
  | AZOISOptedIn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AvailabilityZoneOptInStatus where
    parser = takeLowerText >>= \case
        "not-opted-in" -> pure AZOISNotOptedIn
        "opt-in-not-required" -> pure AZOISOptInNotRequired
        "opted-in" -> pure AZOISOptedIn
        e -> fromTextError $ "Failure parsing AvailabilityZoneOptInStatus from value: '" <> e
           <> "'. Accepted values: not-opted-in, opt-in-not-required, opted-in"

instance ToText AvailabilityZoneOptInStatus where
    toText = \case
        AZOISNotOptedIn -> "not-opted-in"
        AZOISOptInNotRequired -> "opt-in-not-required"
        AZOISOptedIn -> "opted-in"

instance Hashable     AvailabilityZoneOptInStatus
instance NFData       AvailabilityZoneOptInStatus
instance ToByteString AvailabilityZoneOptInStatus
instance ToQuery      AvailabilityZoneOptInStatus
instance ToHeader     AvailabilityZoneOptInStatus

instance FromXML AvailabilityZoneOptInStatus where
    parseXML = parseXMLText "AvailabilityZoneOptInStatus"

data AvailabilityZoneState
  = AZSAvailable
  | AZSImpaired
  | AZSInformation
  | AZSUnavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AvailabilityZoneState where
    parser = takeLowerText >>= \case
        "available" -> pure AZSAvailable
        "impaired" -> pure AZSImpaired
        "information" -> pure AZSInformation
        "unavailable" -> pure AZSUnavailable
        e -> fromTextError $ "Failure parsing AvailabilityZoneState from value: '" <> e
           <> "'. Accepted values: available, impaired, information, unavailable"

instance ToText AvailabilityZoneState where
    toText = \case
        AZSAvailable -> "available"
        AZSImpaired -> "impaired"
        AZSInformation -> "information"
        AZSUnavailable -> "unavailable"

instance Hashable     AvailabilityZoneState
instance NFData       AvailabilityZoneState
instance ToByteString AvailabilityZoneState
instance ToQuery      AvailabilityZoneState
instance ToHeader     AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"

data BGPStatus
  = BSDown
  | BSUP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BGPStatus where
    parser = takeLowerText >>= \case
        "down" -> pure BSDown
        "up" -> pure BSUP
        e -> fromTextError $ "Failure parsing BGPStatus from value: '" <> e
           <> "'. Accepted values: down, up"

instance ToText BGPStatus where
    toText = \case
        BSDown -> "down"
        BSUP -> "up"

instance Hashable     BGPStatus
instance NFData       BGPStatus
instance ToByteString BGPStatus
instance ToQuery      BGPStatus
instance ToHeader     BGPStatus

instance FromXML BGPStatus where
    parseXML = parseXMLText "BGPStatus"

data BatchState
  = BSActive
  | BSCancelled
  | BSCancelledRunning
  | BSCancelledTerminating
  | BSFailed
  | BSModifying
  | BSSubmitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchState where
    parser = takeLowerText >>= \case
        "active" -> pure BSActive
        "cancelled" -> pure BSCancelled
        "cancelled_running" -> pure BSCancelledRunning
        "cancelled_terminating" -> pure BSCancelledTerminating
        "failed" -> pure BSFailed
        "modifying" -> pure BSModifying
        "submitted" -> pure BSSubmitted
        e -> fromTextError $ "Failure parsing BatchState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelled_running, cancelled_terminating, failed, modifying, submitted"

instance ToText BatchState where
    toText = \case
        BSActive -> "active"
        BSCancelled -> "cancelled"
        BSCancelledRunning -> "cancelled_running"
        BSCancelledTerminating -> "cancelled_terminating"
        BSFailed -> "failed"
        BSModifying -> "modifying"
        BSSubmitted -> "submitted"

instance Hashable     BatchState
instance NFData       BatchState
instance ToByteString BatchState
instance ToQuery      BatchState
instance ToHeader     BatchState

instance FromXML BatchState where
    parseXML = parseXMLText "BatchState"

data BundleTaskState
  = BTSBundling
  | BTSCancelling
  | BTSComplete
  | BTSFailed
  | BTSPending
  | BTSStoring
  | BTSWaitingForShutdown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BundleTaskState where
    parser = takeLowerText >>= \case
        "bundling" -> pure BTSBundling
        "cancelling" -> pure BTSCancelling
        "complete" -> pure BTSComplete
        "failed" -> pure BTSFailed
        "pending" -> pure BTSPending
        "storing" -> pure BTSStoring
        "waiting-for-shutdown" -> pure BTSWaitingForShutdown
        e -> fromTextError $ "Failure parsing BundleTaskState from value: '" <> e
           <> "'. Accepted values: bundling, cancelling, complete, failed, pending, storing, waiting-for-shutdown"

instance ToText BundleTaskState where
    toText = \case
        BTSBundling -> "bundling"
        BTSCancelling -> "cancelling"
        BTSComplete -> "complete"
        BTSFailed -> "failed"
        BTSPending -> "pending"
        BTSStoring -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance Hashable     BundleTaskState
instance NFData       BundleTaskState
instance ToByteString BundleTaskState
instance ToQuery      BundleTaskState
instance ToHeader     BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"

data ByoipCidrState
  = Advertised
  | Deprovisioned
  | FailedDeprovision
  | FailedProvision
  | PendingDeprovision
  | PendingProvision
  | Provisioned
  | ProvisionedNotPubliclyAdvertisable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ByoipCidrState where
    parser = takeLowerText >>= \case
        "advertised" -> pure Advertised
        "deprovisioned" -> pure Deprovisioned
        "failed-deprovision" -> pure FailedDeprovision
        "failed-provision" -> pure FailedProvision
        "pending-deprovision" -> pure PendingDeprovision
        "pending-provision" -> pure PendingProvision
        "provisioned" -> pure Provisioned
        "provisioned-not-publicly-advertisable" -> pure ProvisionedNotPubliclyAdvertisable
        e -> fromTextError $ "Failure parsing ByoipCidrState from value: '" <> e
           <> "'. Accepted values: advertised, deprovisioned, failed-deprovision, failed-provision, pending-deprovision, pending-provision, provisioned, provisioned-not-publicly-advertisable"

instance ToText ByoipCidrState where
    toText = \case
        Advertised -> "advertised"
        Deprovisioned -> "deprovisioned"
        FailedDeprovision -> "failed-deprovision"
        FailedProvision -> "failed-provision"
        PendingDeprovision -> "pending-deprovision"
        PendingProvision -> "pending-provision"
        Provisioned -> "provisioned"
        ProvisionedNotPubliclyAdvertisable -> "provisioned-not-publicly-advertisable"

instance Hashable     ByoipCidrState
instance NFData       ByoipCidrState
instance ToByteString ByoipCidrState
instance ToQuery      ByoipCidrState
instance ToHeader     ByoipCidrState

instance FromXML ByoipCidrState where
    parseXML = parseXMLText "ByoipCidrState"

data CancelBatchErrorCode
  = CBECFleetRequestIdDoesNotExist
  | CBECFleetRequestIdMalformed
  | CBECFleetRequestNotInCancellableState
  | CBECUnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelBatchErrorCode where
    parser = takeLowerText >>= \case
        "fleetrequestiddoesnotexist" -> pure CBECFleetRequestIdDoesNotExist
        "fleetrequestidmalformed" -> pure CBECFleetRequestIdMalformed
        "fleetrequestnotincancellablestate" -> pure CBECFleetRequestNotInCancellableState
        "unexpectederror" -> pure CBECUnexpectedError
        e -> fromTextError $ "Failure parsing CancelBatchErrorCode from value: '" <> e
           <> "'. Accepted values: fleetrequestiddoesnotexist, fleetrequestidmalformed, fleetrequestnotincancellablestate, unexpectederror"

instance ToText CancelBatchErrorCode where
    toText = \case
        CBECFleetRequestIdDoesNotExist -> "fleetRequestIdDoesNotExist"
        CBECFleetRequestIdMalformed -> "fleetRequestIdMalformed"
        CBECFleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
        CBECUnexpectedError -> "unexpectedError"

instance Hashable     CancelBatchErrorCode
instance NFData       CancelBatchErrorCode
instance ToByteString CancelBatchErrorCode
instance ToQuery      CancelBatchErrorCode
instance ToHeader     CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

data CancelSpotInstanceRequestState
  = CSIRSActive
  | CSIRSCancelled
  | CSIRSClosed
  | CSIRSCompleted
  | CSIRSOpen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelSpotInstanceRequestState where
    parser = takeLowerText >>= \case
        "active" -> pure CSIRSActive
        "cancelled" -> pure CSIRSCancelled
        "closed" -> pure CSIRSClosed
        "completed" -> pure CSIRSCompleted
        "open" -> pure CSIRSOpen
        e -> fromTextError $ "Failure parsing CancelSpotInstanceRequestState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, completed, open"

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen -> "open"

instance Hashable     CancelSpotInstanceRequestState
instance NFData       CancelSpotInstanceRequestState
instance ToByteString CancelSpotInstanceRequestState
instance ToQuery      CancelSpotInstanceRequestState
instance ToHeader     CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    parseXML = parseXMLText "CancelSpotInstanceRequestState"

data CapacityReservationInstancePlatform
  = CRIPLinuxUnix
  | CRIPLinuxWithSqlServerEnterprise
  | CRIPLinuxWithSqlServerStandard
  | CRIPLinuxWithSqlServerWeb
  | CRIPRedHatEnterpriseLinux
  | CRIPSuseLinux
  | CRIPWindows
  | CRIPWindowsWithSqlServer
  | CRIPWindowsWithSqlServerEnterprise
  | CRIPWindowsWithSqlServerStandard
  | CRIPWindowsWithSqlServerWeb
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CapacityReservationInstancePlatform where
    parser = takeLowerText >>= \case
        "linux/unix" -> pure CRIPLinuxUnix
        "linux with sql server enterprise" -> pure CRIPLinuxWithSqlServerEnterprise
        "linux with sql server standard" -> pure CRIPLinuxWithSqlServerStandard
        "linux with sql server web" -> pure CRIPLinuxWithSqlServerWeb
        "red hat enterprise linux" -> pure CRIPRedHatEnterpriseLinux
        "suse linux" -> pure CRIPSuseLinux
        "windows" -> pure CRIPWindows
        "windows with sql server" -> pure CRIPWindowsWithSqlServer
        "windows with sql server enterprise" -> pure CRIPWindowsWithSqlServerEnterprise
        "windows with sql server standard" -> pure CRIPWindowsWithSqlServerStandard
        "windows with sql server web" -> pure CRIPWindowsWithSqlServerWeb
        e -> fromTextError $ "Failure parsing CapacityReservationInstancePlatform from value: '" <> e
           <> "'. Accepted values: linux/unix, linux with sql server enterprise, linux with sql server standard, linux with sql server web, red hat enterprise linux, suse linux, windows, windows with sql server, windows with sql server enterprise, windows with sql server standard, windows with sql server web"

instance ToText CapacityReservationInstancePlatform where
    toText = \case
        CRIPLinuxUnix -> "Linux/UNIX"
        CRIPLinuxWithSqlServerEnterprise -> "Linux with SQL Server Enterprise"
        CRIPLinuxWithSqlServerStandard -> "Linux with SQL Server Standard"
        CRIPLinuxWithSqlServerWeb -> "Linux with SQL Server Web"
        CRIPRedHatEnterpriseLinux -> "Red Hat Enterprise Linux"
        CRIPSuseLinux -> "SUSE Linux"
        CRIPWindows -> "Windows"
        CRIPWindowsWithSqlServer -> "Windows with SQL Server"
        CRIPWindowsWithSqlServerEnterprise -> "Windows with SQL Server Enterprise"
        CRIPWindowsWithSqlServerStandard -> "Windows with SQL Server Standard"
        CRIPWindowsWithSqlServerWeb -> "Windows with SQL Server Web"

instance Hashable     CapacityReservationInstancePlatform
instance NFData       CapacityReservationInstancePlatform
instance ToByteString CapacityReservationInstancePlatform
instance ToQuery      CapacityReservationInstancePlatform
instance ToHeader     CapacityReservationInstancePlatform

instance FromXML CapacityReservationInstancePlatform where
    parseXML = parseXMLText "CapacityReservationInstancePlatform"

data CapacityReservationPreference
  = None
  | Open
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CapacityReservationPreference where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "open" -> pure Open
        e -> fromTextError $ "Failure parsing CapacityReservationPreference from value: '" <> e
           <> "'. Accepted values: none, open"

instance ToText CapacityReservationPreference where
    toText = \case
        None -> "none"
        Open -> "open"

instance Hashable     CapacityReservationPreference
instance NFData       CapacityReservationPreference
instance ToByteString CapacityReservationPreference
instance ToQuery      CapacityReservationPreference
instance ToHeader     CapacityReservationPreference

instance FromXML CapacityReservationPreference where
    parseXML = parseXMLText "CapacityReservationPreference"

data CapacityReservationState
  = CRSActive
  | CRSCancelled
  | CRSExpired
  | CRSFailed
  | CRSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CapacityReservationState where
    parser = takeLowerText >>= \case
        "active" -> pure CRSActive
        "cancelled" -> pure CRSCancelled
        "expired" -> pure CRSExpired
        "failed" -> pure CRSFailed
        "pending" -> pure CRSPending
        e -> fromTextError $ "Failure parsing CapacityReservationState from value: '" <> e
           <> "'. Accepted values: active, cancelled, expired, failed, pending"

instance ToText CapacityReservationState where
    toText = \case
        CRSActive -> "active"
        CRSCancelled -> "cancelled"
        CRSExpired -> "expired"
        CRSFailed -> "failed"
        CRSPending -> "pending"

instance Hashable     CapacityReservationState
instance NFData       CapacityReservationState
instance ToByteString CapacityReservationState
instance ToQuery      CapacityReservationState
instance ToHeader     CapacityReservationState

instance FromXML CapacityReservationState where
    parseXML = parseXMLText "CapacityReservationState"

data CapacityReservationTenancy
  = CRTDedicated
  | CRTDefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CapacityReservationTenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure CRTDedicated
        "default" -> pure CRTDefault
        e -> fromTextError $ "Failure parsing CapacityReservationTenancy from value: '" <> e
           <> "'. Accepted values: dedicated, default"

instance ToText CapacityReservationTenancy where
    toText = \case
        CRTDedicated -> "dedicated"
        CRTDefault -> "default"

instance Hashable     CapacityReservationTenancy
instance NFData       CapacityReservationTenancy
instance ToByteString CapacityReservationTenancy
instance ToQuery      CapacityReservationTenancy
instance ToHeader     CapacityReservationTenancy

instance FromXML CapacityReservationTenancy where
    parseXML = parseXMLText "CapacityReservationTenancy"

data CarrierGatewayState
  = Available
  | Deleted
  | Deleting
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CarrierGatewayState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing CarrierGatewayState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText CarrierGatewayState where
    toText = \case
        Available -> "available"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"

instance Hashable     CarrierGatewayState
instance NFData       CarrierGatewayState
instance ToByteString CarrierGatewayState
instance ToQuery      CarrierGatewayState
instance ToHeader     CarrierGatewayState

instance FromXML CarrierGatewayState where
    parseXML = parseXMLText "CarrierGatewayState"

data ClientCertificateRevocationListStatusCode
  = CCRLSCActive
  | CCRLSCPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientCertificateRevocationListStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure CCRLSCActive
        "pending" -> pure CCRLSCPending
        e -> fromTextError $ "Failure parsing ClientCertificateRevocationListStatusCode from value: '" <> e
           <> "'. Accepted values: active, pending"

instance ToText ClientCertificateRevocationListStatusCode where
    toText = \case
        CCRLSCActive -> "active"
        CCRLSCPending -> "pending"

instance Hashable     ClientCertificateRevocationListStatusCode
instance NFData       ClientCertificateRevocationListStatusCode
instance ToByteString ClientCertificateRevocationListStatusCode
instance ToQuery      ClientCertificateRevocationListStatusCode
instance ToHeader     ClientCertificateRevocationListStatusCode

instance FromXML ClientCertificateRevocationListStatusCode where
    parseXML = parseXMLText "ClientCertificateRevocationListStatusCode"

data ClientVPNAuthenticationType
  = CertificateAuthentication
  | DirectoryServiceAuthentication
  | FederatedAuthentication
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNAuthenticationType where
    parser = takeLowerText >>= \case
        "certificate-authentication" -> pure CertificateAuthentication
        "directory-service-authentication" -> pure DirectoryServiceAuthentication
        "federated-authentication" -> pure FederatedAuthentication
        e -> fromTextError $ "Failure parsing ClientVPNAuthenticationType from value: '" <> e
           <> "'. Accepted values: certificate-authentication, directory-service-authentication, federated-authentication"

instance ToText ClientVPNAuthenticationType where
    toText = \case
        CertificateAuthentication -> "certificate-authentication"
        DirectoryServiceAuthentication -> "directory-service-authentication"
        FederatedAuthentication -> "federated-authentication"

instance Hashable     ClientVPNAuthenticationType
instance NFData       ClientVPNAuthenticationType
instance ToByteString ClientVPNAuthenticationType
instance ToQuery      ClientVPNAuthenticationType
instance ToHeader     ClientVPNAuthenticationType

instance FromXML ClientVPNAuthenticationType where
    parseXML = parseXMLText "ClientVPNAuthenticationType"

data ClientVPNAuthorizationRuleStatusCode
  = CVARSCActive
  | CVARSCAuthorizing
  | CVARSCFailed
  | CVARSCRevoking
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNAuthorizationRuleStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure CVARSCActive
        "authorizing" -> pure CVARSCAuthorizing
        "failed" -> pure CVARSCFailed
        "revoking" -> pure CVARSCRevoking
        e -> fromTextError $ "Failure parsing ClientVPNAuthorizationRuleStatusCode from value: '" <> e
           <> "'. Accepted values: active, authorizing, failed, revoking"

instance ToText ClientVPNAuthorizationRuleStatusCode where
    toText = \case
        CVARSCActive -> "active"
        CVARSCAuthorizing -> "authorizing"
        CVARSCFailed -> "failed"
        CVARSCRevoking -> "revoking"

instance Hashable     ClientVPNAuthorizationRuleStatusCode
instance NFData       ClientVPNAuthorizationRuleStatusCode
instance ToByteString ClientVPNAuthorizationRuleStatusCode
instance ToQuery      ClientVPNAuthorizationRuleStatusCode
instance ToHeader     ClientVPNAuthorizationRuleStatusCode

instance FromXML ClientVPNAuthorizationRuleStatusCode where
    parseXML = parseXMLText "ClientVPNAuthorizationRuleStatusCode"

data ClientVPNConnectionStatusCode
  = CVCSCActive
  | CVCSCFailedToTerminate
  | CVCSCTerminated
  | CVCSCTerminating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNConnectionStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure CVCSCActive
        "failed-to-terminate" -> pure CVCSCFailedToTerminate
        "terminated" -> pure CVCSCTerminated
        "terminating" -> pure CVCSCTerminating
        e -> fromTextError $ "Failure parsing ClientVPNConnectionStatusCode from value: '" <> e
           <> "'. Accepted values: active, failed-to-terminate, terminated, terminating"

instance ToText ClientVPNConnectionStatusCode where
    toText = \case
        CVCSCActive -> "active"
        CVCSCFailedToTerminate -> "failed-to-terminate"
        CVCSCTerminated -> "terminated"
        CVCSCTerminating -> "terminating"

instance Hashable     ClientVPNConnectionStatusCode
instance NFData       ClientVPNConnectionStatusCode
instance ToByteString ClientVPNConnectionStatusCode
instance ToQuery      ClientVPNConnectionStatusCode
instance ToHeader     ClientVPNConnectionStatusCode

instance FromXML ClientVPNConnectionStatusCode where
    parseXML = parseXMLText "ClientVPNConnectionStatusCode"

data ClientVPNEndpointAttributeStatusCode
  = Applied
  | Applying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNEndpointAttributeStatusCode where
    parser = takeLowerText >>= \case
        "applied" -> pure Applied
        "applying" -> pure Applying
        e -> fromTextError $ "Failure parsing ClientVPNEndpointAttributeStatusCode from value: '" <> e
           <> "'. Accepted values: applied, applying"

instance ToText ClientVPNEndpointAttributeStatusCode where
    toText = \case
        Applied -> "applied"
        Applying -> "applying"

instance Hashable     ClientVPNEndpointAttributeStatusCode
instance NFData       ClientVPNEndpointAttributeStatusCode
instance ToByteString ClientVPNEndpointAttributeStatusCode
instance ToQuery      ClientVPNEndpointAttributeStatusCode
instance ToHeader     ClientVPNEndpointAttributeStatusCode

instance FromXML ClientVPNEndpointAttributeStatusCode where
    parseXML = parseXMLText "ClientVPNEndpointAttributeStatusCode"

data ClientVPNEndpointStatusCode
  = CVESCAvailable
  | CVESCDeleted
  | CVESCDeleting
  | CVESCPendingAssociate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNEndpointStatusCode where
    parser = takeLowerText >>= \case
        "available" -> pure CVESCAvailable
        "deleted" -> pure CVESCDeleted
        "deleting" -> pure CVESCDeleting
        "pending-associate" -> pure CVESCPendingAssociate
        e -> fromTextError $ "Failure parsing ClientVPNEndpointStatusCode from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending-associate"

instance ToText ClientVPNEndpointStatusCode where
    toText = \case
        CVESCAvailable -> "available"
        CVESCDeleted -> "deleted"
        CVESCDeleting -> "deleting"
        CVESCPendingAssociate -> "pending-associate"

instance Hashable     ClientVPNEndpointStatusCode
instance NFData       ClientVPNEndpointStatusCode
instance ToByteString ClientVPNEndpointStatusCode
instance ToQuery      ClientVPNEndpointStatusCode
instance ToHeader     ClientVPNEndpointStatusCode

instance FromXML ClientVPNEndpointStatusCode where
    parseXML = parseXMLText "ClientVPNEndpointStatusCode"

data ClientVPNRouteStatusCode
  = CVRSCActive
  | CVRSCCreating
  | CVRSCDeleting
  | CVRSCFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVPNRouteStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure CVRSCActive
        "creating" -> pure CVRSCCreating
        "deleting" -> pure CVRSCDeleting
        "failed" -> pure CVRSCFailed
        e -> fromTextError $ "Failure parsing ClientVPNRouteStatusCode from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, failed"

instance ToText ClientVPNRouteStatusCode where
    toText = \case
        CVRSCActive -> "active"
        CVRSCCreating -> "creating"
        CVRSCDeleting -> "deleting"
        CVRSCFailed -> "failed"

instance Hashable     ClientVPNRouteStatusCode
instance NFData       ClientVPNRouteStatusCode
instance ToByteString ClientVPNRouteStatusCode
instance ToQuery      ClientVPNRouteStatusCode
instance ToHeader     ClientVPNRouteStatusCode

instance FromXML ClientVPNRouteStatusCode where
    parseXML = parseXMLText "ClientVPNRouteStatusCode"

data ConnectionNotificationState
  = CNSDisabled
  | CNSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionNotificationState where
    parser = takeLowerText >>= \case
        "disabled" -> pure CNSDisabled
        "enabled" -> pure CNSEnabled
        e -> fromTextError $ "Failure parsing ConnectionNotificationState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ConnectionNotificationState where
    toText = \case
        CNSDisabled -> "Disabled"
        CNSEnabled -> "Enabled"

instance Hashable     ConnectionNotificationState
instance NFData       ConnectionNotificationState
instance ToByteString ConnectionNotificationState
instance ToQuery      ConnectionNotificationState
instance ToHeader     ConnectionNotificationState

instance FromXML ConnectionNotificationState where
    parseXML = parseXMLText "ConnectionNotificationState"

data ConnectionNotificationType =
  Topic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionNotificationType where
    parser = takeLowerText >>= \case
        "topic" -> pure Topic
        e -> fromTextError $ "Failure parsing ConnectionNotificationType from value: '" <> e
           <> "'. Accepted values: topic"

instance ToText ConnectionNotificationType where
    toText = \case
        Topic -> "Topic"

instance Hashable     ConnectionNotificationType
instance NFData       ConnectionNotificationType
instance ToByteString ConnectionNotificationType
instance ToQuery      ConnectionNotificationType
instance ToHeader     ConnectionNotificationType

instance FromXML ConnectionNotificationType where
    parseXML = parseXMLText "ConnectionNotificationType"

data ContainerFormat =
  Ova
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "ova" -> pure Ova
        e -> fromTextError $ "Failure parsing ContainerFormat from value: '" <> e
           <> "'. Accepted values: ova"

instance ToText ContainerFormat where
    toText = \case
        Ova -> "ova"

instance Hashable     ContainerFormat
instance NFData       ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"

data ConversionTaskState
  = CTSActive
  | CTSCancelled
  | CTSCancelling
  | CTSCompleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConversionTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure CTSActive
        "cancelled" -> pure CTSCancelled
        "cancelling" -> pure CTSCancelling
        "completed" -> pure CTSCompleted
        e -> fromTextError $ "Failure parsing ConversionTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ConversionTaskState where
    toText = \case
        CTSActive -> "active"
        CTSCancelled -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted -> "completed"

instance Hashable     ConversionTaskState
instance NFData       ConversionTaskState
instance ToByteString ConversionTaskState
instance ToQuery      ConversionTaskState
instance ToHeader     ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"

data CopyTagsFromSource =
  Volume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CopyTagsFromSource where
    parser = takeLowerText >>= \case
        "volume" -> pure Volume
        e -> fromTextError $ "Failure parsing CopyTagsFromSource from value: '" <> e
           <> "'. Accepted values: volume"

instance ToText CopyTagsFromSource where
    toText = \case
        Volume -> "volume"

instance Hashable     CopyTagsFromSource
instance NFData       CopyTagsFromSource
instance ToByteString CopyTagsFromSource
instance ToQuery      CopyTagsFromSource
instance ToHeader     CopyTagsFromSource

data CurrencyCodeValues =
  Usd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CurrencyCodeValues where
    parser = takeLowerText >>= \case
        "usd" -> pure Usd
        e -> fromTextError $ "Failure parsing CurrencyCodeValues from value: '" <> e
           <> "'. Accepted values: usd"

instance ToText CurrencyCodeValues where
    toText = \case
        Usd -> "USD"

instance Hashable     CurrencyCodeValues
instance NFData       CurrencyCodeValues
instance ToByteString CurrencyCodeValues
instance ToQuery      CurrencyCodeValues
instance ToHeader     CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    parseXML = parseXMLText "CurrencyCodeValues"

data DNSNameState
  = DNSFailed
  | DNSPendingVerification
  | DNSVerified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DNSNameState where
    parser = takeLowerText >>= \case
        "failed" -> pure DNSFailed
        "pendingverification" -> pure DNSPendingVerification
        "verified" -> pure DNSVerified
        e -> fromTextError $ "Failure parsing DNSNameState from value: '" <> e
           <> "'. Accepted values: failed, pendingverification, verified"

instance ToText DNSNameState where
    toText = \case
        DNSFailed -> "failed"
        DNSPendingVerification -> "pendingVerification"
        DNSVerified -> "verified"

instance Hashable     DNSNameState
instance NFData       DNSNameState
instance ToByteString DNSNameState
instance ToQuery      DNSNameState
instance ToHeader     DNSNameState

instance FromXML DNSNameState where
    parseXML = parseXMLText "DNSNameState"

data DNSSupportValue
  = DSVDisable
  | DSVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DNSSupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure DSVDisable
        "enable" -> pure DSVEnable
        e -> fromTextError $ "Failure parsing DNSSupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText DNSSupportValue where
    toText = \case
        DSVDisable -> "disable"
        DSVEnable -> "enable"

instance Hashable     DNSSupportValue
instance NFData       DNSSupportValue
instance ToByteString DNSSupportValue
instance ToQuery      DNSSupportValue
instance ToHeader     DNSSupportValue

instance FromXML DNSSupportValue where
    parseXML = parseXMLText "DNSSupportValue"

data DatafeedSubscriptionState
  = DSSActive
  | DSSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DatafeedSubscriptionState where
    parser = takeLowerText >>= \case
        "active" -> pure DSSActive
        "inactive" -> pure DSSInactive
        e -> fromTextError $ "Failure parsing DatafeedSubscriptionState from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive -> "Active"
        DSSInactive -> "Inactive"

instance Hashable     DatafeedSubscriptionState
instance NFData       DatafeedSubscriptionState
instance ToByteString DatafeedSubscriptionState
instance ToQuery      DatafeedSubscriptionState
instance ToHeader     DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    parseXML = parseXMLText "DatafeedSubscriptionState"

data DefaultRouteTableAssociationValue
  = DRTAVDisable
  | DRTAVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DefaultRouteTableAssociationValue where
    parser = takeLowerText >>= \case
        "disable" -> pure DRTAVDisable
        "enable" -> pure DRTAVEnable
        e -> fromTextError $ "Failure parsing DefaultRouteTableAssociationValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText DefaultRouteTableAssociationValue where
    toText = \case
        DRTAVDisable -> "disable"
        DRTAVEnable -> "enable"

instance Hashable     DefaultRouteTableAssociationValue
instance NFData       DefaultRouteTableAssociationValue
instance ToByteString DefaultRouteTableAssociationValue
instance ToQuery      DefaultRouteTableAssociationValue
instance ToHeader     DefaultRouteTableAssociationValue

instance FromXML DefaultRouteTableAssociationValue where
    parseXML = parseXMLText "DefaultRouteTableAssociationValue"

data DefaultRouteTablePropagationValue
  = DRTPVDisable
  | DRTPVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DefaultRouteTablePropagationValue where
    parser = takeLowerText >>= \case
        "disable" -> pure DRTPVDisable
        "enable" -> pure DRTPVEnable
        e -> fromTextError $ "Failure parsing DefaultRouteTablePropagationValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText DefaultRouteTablePropagationValue where
    toText = \case
        DRTPVDisable -> "disable"
        DRTPVEnable -> "enable"

instance Hashable     DefaultRouteTablePropagationValue
instance NFData       DefaultRouteTablePropagationValue
instance ToByteString DefaultRouteTablePropagationValue
instance ToQuery      DefaultRouteTablePropagationValue
instance ToHeader     DefaultRouteTablePropagationValue

instance FromXML DefaultRouteTablePropagationValue where
    parseXML = parseXMLText "DefaultRouteTablePropagationValue"

data DefaultTargetCapacityType
  = DTCTOnDemand
  | DTCTSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DefaultTargetCapacityType where
    parser = takeLowerText >>= \case
        "on-demand" -> pure DTCTOnDemand
        "spot" -> pure DTCTSpot
        e -> fromTextError $ "Failure parsing DefaultTargetCapacityType from value: '" <> e
           <> "'. Accepted values: on-demand, spot"

instance ToText DefaultTargetCapacityType where
    toText = \case
        DTCTOnDemand -> "on-demand"
        DTCTSpot -> "spot"

instance Hashable     DefaultTargetCapacityType
instance NFData       DefaultTargetCapacityType
instance ToByteString DefaultTargetCapacityType
instance ToQuery      DefaultTargetCapacityType
instance ToHeader     DefaultTargetCapacityType

instance FromXML DefaultTargetCapacityType where
    parseXML = parseXMLText "DefaultTargetCapacityType"

data DeleteFleetErrorCode
  = DFECFleetIdDoesNotExist
  | DFECFleetIdMalformed
  | DFECFleetNotInDeletableState
  | DFECUnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeleteFleetErrorCode where
    parser = takeLowerText >>= \case
        "fleetiddoesnotexist" -> pure DFECFleetIdDoesNotExist
        "fleetidmalformed" -> pure DFECFleetIdMalformed
        "fleetnotindeletablestate" -> pure DFECFleetNotInDeletableState
        "unexpectederror" -> pure DFECUnexpectedError
        e -> fromTextError $ "Failure parsing DeleteFleetErrorCode from value: '" <> e
           <> "'. Accepted values: fleetiddoesnotexist, fleetidmalformed, fleetnotindeletablestate, unexpectederror"

instance ToText DeleteFleetErrorCode where
    toText = \case
        DFECFleetIdDoesNotExist -> "fleetIdDoesNotExist"
        DFECFleetIdMalformed -> "fleetIdMalformed"
        DFECFleetNotInDeletableState -> "fleetNotInDeletableState"
        DFECUnexpectedError -> "unexpectedError"

instance Hashable     DeleteFleetErrorCode
instance NFData       DeleteFleetErrorCode
instance ToByteString DeleteFleetErrorCode
instance ToQuery      DeleteFleetErrorCode
instance ToHeader     DeleteFleetErrorCode

instance FromXML DeleteFleetErrorCode where
    parseXML = parseXMLText "DeleteFleetErrorCode"

data DeleteQueuedReservedInstancesErrorCode
  = DQRIECReservedInstancesIdInvalid
  | DQRIECReservedInstancesNotInQueuedState
  | DQRIECUnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeleteQueuedReservedInstancesErrorCode where
    parser = takeLowerText >>= \case
        "reserved-instances-id-invalid" -> pure DQRIECReservedInstancesIdInvalid
        "reserved-instances-not-in-queued-state" -> pure DQRIECReservedInstancesNotInQueuedState
        "unexpected-error" -> pure DQRIECUnexpectedError
        e -> fromTextError $ "Failure parsing DeleteQueuedReservedInstancesErrorCode from value: '" <> e
           <> "'. Accepted values: reserved-instances-id-invalid, reserved-instances-not-in-queued-state, unexpected-error"

instance ToText DeleteQueuedReservedInstancesErrorCode where
    toText = \case
        DQRIECReservedInstancesIdInvalid -> "reserved-instances-id-invalid"
        DQRIECReservedInstancesNotInQueuedState -> "reserved-instances-not-in-queued-state"
        DQRIECUnexpectedError -> "unexpected-error"

instance Hashable     DeleteQueuedReservedInstancesErrorCode
instance NFData       DeleteQueuedReservedInstancesErrorCode
instance ToByteString DeleteQueuedReservedInstancesErrorCode
instance ToQuery      DeleteQueuedReservedInstancesErrorCode
instance ToHeader     DeleteQueuedReservedInstancesErrorCode

instance FromXML DeleteQueuedReservedInstancesErrorCode where
    parseXML = parseXMLText "DeleteQueuedReservedInstancesErrorCode"

data DeviceType
  = DTEBS
  | DTInstanceStore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure DTEBS
        "instance-store" -> pure DTInstanceStore
        e -> fromTextError $ "Failure parsing DeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText DeviceType where
    toText = \case
        DTEBS -> "ebs"
        DTInstanceStore -> "instance-store"

instance Hashable     DeviceType
instance NFData       DeviceType
instance ToByteString DeviceType
instance ToQuery      DeviceType
instance ToHeader     DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"

data DiskImageFormat
  = Raw
  | VHD
  | VMDK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiskImageFormat where
    parser = takeLowerText >>= \case
        "raw" -> pure Raw
        "vhd" -> pure VHD
        "vmdk" -> pure VMDK
        e -> fromTextError $ "Failure parsing DiskImageFormat from value: '" <> e
           <> "'. Accepted values: raw, vhd, vmdk"

instance ToText DiskImageFormat where
    toText = \case
        Raw -> "RAW"
        VHD -> "VHD"
        VMDK -> "VMDK"

instance Hashable     DiskImageFormat
instance NFData       DiskImageFormat
instance ToByteString DiskImageFormat
instance ToQuery      DiskImageFormat
instance ToHeader     DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"

data DiskType
  = Hdd
  | Ssd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiskType where
    parser = takeLowerText >>= \case
        "hdd" -> pure Hdd
        "ssd" -> pure Ssd
        e -> fromTextError $ "Failure parsing DiskType from value: '" <> e
           <> "'. Accepted values: hdd, ssd"

instance ToText DiskType where
    toText = \case
        Hdd -> "hdd"
        Ssd -> "ssd"

instance Hashable     DiskType
instance NFData       DiskType
instance ToByteString DiskType
instance ToQuery      DiskType
instance ToHeader     DiskType

instance FromXML DiskType where
    parseXML = parseXMLText "DiskType"

data DomainType
  = DTStandard
  | DTVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainType where
    parser = takeLowerText >>= \case
        "standard" -> pure DTStandard
        "vpc" -> pure DTVPC
        e -> fromTextError $ "Failure parsing DomainType from value: '" <> e
           <> "'. Accepted values: standard, vpc"

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVPC -> "vpc"

instance Hashable     DomainType
instance NFData       DomainType
instance ToByteString DomainType
instance ToQuery      DomainType
instance ToHeader     DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"

data EBSEncryptionSupport
  = Supported
  | Unsupported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EBSEncryptionSupport where
    parser = takeLowerText >>= \case
        "supported" -> pure Supported
        "unsupported" -> pure Unsupported
        e -> fromTextError $ "Failure parsing EBSEncryptionSupport from value: '" <> e
           <> "'. Accepted values: supported, unsupported"

instance ToText EBSEncryptionSupport where
    toText = \case
        Supported -> "supported"
        Unsupported -> "unsupported"

instance Hashable     EBSEncryptionSupport
instance NFData       EBSEncryptionSupport
instance ToByteString EBSEncryptionSupport
instance ToQuery      EBSEncryptionSupport
instance ToHeader     EBSEncryptionSupport

instance FromXML EBSEncryptionSupport where
    parseXML = parseXMLText "EBSEncryptionSupport"

data EBSNvmeSupport
  = ENSRequired
  | ENSSupported
  | ENSUnsupported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EBSNvmeSupport where
    parser = takeLowerText >>= \case
        "required" -> pure ENSRequired
        "supported" -> pure ENSSupported
        "unsupported" -> pure ENSUnsupported
        e -> fromTextError $ "Failure parsing EBSNvmeSupport from value: '" <> e
           <> "'. Accepted values: required, supported, unsupported"

instance ToText EBSNvmeSupport where
    toText = \case
        ENSRequired -> "required"
        ENSSupported -> "supported"
        ENSUnsupported -> "unsupported"

instance Hashable     EBSNvmeSupport
instance NFData       EBSNvmeSupport
instance ToByteString EBSNvmeSupport
instance ToQuery      EBSNvmeSupport
instance ToHeader     EBSNvmeSupport

instance FromXML EBSNvmeSupport where
    parseXML = parseXMLText "EBSNvmeSupport"

data EBSOptimizedSupport
  = EOSDefault
  | EOSSupported
  | EOSUnsupported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EBSOptimizedSupport where
    parser = takeLowerText >>= \case
        "default" -> pure EOSDefault
        "supported" -> pure EOSSupported
        "unsupported" -> pure EOSUnsupported
        e -> fromTextError $ "Failure parsing EBSOptimizedSupport from value: '" <> e
           <> "'. Accepted values: default, supported, unsupported"

instance ToText EBSOptimizedSupport where
    toText = \case
        EOSDefault -> "default"
        EOSSupported -> "supported"
        EOSUnsupported -> "unsupported"

instance Hashable     EBSOptimizedSupport
instance NFData       EBSOptimizedSupport
instance ToByteString EBSOptimizedSupport
instance ToQuery      EBSOptimizedSupport
instance ToHeader     EBSOptimizedSupport

instance FromXML EBSOptimizedSupport where
    parseXML = parseXMLText "EBSOptimizedSupport"

data ElasticGpuState =
  Attached
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticGpuState where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        e -> fromTextError $ "Failure parsing ElasticGpuState from value: '" <> e
           <> "'. Accepted values: attached"

instance ToText ElasticGpuState where
    toText = \case
        Attached -> "ATTACHED"

instance Hashable     ElasticGpuState
instance NFData       ElasticGpuState
instance ToByteString ElasticGpuState
instance ToQuery      ElasticGpuState
instance ToHeader     ElasticGpuState

instance FromXML ElasticGpuState where
    parseXML = parseXMLText "ElasticGpuState"

data ElasticGpuStatus
  = EGSImpaired
  | EGSOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticGpuStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure EGSImpaired
        "ok" -> pure EGSOK
        e -> fromTextError $ "Failure parsing ElasticGpuStatus from value: '" <> e
           <> "'. Accepted values: impaired, ok"

instance ToText ElasticGpuStatus where
    toText = \case
        EGSImpaired -> "IMPAIRED"
        EGSOK -> "OK"

instance Hashable     ElasticGpuStatus
instance NFData       ElasticGpuStatus
instance ToByteString ElasticGpuStatus
instance ToQuery      ElasticGpuStatus
instance ToHeader     ElasticGpuStatus

instance FromXML ElasticGpuStatus where
    parseXML = parseXMLText "ElasticGpuStatus"

data EnaSupport
  = ESRequired
  | ESSupported
  | ESUnsupported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnaSupport where
    parser = takeLowerText >>= \case
        "required" -> pure ESRequired
        "supported" -> pure ESSupported
        "unsupported" -> pure ESUnsupported
        e -> fromTextError $ "Failure parsing EnaSupport from value: '" <> e
           <> "'. Accepted values: required, supported, unsupported"

instance ToText EnaSupport where
    toText = \case
        ESRequired -> "required"
        ESSupported -> "supported"
        ESUnsupported -> "unsupported"

instance Hashable     EnaSupport
instance NFData       EnaSupport
instance ToByteString EnaSupport
instance ToQuery      EnaSupport
instance ToHeader     EnaSupport

instance FromXML EnaSupport where
    parseXML = parseXMLText "EnaSupport"

data EndDateType
  = Limited
  | Unlimited
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndDateType where
    parser = takeLowerText >>= \case
        "limited" -> pure Limited
        "unlimited" -> pure Unlimited
        e -> fromTextError $ "Failure parsing EndDateType from value: '" <> e
           <> "'. Accepted values: limited, unlimited"

instance ToText EndDateType where
    toText = \case
        Limited -> "limited"
        Unlimited -> "unlimited"

instance Hashable     EndDateType
instance NFData       EndDateType
instance ToByteString EndDateType
instance ToQuery      EndDateType
instance ToHeader     EndDateType

instance FromXML EndDateType where
    parseXML = parseXMLText "EndDateType"

data EphemeralNvmeSupport
  = ERequired
  | ESupported
  | EUnsupported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EphemeralNvmeSupport where
    parser = takeLowerText >>= \case
        "required" -> pure ERequired
        "supported" -> pure ESupported
        "unsupported" -> pure EUnsupported
        e -> fromTextError $ "Failure parsing EphemeralNvmeSupport from value: '" <> e
           <> "'. Accepted values: required, supported, unsupported"

instance ToText EphemeralNvmeSupport where
    toText = \case
        ERequired -> "required"
        ESupported -> "supported"
        EUnsupported -> "unsupported"

instance Hashable     EphemeralNvmeSupport
instance NFData       EphemeralNvmeSupport
instance ToByteString EphemeralNvmeSupport
instance ToQuery      EphemeralNvmeSupport
instance ToHeader     EphemeralNvmeSupport

instance FromXML EphemeralNvmeSupport where
    parseXML = parseXMLText "EphemeralNvmeSupport"

data EventCode
  = InstanceReboot
  | InstanceRetirement
  | InstanceStop
  | SystemMaintenance
  | SystemReboot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventCode where
    parser = takeLowerText >>= \case
        "instance-reboot" -> pure InstanceReboot
        "instance-retirement" -> pure InstanceRetirement
        "instance-stop" -> pure InstanceStop
        "system-maintenance" -> pure SystemMaintenance
        "system-reboot" -> pure SystemReboot
        e -> fromTextError $ "Failure parsing EventCode from value: '" <> e
           <> "'. Accepted values: instance-reboot, instance-retirement, instance-stop, system-maintenance, system-reboot"

instance ToText EventCode where
    toText = \case
        InstanceReboot -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop -> "instance-stop"
        SystemMaintenance -> "system-maintenance"
        SystemReboot -> "system-reboot"

instance Hashable     EventCode
instance NFData       EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"

data EventType
  = ETError'
  | ETFleetRequestChange
  | ETInformation
  | ETInstanceChange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "error" -> pure ETError'
        "fleetrequestchange" -> pure ETFleetRequestChange
        "information" -> pure ETInformation
        "instancechange" -> pure ETInstanceChange
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: error, fleetrequestchange, information, instancechange"

instance ToText EventType where
    toText = \case
        ETError' -> "error"
        ETFleetRequestChange -> "fleetRequestChange"
        ETInformation -> "information"
        ETInstanceChange -> "instanceChange"

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data ExcessCapacityTerminationPolicy
  = ECTPDefault
  | ECTPNoTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExcessCapacityTerminationPolicy where
    parser = takeLowerText >>= \case
        "default" -> pure ECTPDefault
        "notermination" -> pure ECTPNoTermination
        e -> fromTextError $ "Failure parsing ExcessCapacityTerminationPolicy from value: '" <> e
           <> "'. Accepted values: default, notermination"

instance ToText ExcessCapacityTerminationPolicy where
    toText = \case
        ECTPDefault -> "default"
        ECTPNoTermination -> "noTermination"

instance Hashable     ExcessCapacityTerminationPolicy
instance NFData       ExcessCapacityTerminationPolicy
instance ToByteString ExcessCapacityTerminationPolicy
instance ToQuery      ExcessCapacityTerminationPolicy
instance ToHeader     ExcessCapacityTerminationPolicy

instance FromXML ExcessCapacityTerminationPolicy where
    parseXML = parseXMLText "ExcessCapacityTerminationPolicy"

data ExportEnvironment
  = Citrix
  | Microsoft
  | VMware
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportEnvironment where
    parser = takeLowerText >>= \case
        "citrix" -> pure Citrix
        "microsoft" -> pure Microsoft
        "vmware" -> pure VMware
        e -> fromTextError $ "Failure parsing ExportEnvironment from value: '" <> e
           <> "'. Accepted values: citrix, microsoft, vmware"

instance ToText ExportEnvironment where
    toText = \case
        Citrix -> "citrix"
        Microsoft -> "microsoft"
        VMware -> "vmware"

instance Hashable     ExportEnvironment
instance NFData       ExportEnvironment
instance ToByteString ExportEnvironment
instance ToQuery      ExportEnvironment
instance ToHeader     ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"

data ExportTaskState
  = ETSActive
  | ETSCancelled
  | ETSCancelling
  | ETSCompleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure ETSActive
        "cancelled" -> pure ETSCancelled
        "cancelling" -> pure ETSCancelling
        "completed" -> pure ETSCompleted
        e -> fromTextError $ "Failure parsing ExportTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ExportTaskState where
    toText = \case
        ETSActive -> "active"
        ETSCancelled -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted -> "completed"

instance Hashable     ExportTaskState
instance NFData       ExportTaskState
instance ToByteString ExportTaskState
instance ToQuery      ExportTaskState
instance ToHeader     ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

data FastSnapshotRestoreStateCode
  = FSRSCDisabled
  | FSRSCDisabling
  | FSRSCEnabled
  | FSRSCEnabling
  | FSRSCOptimizing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FastSnapshotRestoreStateCode where
    parser = takeLowerText >>= \case
        "disabled" -> pure FSRSCDisabled
        "disabling" -> pure FSRSCDisabling
        "enabled" -> pure FSRSCEnabled
        "enabling" -> pure FSRSCEnabling
        "optimizing" -> pure FSRSCOptimizing
        e -> fromTextError $ "Failure parsing FastSnapshotRestoreStateCode from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, enabling, optimizing"

instance ToText FastSnapshotRestoreStateCode where
    toText = \case
        FSRSCDisabled -> "disabled"
        FSRSCDisabling -> "disabling"
        FSRSCEnabled -> "enabled"
        FSRSCEnabling -> "enabling"
        FSRSCOptimizing -> "optimizing"

instance Hashable     FastSnapshotRestoreStateCode
instance NFData       FastSnapshotRestoreStateCode
instance ToByteString FastSnapshotRestoreStateCode
instance ToQuery      FastSnapshotRestoreStateCode
instance ToHeader     FastSnapshotRestoreStateCode

instance FromXML FastSnapshotRestoreStateCode where
    parseXML = parseXMLText "FastSnapshotRestoreStateCode"

data FleetActivityStatus
  = Error'
  | Fulfilled
  | PendingFulfillment
  | PendingTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetActivityStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "fulfilled" -> pure Fulfilled
        "pending_fulfillment" -> pure PendingFulfillment
        "pending_termination" -> pure PendingTermination
        e -> fromTextError $ "Failure parsing FleetActivityStatus from value: '" <> e
           <> "'. Accepted values: error, fulfilled, pending_fulfillment, pending_termination"

instance ToText FleetActivityStatus where
    toText = \case
        Error' -> "error"
        Fulfilled -> "fulfilled"
        PendingFulfillment -> "pending_fulfillment"
        PendingTermination -> "pending_termination"

instance Hashable     FleetActivityStatus
instance NFData       FleetActivityStatus
instance ToByteString FleetActivityStatus
instance ToQuery      FleetActivityStatus
instance ToHeader     FleetActivityStatus

instance FromXML FleetActivityStatus where
    parseXML = parseXMLText "FleetActivityStatus"

data FleetCapacityReservationUsageStrategy =
  UseCapacityReservationsFirst
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetCapacityReservationUsageStrategy where
    parser = takeLowerText >>= \case
        "use-capacity-reservations-first" -> pure UseCapacityReservationsFirst
        e -> fromTextError $ "Failure parsing FleetCapacityReservationUsageStrategy from value: '" <> e
           <> "'. Accepted values: use-capacity-reservations-first"

instance ToText FleetCapacityReservationUsageStrategy where
    toText = \case
        UseCapacityReservationsFirst -> "use-capacity-reservations-first"

instance Hashable     FleetCapacityReservationUsageStrategy
instance NFData       FleetCapacityReservationUsageStrategy
instance ToByteString FleetCapacityReservationUsageStrategy
instance ToQuery      FleetCapacityReservationUsageStrategy
instance ToHeader     FleetCapacityReservationUsageStrategy

instance FromXML FleetCapacityReservationUsageStrategy where
    parseXML = parseXMLText "FleetCapacityReservationUsageStrategy"

data FleetEventType
  = FETFleetChange
  | FETInstanceChange
  | FETServiceError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetEventType where
    parser = takeLowerText >>= \case
        "fleet-change" -> pure FETFleetChange
        "instance-change" -> pure FETInstanceChange
        "service-error" -> pure FETServiceError
        e -> fromTextError $ "Failure parsing FleetEventType from value: '" <> e
           <> "'. Accepted values: fleet-change, instance-change, service-error"

instance ToText FleetEventType where
    toText = \case
        FETFleetChange -> "fleet-change"
        FETInstanceChange -> "instance-change"
        FETServiceError -> "service-error"

instance Hashable     FleetEventType
instance NFData       FleetEventType
instance ToByteString FleetEventType
instance ToQuery      FleetEventType
instance ToHeader     FleetEventType

instance FromXML FleetEventType where
    parseXML = parseXMLText "FleetEventType"

data FleetExcessCapacityTerminationPolicy
  = NoTermination
  | Termination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetExcessCapacityTerminationPolicy where
    parser = takeLowerText >>= \case
        "no-termination" -> pure NoTermination
        "termination" -> pure Termination
        e -> fromTextError $ "Failure parsing FleetExcessCapacityTerminationPolicy from value: '" <> e
           <> "'. Accepted values: no-termination, termination"

instance ToText FleetExcessCapacityTerminationPolicy where
    toText = \case
        NoTermination -> "no-termination"
        Termination -> "termination"

instance Hashable     FleetExcessCapacityTerminationPolicy
instance NFData       FleetExcessCapacityTerminationPolicy
instance ToByteString FleetExcessCapacityTerminationPolicy
instance ToQuery      FleetExcessCapacityTerminationPolicy
instance ToHeader     FleetExcessCapacityTerminationPolicy

instance FromXML FleetExcessCapacityTerminationPolicy where
    parseXML = parseXMLText "FleetExcessCapacityTerminationPolicy"

data FleetOnDemandAllocationStrategy
  = FODASLowestPrice
  | FODASPrioritized
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetOnDemandAllocationStrategy where
    parser = takeLowerText >>= \case
        "lowest-price" -> pure FODASLowestPrice
        "prioritized" -> pure FODASPrioritized
        e -> fromTextError $ "Failure parsing FleetOnDemandAllocationStrategy from value: '" <> e
           <> "'. Accepted values: lowest-price, prioritized"

instance ToText FleetOnDemandAllocationStrategy where
    toText = \case
        FODASLowestPrice -> "lowest-price"
        FODASPrioritized -> "prioritized"

instance Hashable     FleetOnDemandAllocationStrategy
instance NFData       FleetOnDemandAllocationStrategy
instance ToByteString FleetOnDemandAllocationStrategy
instance ToQuery      FleetOnDemandAllocationStrategy
instance ToHeader     FleetOnDemandAllocationStrategy

instance FromXML FleetOnDemandAllocationStrategy where
    parseXML = parseXMLText "FleetOnDemandAllocationStrategy"

data FleetReplacementStrategy =
  Launch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetReplacementStrategy where
    parser = takeLowerText >>= \case
        "launch" -> pure Launch
        e -> fromTextError $ "Failure parsing FleetReplacementStrategy from value: '" <> e
           <> "'. Accepted values: launch"

instance ToText FleetReplacementStrategy where
    toText = \case
        Launch -> "launch"

instance Hashable     FleetReplacementStrategy
instance NFData       FleetReplacementStrategy
instance ToByteString FleetReplacementStrategy
instance ToQuery      FleetReplacementStrategy
instance ToHeader     FleetReplacementStrategy

instance FromXML FleetReplacementStrategy where
    parseXML = parseXMLText "FleetReplacementStrategy"

data FleetStateCode
  = FSCActive
  | FSCDeleted
  | FSCDeletedRunning
  | FSCDeletedTerminating
  | FSCFailed
  | FSCModifying
  | FSCSubmitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetStateCode where
    parser = takeLowerText >>= \case
        "active" -> pure FSCActive
        "deleted" -> pure FSCDeleted
        "deleted_running" -> pure FSCDeletedRunning
        "deleted_terminating" -> pure FSCDeletedTerminating
        "failed" -> pure FSCFailed
        "modifying" -> pure FSCModifying
        "submitted" -> pure FSCSubmitted
        e -> fromTextError $ "Failure parsing FleetStateCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleted_running, deleted_terminating, failed, modifying, submitted"

instance ToText FleetStateCode where
    toText = \case
        FSCActive -> "active"
        FSCDeleted -> "deleted"
        FSCDeletedRunning -> "deleted_running"
        FSCDeletedTerminating -> "deleted_terminating"
        FSCFailed -> "failed"
        FSCModifying -> "modifying"
        FSCSubmitted -> "submitted"

instance Hashable     FleetStateCode
instance NFData       FleetStateCode
instance ToByteString FleetStateCode
instance ToQuery      FleetStateCode
instance ToHeader     FleetStateCode

instance FromXML FleetStateCode where
    parseXML = parseXMLText "FleetStateCode"

data FleetType
  = FTInstant
  | FTMaintain
  | FTRequest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetType where
    parser = takeLowerText >>= \case
        "instant" -> pure FTInstant
        "maintain" -> pure FTMaintain
        "request" -> pure FTRequest
        e -> fromTextError $ "Failure parsing FleetType from value: '" <> e
           <> "'. Accepted values: instant, maintain, request"

instance ToText FleetType where
    toText = \case
        FTInstant -> "instant"
        FTMaintain -> "maintain"
        FTRequest -> "request"

instance Hashable     FleetType
instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance FromXML FleetType where
    parseXML = parseXMLText "FleetType"

data FlowLogsResourceType
  = FLRTNetworkInterface
  | FLRTSubnet
  | FLRTVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FlowLogsResourceType where
    parser = takeLowerText >>= \case
        "networkinterface" -> pure FLRTNetworkInterface
        "subnet" -> pure FLRTSubnet
        "vpc" -> pure FLRTVPC
        e -> fromTextError $ "Failure parsing FlowLogsResourceType from value: '" <> e
           <> "'. Accepted values: networkinterface, subnet, vpc"

instance ToText FlowLogsResourceType where
    toText = \case
        FLRTNetworkInterface -> "NetworkInterface"
        FLRTSubnet -> "Subnet"
        FLRTVPC -> "VPC"

instance Hashable     FlowLogsResourceType
instance NFData       FlowLogsResourceType
instance ToByteString FlowLogsResourceType
instance ToQuery      FlowLogsResourceType
instance ToHeader     FlowLogsResourceType

data FpgaImageAttributeName
  = FIANDescription
  | FIANLoadPermission
  | FIANName
  | FIANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FpgaImageAttributeName where
    parser = takeLowerText >>= \case
        "description" -> pure FIANDescription
        "loadpermission" -> pure FIANLoadPermission
        "name" -> pure FIANName
        "productcodes" -> pure FIANProductCodes
        e -> fromTextError $ "Failure parsing FpgaImageAttributeName from value: '" <> e
           <> "'. Accepted values: description, loadpermission, name, productcodes"

instance ToText FpgaImageAttributeName where
    toText = \case
        FIANDescription -> "description"
        FIANLoadPermission -> "loadPermission"
        FIANName -> "name"
        FIANProductCodes -> "productCodes"

instance Hashable     FpgaImageAttributeName
instance NFData       FpgaImageAttributeName
instance ToByteString FpgaImageAttributeName
instance ToQuery      FpgaImageAttributeName
instance ToHeader     FpgaImageAttributeName

data FpgaImageStateCode
  = FISCAvailable
  | FISCFailed
  | FISCPending
  | FISCUnavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FpgaImageStateCode where
    parser = takeLowerText >>= \case
        "available" -> pure FISCAvailable
        "failed" -> pure FISCFailed
        "pending" -> pure FISCPending
        "unavailable" -> pure FISCUnavailable
        e -> fromTextError $ "Failure parsing FpgaImageStateCode from value: '" <> e
           <> "'. Accepted values: available, failed, pending, unavailable"

instance ToText FpgaImageStateCode where
    toText = \case
        FISCAvailable -> "available"
        FISCFailed -> "failed"
        FISCPending -> "pending"
        FISCUnavailable -> "unavailable"

instance Hashable     FpgaImageStateCode
instance NFData       FpgaImageStateCode
instance ToByteString FpgaImageStateCode
instance ToQuery      FpgaImageStateCode
instance ToHeader     FpgaImageStateCode

instance FromXML FpgaImageStateCode where
    parseXML = parseXMLText "FpgaImageStateCode"

data GatewayType =
  IPsec_1
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GatewayType where
    parser = takeLowerText >>= \case
        "ipsec.1" -> pure IPsec_1
        e -> fromTextError $ "Failure parsing GatewayType from value: '" <> e
           <> "'. Accepted values: ipsec.1"

instance ToText GatewayType where
    toText = \case
        IPsec_1 -> "ipsec.1"

instance Hashable     GatewayType
instance NFData       GatewayType
instance ToByteString GatewayType
instance ToQuery      GatewayType
instance ToHeader     GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"

data HTTPTokensState
  = HTTPTSOptional
  | HTTPTSRequired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HTTPTokensState where
    parser = takeLowerText >>= \case
        "optional" -> pure HTTPTSOptional
        "required" -> pure HTTPTSRequired
        e -> fromTextError $ "Failure parsing HTTPTokensState from value: '" <> e
           <> "'. Accepted values: optional, required"

instance ToText HTTPTokensState where
    toText = \case
        HTTPTSOptional -> "optional"
        HTTPTSRequired -> "required"

instance Hashable     HTTPTokensState
instance NFData       HTTPTokensState
instance ToByteString HTTPTokensState
instance ToQuery      HTTPTokensState
instance ToHeader     HTTPTokensState

instance FromXML HTTPTokensState where
    parseXML = parseXMLText "HTTPTokensState"

data HostRecovery
  = HRON
  | HROff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HostRecovery where
    parser = takeLowerText >>= \case
        "on" -> pure HRON
        "off" -> pure HROff
        e -> fromTextError $ "Failure parsing HostRecovery from value: '" <> e
           <> "'. Accepted values: on, off"

instance ToText HostRecovery where
    toText = \case
        HRON -> "on"
        HROff -> "off"

instance Hashable     HostRecovery
instance NFData       HostRecovery
instance ToByteString HostRecovery
instance ToQuery      HostRecovery
instance ToHeader     HostRecovery

instance FromXML HostRecovery where
    parseXML = parseXMLText "HostRecovery"

data HostTenancy
  = HTDedicated
  | HTHost
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HostTenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure HTDedicated
        "host" -> pure HTHost
        e -> fromTextError $ "Failure parsing HostTenancy from value: '" <> e
           <> "'. Accepted values: dedicated, host"

instance ToText HostTenancy where
    toText = \case
        HTDedicated -> "dedicated"
        HTHost -> "host"

instance Hashable     HostTenancy
instance NFData       HostTenancy
instance ToByteString HostTenancy
instance ToQuery      HostTenancy
instance ToHeader     HostTenancy

data HypervisorType
  = HTOvm
  | HTXen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HypervisorType where
    parser = takeLowerText >>= \case
        "ovm" -> pure HTOvm
        "xen" -> pure HTXen
        e -> fromTextError $ "Failure parsing HypervisorType from value: '" <> e
           <> "'. Accepted values: ovm, xen"

instance ToText HypervisorType where
    toText = \case
        HTOvm -> "ovm"
        HTXen -> "xen"

instance Hashable     HypervisorType
instance NFData       HypervisorType
instance ToByteString HypervisorType
instance ToQuery      HypervisorType
instance ToHeader     HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"

data IAMInstanceProfileAssociationState
  = IAPASAssociated
  | IAPASAssociating
  | IAPASDisassociated
  | IAPASDisassociating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IAMInstanceProfileAssociationState where
    parser = takeLowerText >>= \case
        "associated" -> pure IAPASAssociated
        "associating" -> pure IAPASAssociating
        "disassociated" -> pure IAPASDisassociated
        "disassociating" -> pure IAPASDisassociating
        e -> fromTextError $ "Failure parsing IAMInstanceProfileAssociationState from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText IAMInstanceProfileAssociationState where
    toText = \case
        IAPASAssociated -> "associated"
        IAPASAssociating -> "associating"
        IAPASDisassociated -> "disassociated"
        IAPASDisassociating -> "disassociating"

instance Hashable     IAMInstanceProfileAssociationState
instance NFData       IAMInstanceProfileAssociationState
instance ToByteString IAMInstanceProfileAssociationState
instance ToQuery      IAMInstanceProfileAssociationState
instance ToHeader     IAMInstanceProfileAssociationState

instance FromXML IAMInstanceProfileAssociationState where
    parseXML = parseXMLText "IAMInstanceProfileAssociationState"

data IPv6SupportValue
  = ISVDisable
  | ISVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPv6SupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure ISVDisable
        "enable" -> pure ISVEnable
        e -> fromTextError $ "Failure parsing IPv6SupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText IPv6SupportValue where
    toText = \case
        ISVDisable -> "disable"
        ISVEnable -> "enable"

instance Hashable     IPv6SupportValue
instance NFData       IPv6SupportValue
instance ToByteString IPv6SupportValue
instance ToQuery      IPv6SupportValue
instance ToHeader     IPv6SupportValue

instance FromXML IPv6SupportValue where
    parseXML = parseXMLText "IPv6SupportValue"

data Igmpv2SupportValue
  = IDisable
  | IEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Igmpv2SupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure IDisable
        "enable" -> pure IEnable
        e -> fromTextError $ "Failure parsing Igmpv2SupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText Igmpv2SupportValue where
    toText = \case
        IDisable -> "disable"
        IEnable -> "enable"

instance Hashable     Igmpv2SupportValue
instance NFData       Igmpv2SupportValue
instance ToByteString Igmpv2SupportValue
instance ToQuery      Igmpv2SupportValue
instance ToHeader     Igmpv2SupportValue

instance FromXML Igmpv2SupportValue where
    parseXML = parseXMLText "Igmpv2SupportValue"

data ImageAttributeName
  = BlockDeviceMapping
  | Description
  | Kernel
  | LaunchPermission
  | ProductCodes
  | RAMDisk
  | SRIOVNetSupport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure BlockDeviceMapping
        "description" -> pure Description
        "kernel" -> pure Kernel
        "launchpermission" -> pure LaunchPermission
        "productcodes" -> pure ProductCodes
        "ramdisk" -> pure RAMDisk
        "sriovnetsupport" -> pure SRIOVNetSupport
        e -> fromTextError $ "Failure parsing ImageAttributeName from value: '" <> e
           <> "'. Accepted values: blockdevicemapping, description, kernel, launchpermission, productcodes, ramdisk, sriovnetsupport"

instance ToText ImageAttributeName where
    toText = \case
        BlockDeviceMapping -> "blockDeviceMapping"
        Description -> "description"
        Kernel -> "kernel"
        LaunchPermission -> "launchPermission"
        ProductCodes -> "productCodes"
        RAMDisk -> "ramdisk"
        SRIOVNetSupport -> "sriovNetSupport"

instance Hashable     ImageAttributeName
instance NFData       ImageAttributeName
instance ToByteString ImageAttributeName
instance ToQuery      ImageAttributeName
instance ToHeader     ImageAttributeName

data ImageState
  = ISAvailable
  | ISDeregistered
  | ISError'
  | ISFailed
  | ISInvalid
  | ISPending
  | ISTransient
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deregistered" -> pure ISDeregistered
        "error" -> pure ISError'
        "failed" -> pure ISFailed
        "invalid" -> pure ISInvalid
        "pending" -> pure ISPending
        "transient" -> pure ISTransient
        e -> fromTextError $ "Failure parsing ImageState from value: '" <> e
           <> "'. Accepted values: available, deregistered, error, failed, invalid, pending, transient"

instance ToText ImageState where
    toText = \case
        ISAvailable -> "available"
        ISDeregistered -> "deregistered"
        ISError' -> "error"
        ISFailed -> "failed"
        ISInvalid -> "invalid"
        ISPending -> "pending"
        ISTransient -> "transient"

instance Hashable     ImageState
instance NFData       ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"

data ImageTypeValues
  = ITVKernel
  | ITVMachine
  | ITVRAMDisk
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageTypeValues where
    parser = takeLowerText >>= \case
        "kernel" -> pure ITVKernel
        "machine" -> pure ITVMachine
        "ramdisk" -> pure ITVRAMDisk
        e -> fromTextError $ "Failure parsing ImageTypeValues from value: '" <> e
           <> "'. Accepted values: kernel, machine, ramdisk"

instance ToText ImageTypeValues where
    toText = \case
        ITVKernel -> "kernel"
        ITVMachine -> "machine"
        ITVRAMDisk -> "ramdisk"

instance Hashable     ImageTypeValues
instance NFData       ImageTypeValues
instance ToByteString ImageTypeValues
instance ToQuery      ImageTypeValues
instance ToHeader     ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"

data InstanceAttributeName
  = IANBlockDeviceMapping
  | IANDisableAPITermination
  | IANEBSOptimized
  | IANEnaSupport
  | IANEnclaveOptions
  | IANGroupSet
  | IANInstanceInitiatedShutdownBehavior
  | IANInstanceType
  | IANKernel
  | IANProductCodes
  | IANRAMDisk
  | IANRootDeviceName
  | IANSRIOVNetSupport
  | IANSourceDestCheck
  | IANUserData
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure IANBlockDeviceMapping
        "disableapitermination" -> pure IANDisableAPITermination
        "ebsoptimized" -> pure IANEBSOptimized
        "enasupport" -> pure IANEnaSupport
        "enclaveoptions" -> pure IANEnclaveOptions
        "groupset" -> pure IANGroupSet
        "instanceinitiatedshutdownbehavior" -> pure IANInstanceInitiatedShutdownBehavior
        "instancetype" -> pure IANInstanceType
        "kernel" -> pure IANKernel
        "productcodes" -> pure IANProductCodes
        "ramdisk" -> pure IANRAMDisk
        "rootdevicename" -> pure IANRootDeviceName
        "sriovnetsupport" -> pure IANSRIOVNetSupport
        "sourcedestcheck" -> pure IANSourceDestCheck
        "userdata" -> pure IANUserData
        e -> fromTextError $ "Failure parsing InstanceAttributeName from value: '" <> e
           <> "'. Accepted values: blockdevicemapping, disableapitermination, ebsoptimized, enasupport, enclaveoptions, groupset, instanceinitiatedshutdownbehavior, instancetype, kernel, productcodes, ramdisk, rootdevicename, sriovnetsupport, sourcedestcheck, userdata"

instance ToText InstanceAttributeName where
    toText = \case
        IANBlockDeviceMapping -> "blockDeviceMapping"
        IANDisableAPITermination -> "disableApiTermination"
        IANEBSOptimized -> "ebsOptimized"
        IANEnaSupport -> "enaSupport"
        IANEnclaveOptions -> "enclaveOptions"
        IANGroupSet -> "groupSet"
        IANInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceType -> "instanceType"
        IANKernel -> "kernel"
        IANProductCodes -> "productCodes"
        IANRAMDisk -> "ramdisk"
        IANRootDeviceName -> "rootDeviceName"
        IANSRIOVNetSupport -> "sriovNetSupport"
        IANSourceDestCheck -> "sourceDestCheck"
        IANUserData -> "userData"

instance Hashable     InstanceAttributeName
instance NFData       InstanceAttributeName
instance ToByteString InstanceAttributeName
instance ToQuery      InstanceAttributeName
instance ToHeader     InstanceAttributeName

data InstanceHealthStatus
  = Healthy
  | Unhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceHealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "unhealthy" -> pure Unhealthy
        e -> fromTextError $ "Failure parsing InstanceHealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy"

instance ToText InstanceHealthStatus where
    toText = \case
        Healthy -> "healthy"
        Unhealthy -> "unhealthy"

instance Hashable     InstanceHealthStatus
instance NFData       InstanceHealthStatus
instance ToByteString InstanceHealthStatus
instance ToQuery      InstanceHealthStatus
instance ToHeader     InstanceHealthStatus

instance FromXML InstanceHealthStatus where
    parseXML = parseXMLText "InstanceHealthStatus"

data InstanceInterruptionBehavior
  = Hibernate
  | Stop
  | Terminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceInterruptionBehavior where
    parser = takeLowerText >>= \case
        "hibernate" -> pure Hibernate
        "stop" -> pure Stop
        "terminate" -> pure Terminate
        e -> fromTextError $ "Failure parsing InstanceInterruptionBehavior from value: '" <> e
           <> "'. Accepted values: hibernate, stop, terminate"

instance ToText InstanceInterruptionBehavior where
    toText = \case
        Hibernate -> "hibernate"
        Stop -> "stop"
        Terminate -> "terminate"

instance Hashable     InstanceInterruptionBehavior
instance NFData       InstanceInterruptionBehavior
instance ToByteString InstanceInterruptionBehavior
instance ToQuery      InstanceInterruptionBehavior
instance ToHeader     InstanceInterruptionBehavior

instance FromXML InstanceInterruptionBehavior where
    parseXML = parseXMLText "InstanceInterruptionBehavior"

data InstanceLifecycle
  = ILOnDemand
  | ILSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceLifecycle where
    parser = takeLowerText >>= \case
        "on-demand" -> pure ILOnDemand
        "spot" -> pure ILSpot
        e -> fromTextError $ "Failure parsing InstanceLifecycle from value: '" <> e
           <> "'. Accepted values: on-demand, spot"

instance ToText InstanceLifecycle where
    toText = \case
        ILOnDemand -> "on-demand"
        ILSpot -> "spot"

instance Hashable     InstanceLifecycle
instance NFData       InstanceLifecycle
instance ToByteString InstanceLifecycle
instance ToQuery      InstanceLifecycle
instance ToHeader     InstanceLifecycle

instance FromXML InstanceLifecycle where
    parseXML = parseXMLText "InstanceLifecycle"

data InstanceLifecycleType
  = ILTScheduled
  | ILTSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceLifecycleType where
    parser = takeLowerText >>= \case
        "scheduled" -> pure ILTScheduled
        "spot" -> pure ILTSpot
        e -> fromTextError $ "Failure parsing InstanceLifecycleType from value: '" <> e
           <> "'. Accepted values: scheduled, spot"

instance ToText InstanceLifecycleType where
    toText = \case
        ILTScheduled -> "scheduled"
        ILTSpot -> "spot"

instance Hashable     InstanceLifecycleType
instance NFData       InstanceLifecycleType
instance ToByteString InstanceLifecycleType
instance ToQuery      InstanceLifecycleType
instance ToHeader     InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"

data InstanceMatchCriteria
  = IMCOpen
  | IMCTargeted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceMatchCriteria where
    parser = takeLowerText >>= \case
        "open" -> pure IMCOpen
        "targeted" -> pure IMCTargeted
        e -> fromTextError $ "Failure parsing InstanceMatchCriteria from value: '" <> e
           <> "'. Accepted values: open, targeted"

instance ToText InstanceMatchCriteria where
    toText = \case
        IMCOpen -> "open"
        IMCTargeted -> "targeted"

instance Hashable     InstanceMatchCriteria
instance NFData       InstanceMatchCriteria
instance ToByteString InstanceMatchCriteria
instance ToQuery      InstanceMatchCriteria
instance ToHeader     InstanceMatchCriteria

instance FromXML InstanceMatchCriteria where
    parseXML = parseXMLText "InstanceMatchCriteria"

data InstanceMetadataEndpointState
  = IMESDisabled
  | IMESEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceMetadataEndpointState where
    parser = takeLowerText >>= \case
        "disabled" -> pure IMESDisabled
        "enabled" -> pure IMESEnabled
        e -> fromTextError $ "Failure parsing InstanceMetadataEndpointState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText InstanceMetadataEndpointState where
    toText = \case
        IMESDisabled -> "disabled"
        IMESEnabled -> "enabled"

instance Hashable     InstanceMetadataEndpointState
instance NFData       InstanceMetadataEndpointState
instance ToByteString InstanceMetadataEndpointState
instance ToQuery      InstanceMetadataEndpointState
instance ToHeader     InstanceMetadataEndpointState

instance FromXML InstanceMetadataEndpointState where
    parseXML = parseXMLText "InstanceMetadataEndpointState"

data InstanceMetadataOptionsState
  = IMOSApplied
  | IMOSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceMetadataOptionsState where
    parser = takeLowerText >>= \case
        "applied" -> pure IMOSApplied
        "pending" -> pure IMOSPending
        e -> fromTextError $ "Failure parsing InstanceMetadataOptionsState from value: '" <> e
           <> "'. Accepted values: applied, pending"

instance ToText InstanceMetadataOptionsState where
    toText = \case
        IMOSApplied -> "applied"
        IMOSPending -> "pending"

instance Hashable     InstanceMetadataOptionsState
instance NFData       InstanceMetadataOptionsState
instance ToByteString InstanceMetadataOptionsState
instance ToQuery      InstanceMetadataOptionsState
instance ToHeader     InstanceMetadataOptionsState

instance FromXML InstanceMetadataOptionsState where
    parseXML = parseXMLText "InstanceMetadataOptionsState"

data InstanceStateName
  = ISNPending
  | ISNRunning
  | ISNShuttingDown
  | ISNStopped
  | ISNStopping
  | ISNTerminated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceStateName where
    parser = takeLowerText >>= \case
        "pending" -> pure ISNPending
        "running" -> pure ISNRunning
        "shutting-down" -> pure ISNShuttingDown
        "stopped" -> pure ISNStopped
        "stopping" -> pure ISNStopping
        "terminated" -> pure ISNTerminated
        e -> fromTextError $ "Failure parsing InstanceStateName from value: '" <> e
           <> "'. Accepted values: pending, running, shutting-down, stopped, stopping, terminated"

instance ToText InstanceStateName where
    toText = \case
        ISNPending -> "pending"
        ISNRunning -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped -> "stopped"
        ISNStopping -> "stopping"
        ISNTerminated -> "terminated"

instance Hashable     InstanceStateName
instance NFData       InstanceStateName
instance ToByteString InstanceStateName
instance ToQuery      InstanceStateName
instance ToHeader     InstanceStateName

instance FromXML InstanceStateName where
    parseXML = parseXMLText "InstanceStateName"

data InstanceType
  = A1_2XLarge
  | A1_4XLarge
  | A1_Large
  | A1_Medium
  | A1_Metal
  | A1_XLarge
  | C1_Medium
  | C1_XLarge
  | C3_2XLarge
  | C3_4XLarge
  | C3_8XLarge
  | C3_Large
  | C3_XLarge
  | C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | C5_12XLarge
  | C5_18XLarge
  | C5_24XLarge
  | C5_2XLarge
  | C5_4XLarge
  | C5_9XLarge
  | C5_Large
  | C5_Metal
  | C5_XLarge
  | C5a_12XLarge
  | C5a_16XLarge
  | C5a_24XLarge
  | C5a_2XLarge
  | C5a_4XLarge
  | C5a_8XLarge
  | C5a_Large
  | C5a_XLarge
  | C5ad_12XLarge
  | C5ad_16XLarge
  | C5ad_24XLarge
  | C5ad_2XLarge
  | C5ad_4XLarge
  | C5ad_8XLarge
  | C5ad_Large
  | C5ad_XLarge
  | C5d_12XLarge
  | C5d_18XLarge
  | C5d_24XLarge
  | C5d_2XLarge
  | C5d_4XLarge
  | C5d_9XLarge
  | C5d_Large
  | C5d_Metal
  | C5d_XLarge
  | C5n_18XLarge
  | C5n_2XLarge
  | C5n_4XLarge
  | C5n_9XLarge
  | C5n_Large
  | C5n_Metal
  | C5n_XLarge
  | C6g_12XLarge
  | C6g_16XLarge
  | C6g_2XLarge
  | C6g_4XLarge
  | C6g_8XLarge
  | C6g_Large
  | C6g_Medium
  | C6g_Metal
  | C6g_XLarge
  | C6gd_12XLarge
  | C6gd_16XLarge
  | C6gd_2XLarge
  | C6gd_4XLarge
  | C6gd_8XLarge
  | C6gd_Large
  | C6gd_Medium
  | C6gd_Metal
  | C6gd_XLarge
  | C6gn_12XLarge
  | C6gn_16XLarge
  | C6gn_2XLarge
  | C6gn_4XLarge
  | C6gn_8XLarge
  | C6gn_Large
  | C6gn_Medium
  | C6gn_XLarge
  | CC1_4XLarge
  | CC2_8XLarge
  | CG1_4XLarge
  | CR1_8XLarge
  | D2_2XLarge
  | D2_4XLarge
  | D2_8XLarge
  | D2_XLarge
  | D3_2XLarge
  | D3_4XLarge
  | D3_8XLarge
  | D3_XLarge
  | D3en_12XLarge
  | D3en_2XLarge
  | D3en_4XLarge
  | D3en_6XLarge
  | D3en_8XLarge
  | D3en_XLarge
  | F1_16XLarge
  | F1_2XLarge
  | F1_4XLarge
  | G2_2XLarge
  | G2_8XLarge
  | G3_16XLarge
  | G3_4XLarge
  | G3_8XLarge
  | G3s_XLarge
  | G4ad_16XLarge
  | G4ad_4XLarge
  | G4ad_8XLarge
  | G4dn_12XLarge
  | G4dn_16XLarge
  | G4dn_2XLarge
  | G4dn_4XLarge
  | G4dn_8XLarge
  | G4dn_Metal
  | G4dn_XLarge
  | H1_16XLarge
  | H1_2XLarge
  | H1_4XLarge
  | H1_8XLarge
  | HI1_4XLarge
  | HS1_8XLarge
  | I2_2XLarge
  | I2_4XLarge
  | I2_8XLarge
  | I2_XLarge
  | I3_16XLarge
  | I3_2XLarge
  | I3_4XLarge
  | I3_8XLarge
  | I3_Large
  | I3_Metal
  | I3_XLarge
  | I3en_12XLarge
  | I3en_24XLarge
  | I3en_2XLarge
  | I3en_3XLarge
  | I3en_6XLarge
  | I3en_Large
  | I3en_Metal
  | I3en_XLarge
  | INF1_24XLarge
  | INF1_2XLarge
  | INF1_6XLarge
  | INF1_XLarge
  | M1_Large
  | M1_Medium
  | M1_Small
  | M1_XLarge
  | M2_2XLarge
  | M2_4XLarge
  | M2_XLarge
  | M3_2XLarge
  | M3_Large
  | M3_Medium
  | M3_XLarge
  | M4_10XLarge
  | M4_16XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | M5_12XLarge
  | M5_16XLarge
  | M5_24XLarge
  | M5_2XLarge
  | M5_4XLarge
  | M5_8XLarge
  | M5_Large
  | M5_Metal
  | M5_XLarge
  | M5a_12XLarge
  | M5a_16XLarge
  | M5a_24XLarge
  | M5a_2XLarge
  | M5a_4XLarge
  | M5a_8XLarge
  | M5a_Large
  | M5a_XLarge
  | M5ad_12XLarge
  | M5ad_16XLarge
  | M5ad_24XLarge
  | M5ad_2XLarge
  | M5ad_4XLarge
  | M5ad_8XLarge
  | M5ad_Large
  | M5ad_XLarge
  | M5d_12XLarge
  | M5d_16XLarge
  | M5d_24XLarge
  | M5d_2XLarge
  | M5d_4XLarge
  | M5d_8XLarge
  | M5d_Large
  | M5d_Metal
  | M5d_XLarge
  | M5dn_12XLarge
  | M5dn_16XLarge
  | M5dn_24XLarge
  | M5dn_2XLarge
  | M5dn_4XLarge
  | M5dn_8XLarge
  | M5dn_Large
  | M5dn_XLarge
  | M5n_12XLarge
  | M5n_16XLarge
  | M5n_24XLarge
  | M5n_2XLarge
  | M5n_4XLarge
  | M5n_8XLarge
  | M5n_Large
  | M5n_XLarge
  | M5zn_12XLarge
  | M5zn_2XLarge
  | M5zn_3XLarge
  | M5zn_6XLarge
  | M5zn_Large
  | M5zn_Metal
  | M5zn_XLarge
  | M6g_12XLarge
  | M6g_16XLarge
  | M6g_2XLarge
  | M6g_4XLarge
  | M6g_8XLarge
  | M6g_Large
  | M6g_Medium
  | M6g_Metal
  | M6g_XLarge
  | M6gd_12XLarge
  | M6gd_16XLarge
  | M6gd_2XLarge
  | M6gd_4XLarge
  | M6gd_8XLarge
  | M6gd_Large
  | M6gd_Medium
  | M6gd_Metal
  | M6gd_XLarge
  | MAC1_Metal
  | P2_16XLarge
  | P2_8XLarge
  | P2_XLarge
  | P3_16XLarge
  | P3_2XLarge
  | P3_8XLarge
  | P3dn_24XLarge
  | P4d_24XLarge
  | R3_2XLarge
  | R3_4XLarge
  | R3_8XLarge
  | R3_Large
  | R3_XLarge
  | R4_16XLarge
  | R4_2XLarge
  | R4_4XLarge
  | R4_8XLarge
  | R4_Large
  | R4_XLarge
  | R5_12XLarge
  | R5_16XLarge
  | R5_24XLarge
  | R5_2XLarge
  | R5_4XLarge
  | R5_8XLarge
  | R5_Large
  | R5_Metal
  | R5_XLarge
  | R5a_12XLarge
  | R5a_16XLarge
  | R5a_24XLarge
  | R5a_2XLarge
  | R5a_4XLarge
  | R5a_8XLarge
  | R5a_Large
  | R5a_XLarge
  | R5ad_12XLarge
  | R5ad_16XLarge
  | R5ad_24XLarge
  | R5ad_2XLarge
  | R5ad_4XLarge
  | R5ad_8XLarge
  | R5ad_Large
  | R5ad_XLarge
  | R5b_12XLarge
  | R5b_16XLarge
  | R5b_24XLarge
  | R5b_2XLarge
  | R5b_4XLarge
  | R5b_8XLarge
  | R5b_Large
  | R5b_Metal
  | R5b_XLarge
  | R5d_12XLarge
  | R5d_16XLarge
  | R5d_24XLarge
  | R5d_2XLarge
  | R5d_4XLarge
  | R5d_8XLarge
  | R5d_Large
  | R5d_Metal
  | R5d_XLarge
  | R5dn_12XLarge
  | R5dn_16XLarge
  | R5dn_24XLarge
  | R5dn_2XLarge
  | R5dn_4XLarge
  | R5dn_8XLarge
  | R5dn_Large
  | R5dn_XLarge
  | R5n_12XLarge
  | R5n_16XLarge
  | R5n_24XLarge
  | R5n_2XLarge
  | R5n_4XLarge
  | R5n_8XLarge
  | R5n_Large
  | R5n_XLarge
  | R6g_12XLarge
  | R6g_16XLarge
  | R6g_2XLarge
  | R6g_4XLarge
  | R6g_8XLarge
  | R6g_Large
  | R6g_Medium
  | R6g_Metal
  | R6g_XLarge
  | R6gd_12XLarge
  | R6gd_16XLarge
  | R6gd_2XLarge
  | R6gd_4XLarge
  | R6gd_8XLarge
  | R6gd_Large
  | R6gd_Medium
  | R6gd_Metal
  | R6gd_XLarge
  | T1_Micro
  | T2_2XLarge
  | T2_Large
  | T2_Medium
  | T2_Micro
  | T2_Nano
  | T2_Small
  | T2_XLarge
  | T3_2XLarge
  | T3_Large
  | T3_Medium
  | T3_Micro
  | T3_Nano
  | T3_Small
  | T3_XLarge
  | T3a_2XLarge
  | T3a_Large
  | T3a_Medium
  | T3a_Micro
  | T3a_Nano
  | T3a_Small
  | T3a_XLarge
  | T4g_2XLarge
  | T4g_Large
  | T4g_Medium
  | T4g_Micro
  | T4g_Nano
  | T4g_Small
  | T4g_XLarge
  | U12TB1_Metal
  | U18TB1_Metal
  | U24TB1_Metal
  | U6TB1_Metal
  | U9TB1_Metal
  | X1_16XLarge
  | X1_32XLarge
  | X1e_16XLarge
  | X1e_2XLarge
  | X1e_32XLarge
  | X1e_4XLarge
  | X1e_8XLarge
  | X1e_XLarge
  | Z1d_12XLarge
  | Z1d_2XLarge
  | Z1d_3XLarge
  | Z1d_6XLarge
  | Z1d_Large
  | Z1d_Metal
  | Z1d_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "a1.2xlarge" -> pure A1_2XLarge
        "a1.4xlarge" -> pure A1_4XLarge
        "a1.large" -> pure A1_Large
        "a1.medium" -> pure A1_Medium
        "a1.metal" -> pure A1_Metal
        "a1.xlarge" -> pure A1_XLarge
        "c1.medium" -> pure C1_Medium
        "c1.xlarge" -> pure C1_XLarge
        "c3.2xlarge" -> pure C3_2XLarge
        "c3.4xlarge" -> pure C3_4XLarge
        "c3.8xlarge" -> pure C3_8XLarge
        "c3.large" -> pure C3_Large
        "c3.xlarge" -> pure C3_XLarge
        "c4.2xlarge" -> pure C4_2XLarge
        "c4.4xlarge" -> pure C4_4XLarge
        "c4.8xlarge" -> pure C4_8XLarge
        "c4.large" -> pure C4_Large
        "c4.xlarge" -> pure C4_XLarge
        "c5.12xlarge" -> pure C5_12XLarge
        "c5.18xlarge" -> pure C5_18XLarge
        "c5.24xlarge" -> pure C5_24XLarge
        "c5.2xlarge" -> pure C5_2XLarge
        "c5.4xlarge" -> pure C5_4XLarge
        "c5.9xlarge" -> pure C5_9XLarge
        "c5.large" -> pure C5_Large
        "c5.metal" -> pure C5_Metal
        "c5.xlarge" -> pure C5_XLarge
        "c5a.12xlarge" -> pure C5a_12XLarge
        "c5a.16xlarge" -> pure C5a_16XLarge
        "c5a.24xlarge" -> pure C5a_24XLarge
        "c5a.2xlarge" -> pure C5a_2XLarge
        "c5a.4xlarge" -> pure C5a_4XLarge
        "c5a.8xlarge" -> pure C5a_8XLarge
        "c5a.large" -> pure C5a_Large
        "c5a.xlarge" -> pure C5a_XLarge
        "c5ad.12xlarge" -> pure C5ad_12XLarge
        "c5ad.16xlarge" -> pure C5ad_16XLarge
        "c5ad.24xlarge" -> pure C5ad_24XLarge
        "c5ad.2xlarge" -> pure C5ad_2XLarge
        "c5ad.4xlarge" -> pure C5ad_4XLarge
        "c5ad.8xlarge" -> pure C5ad_8XLarge
        "c5ad.large" -> pure C5ad_Large
        "c5ad.xlarge" -> pure C5ad_XLarge
        "c5d.12xlarge" -> pure C5d_12XLarge
        "c5d.18xlarge" -> pure C5d_18XLarge
        "c5d.24xlarge" -> pure C5d_24XLarge
        "c5d.2xlarge" -> pure C5d_2XLarge
        "c5d.4xlarge" -> pure C5d_4XLarge
        "c5d.9xlarge" -> pure C5d_9XLarge
        "c5d.large" -> pure C5d_Large
        "c5d.metal" -> pure C5d_Metal
        "c5d.xlarge" -> pure C5d_XLarge
        "c5n.18xlarge" -> pure C5n_18XLarge
        "c5n.2xlarge" -> pure C5n_2XLarge
        "c5n.4xlarge" -> pure C5n_4XLarge
        "c5n.9xlarge" -> pure C5n_9XLarge
        "c5n.large" -> pure C5n_Large
        "c5n.metal" -> pure C5n_Metal
        "c5n.xlarge" -> pure C5n_XLarge
        "c6g.12xlarge" -> pure C6g_12XLarge
        "c6g.16xlarge" -> pure C6g_16XLarge
        "c6g.2xlarge" -> pure C6g_2XLarge
        "c6g.4xlarge" -> pure C6g_4XLarge
        "c6g.8xlarge" -> pure C6g_8XLarge
        "c6g.large" -> pure C6g_Large
        "c6g.medium" -> pure C6g_Medium
        "c6g.metal" -> pure C6g_Metal
        "c6g.xlarge" -> pure C6g_XLarge
        "c6gd.12xlarge" -> pure C6gd_12XLarge
        "c6gd.16xlarge" -> pure C6gd_16XLarge
        "c6gd.2xlarge" -> pure C6gd_2XLarge
        "c6gd.4xlarge" -> pure C6gd_4XLarge
        "c6gd.8xlarge" -> pure C6gd_8XLarge
        "c6gd.large" -> pure C6gd_Large
        "c6gd.medium" -> pure C6gd_Medium
        "c6gd.metal" -> pure C6gd_Metal
        "c6gd.xlarge" -> pure C6gd_XLarge
        "c6gn.12xlarge" -> pure C6gn_12XLarge
        "c6gn.16xlarge" -> pure C6gn_16XLarge
        "c6gn.2xlarge" -> pure C6gn_2XLarge
        "c6gn.4xlarge" -> pure C6gn_4XLarge
        "c6gn.8xlarge" -> pure C6gn_8XLarge
        "c6gn.large" -> pure C6gn_Large
        "c6gn.medium" -> pure C6gn_Medium
        "c6gn.xlarge" -> pure C6gn_XLarge
        "cc1.4xlarge" -> pure CC1_4XLarge
        "cc2.8xlarge" -> pure CC2_8XLarge
        "cg1.4xlarge" -> pure CG1_4XLarge
        "cr1.8xlarge" -> pure CR1_8XLarge
        "d2.2xlarge" -> pure D2_2XLarge
        "d2.4xlarge" -> pure D2_4XLarge
        "d2.8xlarge" -> pure D2_8XLarge
        "d2.xlarge" -> pure D2_XLarge
        "d3.2xlarge" -> pure D3_2XLarge
        "d3.4xlarge" -> pure D3_4XLarge
        "d3.8xlarge" -> pure D3_8XLarge
        "d3.xlarge" -> pure D3_XLarge
        "d3en.12xlarge" -> pure D3en_12XLarge
        "d3en.2xlarge" -> pure D3en_2XLarge
        "d3en.4xlarge" -> pure D3en_4XLarge
        "d3en.6xlarge" -> pure D3en_6XLarge
        "d3en.8xlarge" -> pure D3en_8XLarge
        "d3en.xlarge" -> pure D3en_XLarge
        "f1.16xlarge" -> pure F1_16XLarge
        "f1.2xlarge" -> pure F1_2XLarge
        "f1.4xlarge" -> pure F1_4XLarge
        "g2.2xlarge" -> pure G2_2XLarge
        "g2.8xlarge" -> pure G2_8XLarge
        "g3.16xlarge" -> pure G3_16XLarge
        "g3.4xlarge" -> pure G3_4XLarge
        "g3.8xlarge" -> pure G3_8XLarge
        "g3s.xlarge" -> pure G3s_XLarge
        "g4ad.16xlarge" -> pure G4ad_16XLarge
        "g4ad.4xlarge" -> pure G4ad_4XLarge
        "g4ad.8xlarge" -> pure G4ad_8XLarge
        "g4dn.12xlarge" -> pure G4dn_12XLarge
        "g4dn.16xlarge" -> pure G4dn_16XLarge
        "g4dn.2xlarge" -> pure G4dn_2XLarge
        "g4dn.4xlarge" -> pure G4dn_4XLarge
        "g4dn.8xlarge" -> pure G4dn_8XLarge
        "g4dn.metal" -> pure G4dn_Metal
        "g4dn.xlarge" -> pure G4dn_XLarge
        "h1.16xlarge" -> pure H1_16XLarge
        "h1.2xlarge" -> pure H1_2XLarge
        "h1.4xlarge" -> pure H1_4XLarge
        "h1.8xlarge" -> pure H1_8XLarge
        "hi1.4xlarge" -> pure HI1_4XLarge
        "hs1.8xlarge" -> pure HS1_8XLarge
        "i2.2xlarge" -> pure I2_2XLarge
        "i2.4xlarge" -> pure I2_4XLarge
        "i2.8xlarge" -> pure I2_8XLarge
        "i2.xlarge" -> pure I2_XLarge
        "i3.16xlarge" -> pure I3_16XLarge
        "i3.2xlarge" -> pure I3_2XLarge
        "i3.4xlarge" -> pure I3_4XLarge
        "i3.8xlarge" -> pure I3_8XLarge
        "i3.large" -> pure I3_Large
        "i3.metal" -> pure I3_Metal
        "i3.xlarge" -> pure I3_XLarge
        "i3en.12xlarge" -> pure I3en_12XLarge
        "i3en.24xlarge" -> pure I3en_24XLarge
        "i3en.2xlarge" -> pure I3en_2XLarge
        "i3en.3xlarge" -> pure I3en_3XLarge
        "i3en.6xlarge" -> pure I3en_6XLarge
        "i3en.large" -> pure I3en_Large
        "i3en.metal" -> pure I3en_Metal
        "i3en.xlarge" -> pure I3en_XLarge
        "inf1.24xlarge" -> pure INF1_24XLarge
        "inf1.2xlarge" -> pure INF1_2XLarge
        "inf1.6xlarge" -> pure INF1_6XLarge
        "inf1.xlarge" -> pure INF1_XLarge
        "m1.large" -> pure M1_Large
        "m1.medium" -> pure M1_Medium
        "m1.small" -> pure M1_Small
        "m1.xlarge" -> pure M1_XLarge
        "m2.2xlarge" -> pure M2_2XLarge
        "m2.4xlarge" -> pure M2_4XLarge
        "m2.xlarge" -> pure M2_XLarge
        "m3.2xlarge" -> pure M3_2XLarge
        "m3.large" -> pure M3_Large
        "m3.medium" -> pure M3_Medium
        "m3.xlarge" -> pure M3_XLarge
        "m4.10xlarge" -> pure M4_10XLarge
        "m4.16xlarge" -> pure M4_16XLarge
        "m4.2xlarge" -> pure M4_2XLarge
        "m4.4xlarge" -> pure M4_4XLarge
        "m4.large" -> pure M4_Large
        "m4.xlarge" -> pure M4_XLarge
        "m5.12xlarge" -> pure M5_12XLarge
        "m5.16xlarge" -> pure M5_16XLarge
        "m5.24xlarge" -> pure M5_24XLarge
        "m5.2xlarge" -> pure M5_2XLarge
        "m5.4xlarge" -> pure M5_4XLarge
        "m5.8xlarge" -> pure M5_8XLarge
        "m5.large" -> pure M5_Large
        "m5.metal" -> pure M5_Metal
        "m5.xlarge" -> pure M5_XLarge
        "m5a.12xlarge" -> pure M5a_12XLarge
        "m5a.16xlarge" -> pure M5a_16XLarge
        "m5a.24xlarge" -> pure M5a_24XLarge
        "m5a.2xlarge" -> pure M5a_2XLarge
        "m5a.4xlarge" -> pure M5a_4XLarge
        "m5a.8xlarge" -> pure M5a_8XLarge
        "m5a.large" -> pure M5a_Large
        "m5a.xlarge" -> pure M5a_XLarge
        "m5ad.12xlarge" -> pure M5ad_12XLarge
        "m5ad.16xlarge" -> pure M5ad_16XLarge
        "m5ad.24xlarge" -> pure M5ad_24XLarge
        "m5ad.2xlarge" -> pure M5ad_2XLarge
        "m5ad.4xlarge" -> pure M5ad_4XLarge
        "m5ad.8xlarge" -> pure M5ad_8XLarge
        "m5ad.large" -> pure M5ad_Large
        "m5ad.xlarge" -> pure M5ad_XLarge
        "m5d.12xlarge" -> pure M5d_12XLarge
        "m5d.16xlarge" -> pure M5d_16XLarge
        "m5d.24xlarge" -> pure M5d_24XLarge
        "m5d.2xlarge" -> pure M5d_2XLarge
        "m5d.4xlarge" -> pure M5d_4XLarge
        "m5d.8xlarge" -> pure M5d_8XLarge
        "m5d.large" -> pure M5d_Large
        "m5d.metal" -> pure M5d_Metal
        "m5d.xlarge" -> pure M5d_XLarge
        "m5dn.12xlarge" -> pure M5dn_12XLarge
        "m5dn.16xlarge" -> pure M5dn_16XLarge
        "m5dn.24xlarge" -> pure M5dn_24XLarge
        "m5dn.2xlarge" -> pure M5dn_2XLarge
        "m5dn.4xlarge" -> pure M5dn_4XLarge
        "m5dn.8xlarge" -> pure M5dn_8XLarge
        "m5dn.large" -> pure M5dn_Large
        "m5dn.xlarge" -> pure M5dn_XLarge
        "m5n.12xlarge" -> pure M5n_12XLarge
        "m5n.16xlarge" -> pure M5n_16XLarge
        "m5n.24xlarge" -> pure M5n_24XLarge
        "m5n.2xlarge" -> pure M5n_2XLarge
        "m5n.4xlarge" -> pure M5n_4XLarge
        "m5n.8xlarge" -> pure M5n_8XLarge
        "m5n.large" -> pure M5n_Large
        "m5n.xlarge" -> pure M5n_XLarge
        "m5zn.12xlarge" -> pure M5zn_12XLarge
        "m5zn.2xlarge" -> pure M5zn_2XLarge
        "m5zn.3xlarge" -> pure M5zn_3XLarge
        "m5zn.6xlarge" -> pure M5zn_6XLarge
        "m5zn.large" -> pure M5zn_Large
        "m5zn.metal" -> pure M5zn_Metal
        "m5zn.xlarge" -> pure M5zn_XLarge
        "m6g.12xlarge" -> pure M6g_12XLarge
        "m6g.16xlarge" -> pure M6g_16XLarge
        "m6g.2xlarge" -> pure M6g_2XLarge
        "m6g.4xlarge" -> pure M6g_4XLarge
        "m6g.8xlarge" -> pure M6g_8XLarge
        "m6g.large" -> pure M6g_Large
        "m6g.medium" -> pure M6g_Medium
        "m6g.metal" -> pure M6g_Metal
        "m6g.xlarge" -> pure M6g_XLarge
        "m6gd.12xlarge" -> pure M6gd_12XLarge
        "m6gd.16xlarge" -> pure M6gd_16XLarge
        "m6gd.2xlarge" -> pure M6gd_2XLarge
        "m6gd.4xlarge" -> pure M6gd_4XLarge
        "m6gd.8xlarge" -> pure M6gd_8XLarge
        "m6gd.large" -> pure M6gd_Large
        "m6gd.medium" -> pure M6gd_Medium
        "m6gd.metal" -> pure M6gd_Metal
        "m6gd.xlarge" -> pure M6gd_XLarge
        "mac1.metal" -> pure MAC1_Metal
        "p2.16xlarge" -> pure P2_16XLarge
        "p2.8xlarge" -> pure P2_8XLarge
        "p2.xlarge" -> pure P2_XLarge
        "p3.16xlarge" -> pure P3_16XLarge
        "p3.2xlarge" -> pure P3_2XLarge
        "p3.8xlarge" -> pure P3_8XLarge
        "p3dn.24xlarge" -> pure P3dn_24XLarge
        "p4d.24xlarge" -> pure P4d_24XLarge
        "r3.2xlarge" -> pure R3_2XLarge
        "r3.4xlarge" -> pure R3_4XLarge
        "r3.8xlarge" -> pure R3_8XLarge
        "r3.large" -> pure R3_Large
        "r3.xlarge" -> pure R3_XLarge
        "r4.16xlarge" -> pure R4_16XLarge
        "r4.2xlarge" -> pure R4_2XLarge
        "r4.4xlarge" -> pure R4_4XLarge
        "r4.8xlarge" -> pure R4_8XLarge
        "r4.large" -> pure R4_Large
        "r4.xlarge" -> pure R4_XLarge
        "r5.12xlarge" -> pure R5_12XLarge
        "r5.16xlarge" -> pure R5_16XLarge
        "r5.24xlarge" -> pure R5_24XLarge
        "r5.2xlarge" -> pure R5_2XLarge
        "r5.4xlarge" -> pure R5_4XLarge
        "r5.8xlarge" -> pure R5_8XLarge
        "r5.large" -> pure R5_Large
        "r5.metal" -> pure R5_Metal
        "r5.xlarge" -> pure R5_XLarge
        "r5a.12xlarge" -> pure R5a_12XLarge
        "r5a.16xlarge" -> pure R5a_16XLarge
        "r5a.24xlarge" -> pure R5a_24XLarge
        "r5a.2xlarge" -> pure R5a_2XLarge
        "r5a.4xlarge" -> pure R5a_4XLarge
        "r5a.8xlarge" -> pure R5a_8XLarge
        "r5a.large" -> pure R5a_Large
        "r5a.xlarge" -> pure R5a_XLarge
        "r5ad.12xlarge" -> pure R5ad_12XLarge
        "r5ad.16xlarge" -> pure R5ad_16XLarge
        "r5ad.24xlarge" -> pure R5ad_24XLarge
        "r5ad.2xlarge" -> pure R5ad_2XLarge
        "r5ad.4xlarge" -> pure R5ad_4XLarge
        "r5ad.8xlarge" -> pure R5ad_8XLarge
        "r5ad.large" -> pure R5ad_Large
        "r5ad.xlarge" -> pure R5ad_XLarge
        "r5b.12xlarge" -> pure R5b_12XLarge
        "r5b.16xlarge" -> pure R5b_16XLarge
        "r5b.24xlarge" -> pure R5b_24XLarge
        "r5b.2xlarge" -> pure R5b_2XLarge
        "r5b.4xlarge" -> pure R5b_4XLarge
        "r5b.8xlarge" -> pure R5b_8XLarge
        "r5b.large" -> pure R5b_Large
        "r5b.metal" -> pure R5b_Metal
        "r5b.xlarge" -> pure R5b_XLarge
        "r5d.12xlarge" -> pure R5d_12XLarge
        "r5d.16xlarge" -> pure R5d_16XLarge
        "r5d.24xlarge" -> pure R5d_24XLarge
        "r5d.2xlarge" -> pure R5d_2XLarge
        "r5d.4xlarge" -> pure R5d_4XLarge
        "r5d.8xlarge" -> pure R5d_8XLarge
        "r5d.large" -> pure R5d_Large
        "r5d.metal" -> pure R5d_Metal
        "r5d.xlarge" -> pure R5d_XLarge
        "r5dn.12xlarge" -> pure R5dn_12XLarge
        "r5dn.16xlarge" -> pure R5dn_16XLarge
        "r5dn.24xlarge" -> pure R5dn_24XLarge
        "r5dn.2xlarge" -> pure R5dn_2XLarge
        "r5dn.4xlarge" -> pure R5dn_4XLarge
        "r5dn.8xlarge" -> pure R5dn_8XLarge
        "r5dn.large" -> pure R5dn_Large
        "r5dn.xlarge" -> pure R5dn_XLarge
        "r5n.12xlarge" -> pure R5n_12XLarge
        "r5n.16xlarge" -> pure R5n_16XLarge
        "r5n.24xlarge" -> pure R5n_24XLarge
        "r5n.2xlarge" -> pure R5n_2XLarge
        "r5n.4xlarge" -> pure R5n_4XLarge
        "r5n.8xlarge" -> pure R5n_8XLarge
        "r5n.large" -> pure R5n_Large
        "r5n.xlarge" -> pure R5n_XLarge
        "r6g.12xlarge" -> pure R6g_12XLarge
        "r6g.16xlarge" -> pure R6g_16XLarge
        "r6g.2xlarge" -> pure R6g_2XLarge
        "r6g.4xlarge" -> pure R6g_4XLarge
        "r6g.8xlarge" -> pure R6g_8XLarge
        "r6g.large" -> pure R6g_Large
        "r6g.medium" -> pure R6g_Medium
        "r6g.metal" -> pure R6g_Metal
        "r6g.xlarge" -> pure R6g_XLarge
        "r6gd.12xlarge" -> pure R6gd_12XLarge
        "r6gd.16xlarge" -> pure R6gd_16XLarge
        "r6gd.2xlarge" -> pure R6gd_2XLarge
        "r6gd.4xlarge" -> pure R6gd_4XLarge
        "r6gd.8xlarge" -> pure R6gd_8XLarge
        "r6gd.large" -> pure R6gd_Large
        "r6gd.medium" -> pure R6gd_Medium
        "r6gd.metal" -> pure R6gd_Metal
        "r6gd.xlarge" -> pure R6gd_XLarge
        "t1.micro" -> pure T1_Micro
        "t2.2xlarge" -> pure T2_2XLarge
        "t2.large" -> pure T2_Large
        "t2.medium" -> pure T2_Medium
        "t2.micro" -> pure T2_Micro
        "t2.nano" -> pure T2_Nano
        "t2.small" -> pure T2_Small
        "t2.xlarge" -> pure T2_XLarge
        "t3.2xlarge" -> pure T3_2XLarge
        "t3.large" -> pure T3_Large
        "t3.medium" -> pure T3_Medium
        "t3.micro" -> pure T3_Micro
        "t3.nano" -> pure T3_Nano
        "t3.small" -> pure T3_Small
        "t3.xlarge" -> pure T3_XLarge
        "t3a.2xlarge" -> pure T3a_2XLarge
        "t3a.large" -> pure T3a_Large
        "t3a.medium" -> pure T3a_Medium
        "t3a.micro" -> pure T3a_Micro
        "t3a.nano" -> pure T3a_Nano
        "t3a.small" -> pure T3a_Small
        "t3a.xlarge" -> pure T3a_XLarge
        "t4g.2xlarge" -> pure T4g_2XLarge
        "t4g.large" -> pure T4g_Large
        "t4g.medium" -> pure T4g_Medium
        "t4g.micro" -> pure T4g_Micro
        "t4g.nano" -> pure T4g_Nano
        "t4g.small" -> pure T4g_Small
        "t4g.xlarge" -> pure T4g_XLarge
        "u-12tb1.metal" -> pure U12TB1_Metal
        "u-18tb1.metal" -> pure U18TB1_Metal
        "u-24tb1.metal" -> pure U24TB1_Metal
        "u-6tb1.metal" -> pure U6TB1_Metal
        "u-9tb1.metal" -> pure U9TB1_Metal
        "x1.16xlarge" -> pure X1_16XLarge
        "x1.32xlarge" -> pure X1_32XLarge
        "x1e.16xlarge" -> pure X1e_16XLarge
        "x1e.2xlarge" -> pure X1e_2XLarge
        "x1e.32xlarge" -> pure X1e_32XLarge
        "x1e.4xlarge" -> pure X1e_4XLarge
        "x1e.8xlarge" -> pure X1e_8XLarge
        "x1e.xlarge" -> pure X1e_XLarge
        "z1d.12xlarge" -> pure Z1d_12XLarge
        "z1d.2xlarge" -> pure Z1d_2XLarge
        "z1d.3xlarge" -> pure Z1d_3XLarge
        "z1d.6xlarge" -> pure Z1d_6XLarge
        "z1d.large" -> pure Z1d_Large
        "z1d.metal" -> pure Z1d_Metal
        "z1d.xlarge" -> pure Z1d_XLarge
        e -> fromTextError $ "Failure parsing InstanceType from value: '" <> e
           <> "'. Accepted values: a1.2xlarge, a1.4xlarge, a1.large, a1.medium, a1.metal, a1.xlarge, c1.medium, c1.xlarge, c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, c5.12xlarge, c5.18xlarge, c5.24xlarge, c5.2xlarge, c5.4xlarge, c5.9xlarge, c5.large, c5.metal, c5.xlarge, c5a.12xlarge, c5a.16xlarge, c5a.24xlarge, c5a.2xlarge, c5a.4xlarge, c5a.8xlarge, c5a.large, c5a.xlarge, c5ad.12xlarge, c5ad.16xlarge, c5ad.24xlarge, c5ad.2xlarge, c5ad.4xlarge, c5ad.8xlarge, c5ad.large, c5ad.xlarge, c5d.12xlarge, c5d.18xlarge, c5d.24xlarge, c5d.2xlarge, c5d.4xlarge, c5d.9xlarge, c5d.large, c5d.metal, c5d.xlarge, c5n.18xlarge, c5n.2xlarge, c5n.4xlarge, c5n.9xlarge, c5n.large, c5n.metal, c5n.xlarge, c6g.12xlarge, c6g.16xlarge, c6g.2xlarge, c6g.4xlarge, c6g.8xlarge, c6g.large, c6g.medium, c6g.metal, c6g.xlarge, c6gd.12xlarge, c6gd.16xlarge, c6gd.2xlarge, c6gd.4xlarge, c6gd.8xlarge, c6gd.large, c6gd.medium, c6gd.metal, c6gd.xlarge, c6gn.12xlarge, c6gn.16xlarge, c6gn.2xlarge, c6gn.4xlarge, c6gn.8xlarge, c6gn.large, c6gn.medium, c6gn.xlarge, cc1.4xlarge, cc2.8xlarge, cg1.4xlarge, cr1.8xlarge, d2.2xlarge, d2.4xlarge, d2.8xlarge, d2.xlarge, d3.2xlarge, d3.4xlarge, d3.8xlarge, d3.xlarge, d3en.12xlarge, d3en.2xlarge, d3en.4xlarge, d3en.6xlarge, d3en.8xlarge, d3en.xlarge, f1.16xlarge, f1.2xlarge, f1.4xlarge, g2.2xlarge, g2.8xlarge, g3.16xlarge, g3.4xlarge, g3.8xlarge, g3s.xlarge, g4ad.16xlarge, g4ad.4xlarge, g4ad.8xlarge, g4dn.12xlarge, g4dn.16xlarge, g4dn.2xlarge, g4dn.4xlarge, g4dn.8xlarge, g4dn.metal, g4dn.xlarge, h1.16xlarge, h1.2xlarge, h1.4xlarge, h1.8xlarge, hi1.4xlarge, hs1.8xlarge, i2.2xlarge, i2.4xlarge, i2.8xlarge, i2.xlarge, i3.16xlarge, i3.2xlarge, i3.4xlarge, i3.8xlarge, i3.large, i3.metal, i3.xlarge, i3en.12xlarge, i3en.24xlarge, i3en.2xlarge, i3en.3xlarge, i3en.6xlarge, i3en.large, i3en.metal, i3en.xlarge, inf1.24xlarge, inf1.2xlarge, inf1.6xlarge, inf1.xlarge, m1.large, m1.medium, m1.small, m1.xlarge, m2.2xlarge, m2.4xlarge, m2.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.16xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, m5.12xlarge, m5.16xlarge, m5.24xlarge, m5.2xlarge, m5.4xlarge, m5.8xlarge, m5.large, m5.metal, m5.xlarge, m5a.12xlarge, m5a.16xlarge, m5a.24xlarge, m5a.2xlarge, m5a.4xlarge, m5a.8xlarge, m5a.large, m5a.xlarge, m5ad.12xlarge, m5ad.16xlarge, m5ad.24xlarge, m5ad.2xlarge, m5ad.4xlarge, m5ad.8xlarge, m5ad.large, m5ad.xlarge, m5d.12xlarge, m5d.16xlarge, m5d.24xlarge, m5d.2xlarge, m5d.4xlarge, m5d.8xlarge, m5d.large, m5d.metal, m5d.xlarge, m5dn.12xlarge, m5dn.16xlarge, m5dn.24xlarge, m5dn.2xlarge, m5dn.4xlarge, m5dn.8xlarge, m5dn.large, m5dn.xlarge, m5n.12xlarge, m5n.16xlarge, m5n.24xlarge, m5n.2xlarge, m5n.4xlarge, m5n.8xlarge, m5n.large, m5n.xlarge, m5zn.12xlarge, m5zn.2xlarge, m5zn.3xlarge, m5zn.6xlarge, m5zn.large, m5zn.metal, m5zn.xlarge, m6g.12xlarge, m6g.16xlarge, m6g.2xlarge, m6g.4xlarge, m6g.8xlarge, m6g.large, m6g.medium, m6g.metal, m6g.xlarge, m6gd.12xlarge, m6gd.16xlarge, m6gd.2xlarge, m6gd.4xlarge, m6gd.8xlarge, m6gd.large, m6gd.medium, m6gd.metal, m6gd.xlarge, mac1.metal, p2.16xlarge, p2.8xlarge, p2.xlarge, p3.16xlarge, p3.2xlarge, p3.8xlarge, p3dn.24xlarge, p4d.24xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, r5.12xlarge, r5.16xlarge, r5.24xlarge, r5.2xlarge, r5.4xlarge, r5.8xlarge, r5.large, r5.metal, r5.xlarge, r5a.12xlarge, r5a.16xlarge, r5a.24xlarge, r5a.2xlarge, r5a.4xlarge, r5a.8xlarge, r5a.large, r5a.xlarge, r5ad.12xlarge, r5ad.16xlarge, r5ad.24xlarge, r5ad.2xlarge, r5ad.4xlarge, r5ad.8xlarge, r5ad.large, r5ad.xlarge, r5b.12xlarge, r5b.16xlarge, r5b.24xlarge, r5b.2xlarge, r5b.4xlarge, r5b.8xlarge, r5b.large, r5b.metal, r5b.xlarge, r5d.12xlarge, r5d.16xlarge, r5d.24xlarge, r5d.2xlarge, r5d.4xlarge, r5d.8xlarge, r5d.large, r5d.metal, r5d.xlarge, r5dn.12xlarge, r5dn.16xlarge, r5dn.24xlarge, r5dn.2xlarge, r5dn.4xlarge, r5dn.8xlarge, r5dn.large, r5dn.xlarge, r5n.12xlarge, r5n.16xlarge, r5n.24xlarge, r5n.2xlarge, r5n.4xlarge, r5n.8xlarge, r5n.large, r5n.xlarge, r6g.12xlarge, r6g.16xlarge, r6g.2xlarge, r6g.4xlarge, r6g.8xlarge, r6g.large, r6g.medium, r6g.metal, r6g.xlarge, r6gd.12xlarge, r6gd.16xlarge, r6gd.2xlarge, r6gd.4xlarge, r6gd.8xlarge, r6gd.large, r6gd.medium, r6gd.metal, r6gd.xlarge, t1.micro, t2.2xlarge, t2.large, t2.medium, t2.micro, t2.nano, t2.small, t2.xlarge, t3.2xlarge, t3.large, t3.medium, t3.micro, t3.nano, t3.small, t3.xlarge, t3a.2xlarge, t3a.large, t3a.medium, t3a.micro, t3a.nano, t3a.small, t3a.xlarge, t4g.2xlarge, t4g.large, t4g.medium, t4g.micro, t4g.nano, t4g.small, t4g.xlarge, u-12tb1.metal, u-18tb1.metal, u-24tb1.metal, u-6tb1.metal, u-9tb1.metal, x1.16xlarge, x1.32xlarge, x1e.16xlarge, x1e.2xlarge, x1e.32xlarge, x1e.4xlarge, x1e.8xlarge, x1e.xlarge, z1d.12xlarge, z1d.2xlarge, z1d.3xlarge, z1d.6xlarge, z1d.large, z1d.metal, z1d.xlarge"

instance ToText InstanceType where
    toText = \case
        A1_2XLarge -> "a1.2xlarge"
        A1_4XLarge -> "a1.4xlarge"
        A1_Large -> "a1.large"
        A1_Medium -> "a1.medium"
        A1_Metal -> "a1.metal"
        A1_XLarge -> "a1.xlarge"
        C1_Medium -> "c1.medium"
        C1_XLarge -> "c1.xlarge"
        C3_2XLarge -> "c3.2xlarge"
        C3_4XLarge -> "c3.4xlarge"
        C3_8XLarge -> "c3.8xlarge"
        C3_Large -> "c3.large"
        C3_XLarge -> "c3.xlarge"
        C4_2XLarge -> "c4.2xlarge"
        C4_4XLarge -> "c4.4xlarge"
        C4_8XLarge -> "c4.8xlarge"
        C4_Large -> "c4.large"
        C4_XLarge -> "c4.xlarge"
        C5_12XLarge -> "c5.12xlarge"
        C5_18XLarge -> "c5.18xlarge"
        C5_24XLarge -> "c5.24xlarge"
        C5_2XLarge -> "c5.2xlarge"
        C5_4XLarge -> "c5.4xlarge"
        C5_9XLarge -> "c5.9xlarge"
        C5_Large -> "c5.large"
        C5_Metal -> "c5.metal"
        C5_XLarge -> "c5.xlarge"
        C5a_12XLarge -> "c5a.12xlarge"
        C5a_16XLarge -> "c5a.16xlarge"
        C5a_24XLarge -> "c5a.24xlarge"
        C5a_2XLarge -> "c5a.2xlarge"
        C5a_4XLarge -> "c5a.4xlarge"
        C5a_8XLarge -> "c5a.8xlarge"
        C5a_Large -> "c5a.large"
        C5a_XLarge -> "c5a.xlarge"
        C5ad_12XLarge -> "c5ad.12xlarge"
        C5ad_16XLarge -> "c5ad.16xlarge"
        C5ad_24XLarge -> "c5ad.24xlarge"
        C5ad_2XLarge -> "c5ad.2xlarge"
        C5ad_4XLarge -> "c5ad.4xlarge"
        C5ad_8XLarge -> "c5ad.8xlarge"
        C5ad_Large -> "c5ad.large"
        C5ad_XLarge -> "c5ad.xlarge"
        C5d_12XLarge -> "c5d.12xlarge"
        C5d_18XLarge -> "c5d.18xlarge"
        C5d_24XLarge -> "c5d.24xlarge"
        C5d_2XLarge -> "c5d.2xlarge"
        C5d_4XLarge -> "c5d.4xlarge"
        C5d_9XLarge -> "c5d.9xlarge"
        C5d_Large -> "c5d.large"
        C5d_Metal -> "c5d.metal"
        C5d_XLarge -> "c5d.xlarge"
        C5n_18XLarge -> "c5n.18xlarge"
        C5n_2XLarge -> "c5n.2xlarge"
        C5n_4XLarge -> "c5n.4xlarge"
        C5n_9XLarge -> "c5n.9xlarge"
        C5n_Large -> "c5n.large"
        C5n_Metal -> "c5n.metal"
        C5n_XLarge -> "c5n.xlarge"
        C6g_12XLarge -> "c6g.12xlarge"
        C6g_16XLarge -> "c6g.16xlarge"
        C6g_2XLarge -> "c6g.2xlarge"
        C6g_4XLarge -> "c6g.4xlarge"
        C6g_8XLarge -> "c6g.8xlarge"
        C6g_Large -> "c6g.large"
        C6g_Medium -> "c6g.medium"
        C6g_Metal -> "c6g.metal"
        C6g_XLarge -> "c6g.xlarge"
        C6gd_12XLarge -> "c6gd.12xlarge"
        C6gd_16XLarge -> "c6gd.16xlarge"
        C6gd_2XLarge -> "c6gd.2xlarge"
        C6gd_4XLarge -> "c6gd.4xlarge"
        C6gd_8XLarge -> "c6gd.8xlarge"
        C6gd_Large -> "c6gd.large"
        C6gd_Medium -> "c6gd.medium"
        C6gd_Metal -> "c6gd.metal"
        C6gd_XLarge -> "c6gd.xlarge"
        C6gn_12XLarge -> "c6gn.12xlarge"
        C6gn_16XLarge -> "c6gn.16xlarge"
        C6gn_2XLarge -> "c6gn.2xlarge"
        C6gn_4XLarge -> "c6gn.4xlarge"
        C6gn_8XLarge -> "c6gn.8xlarge"
        C6gn_Large -> "c6gn.large"
        C6gn_Medium -> "c6gn.medium"
        C6gn_XLarge -> "c6gn.xlarge"
        CC1_4XLarge -> "cc1.4xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        D2_2XLarge -> "d2.2xlarge"
        D2_4XLarge -> "d2.4xlarge"
        D2_8XLarge -> "d2.8xlarge"
        D2_XLarge -> "d2.xlarge"
        D3_2XLarge -> "d3.2xlarge"
        D3_4XLarge -> "d3.4xlarge"
        D3_8XLarge -> "d3.8xlarge"
        D3_XLarge -> "d3.xlarge"
        D3en_12XLarge -> "d3en.12xlarge"
        D3en_2XLarge -> "d3en.2xlarge"
        D3en_4XLarge -> "d3en.4xlarge"
        D3en_6XLarge -> "d3en.6xlarge"
        D3en_8XLarge -> "d3en.8xlarge"
        D3en_XLarge -> "d3en.xlarge"
        F1_16XLarge -> "f1.16xlarge"
        F1_2XLarge -> "f1.2xlarge"
        F1_4XLarge -> "f1.4xlarge"
        G2_2XLarge -> "g2.2xlarge"
        G2_8XLarge -> "g2.8xlarge"
        G3_16XLarge -> "g3.16xlarge"
        G3_4XLarge -> "g3.4xlarge"
        G3_8XLarge -> "g3.8xlarge"
        G3s_XLarge -> "g3s.xlarge"
        G4ad_16XLarge -> "g4ad.16xlarge"
        G4ad_4XLarge -> "g4ad.4xlarge"
        G4ad_8XLarge -> "g4ad.8xlarge"
        G4dn_12XLarge -> "g4dn.12xlarge"
        G4dn_16XLarge -> "g4dn.16xlarge"
        G4dn_2XLarge -> "g4dn.2xlarge"
        G4dn_4XLarge -> "g4dn.4xlarge"
        G4dn_8XLarge -> "g4dn.8xlarge"
        G4dn_Metal -> "g4dn.metal"
        G4dn_XLarge -> "g4dn.xlarge"
        H1_16XLarge -> "h1.16xlarge"
        H1_2XLarge -> "h1.2xlarge"
        H1_4XLarge -> "h1.4xlarge"
        H1_8XLarge -> "h1.8xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        I2_2XLarge -> "i2.2xlarge"
        I2_4XLarge -> "i2.4xlarge"
        I2_8XLarge -> "i2.8xlarge"
        I2_XLarge -> "i2.xlarge"
        I3_16XLarge -> "i3.16xlarge"
        I3_2XLarge -> "i3.2xlarge"
        I3_4XLarge -> "i3.4xlarge"
        I3_8XLarge -> "i3.8xlarge"
        I3_Large -> "i3.large"
        I3_Metal -> "i3.metal"
        I3_XLarge -> "i3.xlarge"
        I3en_12XLarge -> "i3en.12xlarge"
        I3en_24XLarge -> "i3en.24xlarge"
        I3en_2XLarge -> "i3en.2xlarge"
        I3en_3XLarge -> "i3en.3xlarge"
        I3en_6XLarge -> "i3en.6xlarge"
        I3en_Large -> "i3en.large"
        I3en_Metal -> "i3en.metal"
        I3en_XLarge -> "i3en.xlarge"
        INF1_24XLarge -> "inf1.24xlarge"
        INF1_2XLarge -> "inf1.2xlarge"
        INF1_6XLarge -> "inf1.6xlarge"
        INF1_XLarge -> "inf1.xlarge"
        M1_Large -> "m1.large"
        M1_Medium -> "m1.medium"
        M1_Small -> "m1.small"
        M1_XLarge -> "m1.xlarge"
        M2_2XLarge -> "m2.2xlarge"
        M2_4XLarge -> "m2.4xlarge"
        M2_XLarge -> "m2.xlarge"
        M3_2XLarge -> "m3.2xlarge"
        M3_Large -> "m3.large"
        M3_Medium -> "m3.medium"
        M3_XLarge -> "m3.xlarge"
        M4_10XLarge -> "m4.10xlarge"
        M4_16XLarge -> "m4.16xlarge"
        M4_2XLarge -> "m4.2xlarge"
        M4_4XLarge -> "m4.4xlarge"
        M4_Large -> "m4.large"
        M4_XLarge -> "m4.xlarge"
        M5_12XLarge -> "m5.12xlarge"
        M5_16XLarge -> "m5.16xlarge"
        M5_24XLarge -> "m5.24xlarge"
        M5_2XLarge -> "m5.2xlarge"
        M5_4XLarge -> "m5.4xlarge"
        M5_8XLarge -> "m5.8xlarge"
        M5_Large -> "m5.large"
        M5_Metal -> "m5.metal"
        M5_XLarge -> "m5.xlarge"
        M5a_12XLarge -> "m5a.12xlarge"
        M5a_16XLarge -> "m5a.16xlarge"
        M5a_24XLarge -> "m5a.24xlarge"
        M5a_2XLarge -> "m5a.2xlarge"
        M5a_4XLarge -> "m5a.4xlarge"
        M5a_8XLarge -> "m5a.8xlarge"
        M5a_Large -> "m5a.large"
        M5a_XLarge -> "m5a.xlarge"
        M5ad_12XLarge -> "m5ad.12xlarge"
        M5ad_16XLarge -> "m5ad.16xlarge"
        M5ad_24XLarge -> "m5ad.24xlarge"
        M5ad_2XLarge -> "m5ad.2xlarge"
        M5ad_4XLarge -> "m5ad.4xlarge"
        M5ad_8XLarge -> "m5ad.8xlarge"
        M5ad_Large -> "m5ad.large"
        M5ad_XLarge -> "m5ad.xlarge"
        M5d_12XLarge -> "m5d.12xlarge"
        M5d_16XLarge -> "m5d.16xlarge"
        M5d_24XLarge -> "m5d.24xlarge"
        M5d_2XLarge -> "m5d.2xlarge"
        M5d_4XLarge -> "m5d.4xlarge"
        M5d_8XLarge -> "m5d.8xlarge"
        M5d_Large -> "m5d.large"
        M5d_Metal -> "m5d.metal"
        M5d_XLarge -> "m5d.xlarge"
        M5dn_12XLarge -> "m5dn.12xlarge"
        M5dn_16XLarge -> "m5dn.16xlarge"
        M5dn_24XLarge -> "m5dn.24xlarge"
        M5dn_2XLarge -> "m5dn.2xlarge"
        M5dn_4XLarge -> "m5dn.4xlarge"
        M5dn_8XLarge -> "m5dn.8xlarge"
        M5dn_Large -> "m5dn.large"
        M5dn_XLarge -> "m5dn.xlarge"
        M5n_12XLarge -> "m5n.12xlarge"
        M5n_16XLarge -> "m5n.16xlarge"
        M5n_24XLarge -> "m5n.24xlarge"
        M5n_2XLarge -> "m5n.2xlarge"
        M5n_4XLarge -> "m5n.4xlarge"
        M5n_8XLarge -> "m5n.8xlarge"
        M5n_Large -> "m5n.large"
        M5n_XLarge -> "m5n.xlarge"
        M5zn_12XLarge -> "m5zn.12xlarge"
        M5zn_2XLarge -> "m5zn.2xlarge"
        M5zn_3XLarge -> "m5zn.3xlarge"
        M5zn_6XLarge -> "m5zn.6xlarge"
        M5zn_Large -> "m5zn.large"
        M5zn_Metal -> "m5zn.metal"
        M5zn_XLarge -> "m5zn.xlarge"
        M6g_12XLarge -> "m6g.12xlarge"
        M6g_16XLarge -> "m6g.16xlarge"
        M6g_2XLarge -> "m6g.2xlarge"
        M6g_4XLarge -> "m6g.4xlarge"
        M6g_8XLarge -> "m6g.8xlarge"
        M6g_Large -> "m6g.large"
        M6g_Medium -> "m6g.medium"
        M6g_Metal -> "m6g.metal"
        M6g_XLarge -> "m6g.xlarge"
        M6gd_12XLarge -> "m6gd.12xlarge"
        M6gd_16XLarge -> "m6gd.16xlarge"
        M6gd_2XLarge -> "m6gd.2xlarge"
        M6gd_4XLarge -> "m6gd.4xlarge"
        M6gd_8XLarge -> "m6gd.8xlarge"
        M6gd_Large -> "m6gd.large"
        M6gd_Medium -> "m6gd.medium"
        M6gd_Metal -> "m6gd.metal"
        M6gd_XLarge -> "m6gd.xlarge"
        MAC1_Metal -> "mac1.metal"
        P2_16XLarge -> "p2.16xlarge"
        P2_8XLarge -> "p2.8xlarge"
        P2_XLarge -> "p2.xlarge"
        P3_16XLarge -> "p3.16xlarge"
        P3_2XLarge -> "p3.2xlarge"
        P3_8XLarge -> "p3.8xlarge"
        P3dn_24XLarge -> "p3dn.24xlarge"
        P4d_24XLarge -> "p4d.24xlarge"
        R3_2XLarge -> "r3.2xlarge"
        R3_4XLarge -> "r3.4xlarge"
        R3_8XLarge -> "r3.8xlarge"
        R3_Large -> "r3.large"
        R3_XLarge -> "r3.xlarge"
        R4_16XLarge -> "r4.16xlarge"
        R4_2XLarge -> "r4.2xlarge"
        R4_4XLarge -> "r4.4xlarge"
        R4_8XLarge -> "r4.8xlarge"
        R4_Large -> "r4.large"
        R4_XLarge -> "r4.xlarge"
        R5_12XLarge -> "r5.12xlarge"
        R5_16XLarge -> "r5.16xlarge"
        R5_24XLarge -> "r5.24xlarge"
        R5_2XLarge -> "r5.2xlarge"
        R5_4XLarge -> "r5.4xlarge"
        R5_8XLarge -> "r5.8xlarge"
        R5_Large -> "r5.large"
        R5_Metal -> "r5.metal"
        R5_XLarge -> "r5.xlarge"
        R5a_12XLarge -> "r5a.12xlarge"
        R5a_16XLarge -> "r5a.16xlarge"
        R5a_24XLarge -> "r5a.24xlarge"
        R5a_2XLarge -> "r5a.2xlarge"
        R5a_4XLarge -> "r5a.4xlarge"
        R5a_8XLarge -> "r5a.8xlarge"
        R5a_Large -> "r5a.large"
        R5a_XLarge -> "r5a.xlarge"
        R5ad_12XLarge -> "r5ad.12xlarge"
        R5ad_16XLarge -> "r5ad.16xlarge"
        R5ad_24XLarge -> "r5ad.24xlarge"
        R5ad_2XLarge -> "r5ad.2xlarge"
        R5ad_4XLarge -> "r5ad.4xlarge"
        R5ad_8XLarge -> "r5ad.8xlarge"
        R5ad_Large -> "r5ad.large"
        R5ad_XLarge -> "r5ad.xlarge"
        R5b_12XLarge -> "r5b.12xlarge"
        R5b_16XLarge -> "r5b.16xlarge"
        R5b_24XLarge -> "r5b.24xlarge"
        R5b_2XLarge -> "r5b.2xlarge"
        R5b_4XLarge -> "r5b.4xlarge"
        R5b_8XLarge -> "r5b.8xlarge"
        R5b_Large -> "r5b.large"
        R5b_Metal -> "r5b.metal"
        R5b_XLarge -> "r5b.xlarge"
        R5d_12XLarge -> "r5d.12xlarge"
        R5d_16XLarge -> "r5d.16xlarge"
        R5d_24XLarge -> "r5d.24xlarge"
        R5d_2XLarge -> "r5d.2xlarge"
        R5d_4XLarge -> "r5d.4xlarge"
        R5d_8XLarge -> "r5d.8xlarge"
        R5d_Large -> "r5d.large"
        R5d_Metal -> "r5d.metal"
        R5d_XLarge -> "r5d.xlarge"
        R5dn_12XLarge -> "r5dn.12xlarge"
        R5dn_16XLarge -> "r5dn.16xlarge"
        R5dn_24XLarge -> "r5dn.24xlarge"
        R5dn_2XLarge -> "r5dn.2xlarge"
        R5dn_4XLarge -> "r5dn.4xlarge"
        R5dn_8XLarge -> "r5dn.8xlarge"
        R5dn_Large -> "r5dn.large"
        R5dn_XLarge -> "r5dn.xlarge"
        R5n_12XLarge -> "r5n.12xlarge"
        R5n_16XLarge -> "r5n.16xlarge"
        R5n_24XLarge -> "r5n.24xlarge"
        R5n_2XLarge -> "r5n.2xlarge"
        R5n_4XLarge -> "r5n.4xlarge"
        R5n_8XLarge -> "r5n.8xlarge"
        R5n_Large -> "r5n.large"
        R5n_XLarge -> "r5n.xlarge"
        R6g_12XLarge -> "r6g.12xlarge"
        R6g_16XLarge -> "r6g.16xlarge"
        R6g_2XLarge -> "r6g.2xlarge"
        R6g_4XLarge -> "r6g.4xlarge"
        R6g_8XLarge -> "r6g.8xlarge"
        R6g_Large -> "r6g.large"
        R6g_Medium -> "r6g.medium"
        R6g_Metal -> "r6g.metal"
        R6g_XLarge -> "r6g.xlarge"
        R6gd_12XLarge -> "r6gd.12xlarge"
        R6gd_16XLarge -> "r6gd.16xlarge"
        R6gd_2XLarge -> "r6gd.2xlarge"
        R6gd_4XLarge -> "r6gd.4xlarge"
        R6gd_8XLarge -> "r6gd.8xlarge"
        R6gd_Large -> "r6gd.large"
        R6gd_Medium -> "r6gd.medium"
        R6gd_Metal -> "r6gd.metal"
        R6gd_XLarge -> "r6gd.xlarge"
        T1_Micro -> "t1.micro"
        T2_2XLarge -> "t2.2xlarge"
        T2_Large -> "t2.large"
        T2_Medium -> "t2.medium"
        T2_Micro -> "t2.micro"
        T2_Nano -> "t2.nano"
        T2_Small -> "t2.small"
        T2_XLarge -> "t2.xlarge"
        T3_2XLarge -> "t3.2xlarge"
        T3_Large -> "t3.large"
        T3_Medium -> "t3.medium"
        T3_Micro -> "t3.micro"
        T3_Nano -> "t3.nano"
        T3_Small -> "t3.small"
        T3_XLarge -> "t3.xlarge"
        T3a_2XLarge -> "t3a.2xlarge"
        T3a_Large -> "t3a.large"
        T3a_Medium -> "t3a.medium"
        T3a_Micro -> "t3a.micro"
        T3a_Nano -> "t3a.nano"
        T3a_Small -> "t3a.small"
        T3a_XLarge -> "t3a.xlarge"
        T4g_2XLarge -> "t4g.2xlarge"
        T4g_Large -> "t4g.large"
        T4g_Medium -> "t4g.medium"
        T4g_Micro -> "t4g.micro"
        T4g_Nano -> "t4g.nano"
        T4g_Small -> "t4g.small"
        T4g_XLarge -> "t4g.xlarge"
        U12TB1_Metal -> "u-12tb1.metal"
        U18TB1_Metal -> "u-18tb1.metal"
        U24TB1_Metal -> "u-24tb1.metal"
        U6TB1_Metal -> "u-6tb1.metal"
        U9TB1_Metal -> "u-9tb1.metal"
        X1_16XLarge -> "x1.16xlarge"
        X1_32XLarge -> "x1.32xlarge"
        X1e_16XLarge -> "x1e.16xlarge"
        X1e_2XLarge -> "x1e.2xlarge"
        X1e_32XLarge -> "x1e.32xlarge"
        X1e_4XLarge -> "x1e.4xlarge"
        X1e_8XLarge -> "x1e.8xlarge"
        X1e_XLarge -> "x1e.xlarge"
        Z1d_12XLarge -> "z1d.12xlarge"
        Z1d_2XLarge -> "z1d.2xlarge"
        Z1d_3XLarge -> "z1d.3xlarge"
        Z1d_6XLarge -> "z1d.6xlarge"
        Z1d_Large -> "z1d.large"
        Z1d_Metal -> "z1d.metal"
        Z1d_XLarge -> "z1d.xlarge"

instance Hashable     InstanceType
instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"

data InstanceTypeHypervisor
  = Nitro
  | Xen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceTypeHypervisor where
    parser = takeLowerText >>= \case
        "nitro" -> pure Nitro
        "xen" -> pure Xen
        e -> fromTextError $ "Failure parsing InstanceTypeHypervisor from value: '" <> e
           <> "'. Accepted values: nitro, xen"

instance ToText InstanceTypeHypervisor where
    toText = \case
        Nitro -> "nitro"
        Xen -> "xen"

instance Hashable     InstanceTypeHypervisor
instance NFData       InstanceTypeHypervisor
instance ToByteString InstanceTypeHypervisor
instance ToQuery      InstanceTypeHypervisor
instance ToHeader     InstanceTypeHypervisor

instance FromXML InstanceTypeHypervisor where
    parseXML = parseXMLText "InstanceTypeHypervisor"

data InterfacePermissionType
  = EIPAssociate
  | InstanceAttach
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InterfacePermissionType where
    parser = takeLowerText >>= \case
        "eip-associate" -> pure EIPAssociate
        "instance-attach" -> pure InstanceAttach
        e -> fromTextError $ "Failure parsing InterfacePermissionType from value: '" <> e
           <> "'. Accepted values: eip-associate, instance-attach"

instance ToText InterfacePermissionType where
    toText = \case
        EIPAssociate -> "EIP-ASSOCIATE"
        InstanceAttach -> "INSTANCE-ATTACH"

instance Hashable     InterfacePermissionType
instance NFData       InterfacePermissionType
instance ToByteString InterfacePermissionType
instance ToQuery      InterfacePermissionType
instance ToHeader     InterfacePermissionType

instance FromXML InterfacePermissionType where
    parseXML = parseXMLText "InterfacePermissionType"

data LaunchTemplateErrorCode
  = LaunchTemplateIdDoesNotExist
  | LaunchTemplateIdMalformed
  | LaunchTemplateNameDoesNotExist
  | LaunchTemplateNameMalformed
  | LaunchTemplateVersionDoesNotExist
  | UnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchTemplateErrorCode where
    parser = takeLowerText >>= \case
        "launchtemplateiddoesnotexist" -> pure LaunchTemplateIdDoesNotExist
        "launchtemplateidmalformed" -> pure LaunchTemplateIdMalformed
        "launchtemplatenamedoesnotexist" -> pure LaunchTemplateNameDoesNotExist
        "launchtemplatenamemalformed" -> pure LaunchTemplateNameMalformed
        "launchtemplateversiondoesnotexist" -> pure LaunchTemplateVersionDoesNotExist
        "unexpectederror" -> pure UnexpectedError
        e -> fromTextError $ "Failure parsing LaunchTemplateErrorCode from value: '" <> e
           <> "'. Accepted values: launchtemplateiddoesnotexist, launchtemplateidmalformed, launchtemplatenamedoesnotexist, launchtemplatenamemalformed, launchtemplateversiondoesnotexist, unexpectederror"

instance ToText LaunchTemplateErrorCode where
    toText = \case
        LaunchTemplateIdDoesNotExist -> "launchTemplateIdDoesNotExist"
        LaunchTemplateIdMalformed -> "launchTemplateIdMalformed"
        LaunchTemplateNameDoesNotExist -> "launchTemplateNameDoesNotExist"
        LaunchTemplateNameMalformed -> "launchTemplateNameMalformed"
        LaunchTemplateVersionDoesNotExist -> "launchTemplateVersionDoesNotExist"
        UnexpectedError -> "unexpectedError"

instance Hashable     LaunchTemplateErrorCode
instance NFData       LaunchTemplateErrorCode
instance ToByteString LaunchTemplateErrorCode
instance ToQuery      LaunchTemplateErrorCode
instance ToHeader     LaunchTemplateErrorCode

instance FromXML LaunchTemplateErrorCode where
    parseXML = parseXMLText "LaunchTemplateErrorCode"

data LaunchTemplateHTTPTokensState
  = Optional
  | Required
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchTemplateHTTPTokensState where
    parser = takeLowerText >>= \case
        "optional" -> pure Optional
        "required" -> pure Required
        e -> fromTextError $ "Failure parsing LaunchTemplateHTTPTokensState from value: '" <> e
           <> "'. Accepted values: optional, required"

instance ToText LaunchTemplateHTTPTokensState where
    toText = \case
        Optional -> "optional"
        Required -> "required"

instance Hashable     LaunchTemplateHTTPTokensState
instance NFData       LaunchTemplateHTTPTokensState
instance ToByteString LaunchTemplateHTTPTokensState
instance ToQuery      LaunchTemplateHTTPTokensState
instance ToHeader     LaunchTemplateHTTPTokensState

instance FromXML LaunchTemplateHTTPTokensState where
    parseXML = parseXMLText "LaunchTemplateHTTPTokensState"

data LaunchTemplateInstanceMetadataEndpointState
  = LTIMESDisabled
  | LTIMESEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchTemplateInstanceMetadataEndpointState where
    parser = takeLowerText >>= \case
        "disabled" -> pure LTIMESDisabled
        "enabled" -> pure LTIMESEnabled
        e -> fromTextError $ "Failure parsing LaunchTemplateInstanceMetadataEndpointState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText LaunchTemplateInstanceMetadataEndpointState where
    toText = \case
        LTIMESDisabled -> "disabled"
        LTIMESEnabled -> "enabled"

instance Hashable     LaunchTemplateInstanceMetadataEndpointState
instance NFData       LaunchTemplateInstanceMetadataEndpointState
instance ToByteString LaunchTemplateInstanceMetadataEndpointState
instance ToQuery      LaunchTemplateInstanceMetadataEndpointState
instance ToHeader     LaunchTemplateInstanceMetadataEndpointState

instance FromXML LaunchTemplateInstanceMetadataEndpointState where
    parseXML = parseXMLText "LaunchTemplateInstanceMetadataEndpointState"

data LaunchTemplateInstanceMetadataOptionsState
  = LTIMOSApplied
  | LTIMOSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchTemplateInstanceMetadataOptionsState where
    parser = takeLowerText >>= \case
        "applied" -> pure LTIMOSApplied
        "pending" -> pure LTIMOSPending
        e -> fromTextError $ "Failure parsing LaunchTemplateInstanceMetadataOptionsState from value: '" <> e
           <> "'. Accepted values: applied, pending"

instance ToText LaunchTemplateInstanceMetadataOptionsState where
    toText = \case
        LTIMOSApplied -> "applied"
        LTIMOSPending -> "pending"

instance Hashable     LaunchTemplateInstanceMetadataOptionsState
instance NFData       LaunchTemplateInstanceMetadataOptionsState
instance ToByteString LaunchTemplateInstanceMetadataOptionsState
instance ToQuery      LaunchTemplateInstanceMetadataOptionsState
instance ToHeader     LaunchTemplateInstanceMetadataOptionsState

instance FromXML LaunchTemplateInstanceMetadataOptionsState where
    parseXML = parseXMLText "LaunchTemplateInstanceMetadataOptionsState"

data ListingState
  = LAvailable
  | LCancelled
  | LPending
  | LSold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListingState where
    parser = takeLowerText >>= \case
        "available" -> pure LAvailable
        "cancelled" -> pure LCancelled
        "pending" -> pure LPending
        "sold" -> pure LSold
        e -> fromTextError $ "Failure parsing ListingState from value: '" <> e
           <> "'. Accepted values: available, cancelled, pending, sold"

instance ToText ListingState where
    toText = \case
        LAvailable -> "available"
        LCancelled -> "cancelled"
        LPending -> "pending"
        LSold -> "sold"

instance Hashable     ListingState
instance NFData       ListingState
instance ToByteString ListingState
instance ToQuery      ListingState
instance ToHeader     ListingState

instance FromXML ListingState where
    parseXML = parseXMLText "ListingState"

data ListingStatus
  = LSActive
  | LSCancelled
  | LSClosed
  | LSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListingStatus where
    parser = takeLowerText >>= \case
        "active" -> pure LSActive
        "cancelled" -> pure LSCancelled
        "closed" -> pure LSClosed
        "pending" -> pure LSPending
        e -> fromTextError $ "Failure parsing ListingStatus from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, pending"

instance ToText ListingStatus where
    toText = \case
        LSActive -> "active"
        LSCancelled -> "cancelled"
        LSClosed -> "closed"
        LSPending -> "pending"

instance Hashable     ListingStatus
instance NFData       ListingStatus
instance ToByteString ListingStatus
instance ToQuery      ListingStatus
instance ToHeader     ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

data LocalGatewayRouteState
  = LGRSActive
  | LGRSBlackhole
  | LGRSDeleted
  | LGRSDeleting
  | LGRSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LocalGatewayRouteState where
    parser = takeLowerText >>= \case
        "active" -> pure LGRSActive
        "blackhole" -> pure LGRSBlackhole
        "deleted" -> pure LGRSDeleted
        "deleting" -> pure LGRSDeleting
        "pending" -> pure LGRSPending
        e -> fromTextError $ "Failure parsing LocalGatewayRouteState from value: '" <> e
           <> "'. Accepted values: active, blackhole, deleted, deleting, pending"

instance ToText LocalGatewayRouteState where
    toText = \case
        LGRSActive -> "active"
        LGRSBlackhole -> "blackhole"
        LGRSDeleted -> "deleted"
        LGRSDeleting -> "deleting"
        LGRSPending -> "pending"

instance Hashable     LocalGatewayRouteState
instance NFData       LocalGatewayRouteState
instance ToByteString LocalGatewayRouteState
instance ToQuery      LocalGatewayRouteState
instance ToHeader     LocalGatewayRouteState

instance FromXML LocalGatewayRouteState where
    parseXML = parseXMLText "LocalGatewayRouteState"

data LocalGatewayRouteType
  = LGRTPropagated
  | LGRTStatic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LocalGatewayRouteType where
    parser = takeLowerText >>= \case
        "propagated" -> pure LGRTPropagated
        "static" -> pure LGRTStatic
        e -> fromTextError $ "Failure parsing LocalGatewayRouteType from value: '" <> e
           <> "'. Accepted values: propagated, static"

instance ToText LocalGatewayRouteType where
    toText = \case
        LGRTPropagated -> "propagated"
        LGRTStatic -> "static"

instance Hashable     LocalGatewayRouteType
instance NFData       LocalGatewayRouteType
instance ToByteString LocalGatewayRouteType
instance ToQuery      LocalGatewayRouteType
instance ToHeader     LocalGatewayRouteType

instance FromXML LocalGatewayRouteType where
    parseXML = parseXMLText "LocalGatewayRouteType"

data LocationType
  = AvailabilityZone
  | AvailabilityZoneId
  | Region
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LocationType where
    parser = takeLowerText >>= \case
        "availability-zone" -> pure AvailabilityZone
        "availability-zone-id" -> pure AvailabilityZoneId
        "region" -> pure Region
        e -> fromTextError $ "Failure parsing LocationType from value: '" <> e
           <> "'. Accepted values: availability-zone, availability-zone-id, region"

instance ToText LocationType where
    toText = \case
        AvailabilityZone -> "availability-zone"
        AvailabilityZoneId -> "availability-zone-id"
        Region -> "region"

instance Hashable     LocationType
instance NFData       LocationType
instance ToByteString LocationType
instance ToQuery      LocationType
instance ToHeader     LocationType

instance FromXML LocationType where
    parseXML = parseXMLText "LocationType"

data LogDestinationType
  = CloudWatchLogs
  | S3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogDestinationType where
    parser = takeLowerText >>= \case
        "cloud-watch-logs" -> pure CloudWatchLogs
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing LogDestinationType from value: '" <> e
           <> "'. Accepted values: cloud-watch-logs, s3"

instance ToText LogDestinationType where
    toText = \case
        CloudWatchLogs -> "cloud-watch-logs"
        S3 -> "s3"

instance Hashable     LogDestinationType
instance NFData       LogDestinationType
instance ToByteString LogDestinationType
instance ToQuery      LogDestinationType
instance ToHeader     LogDestinationType

instance FromXML LogDestinationType where
    parseXML = parseXMLText "LogDestinationType"

data MarketType =
  Spot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MarketType where
    parser = takeLowerText >>= \case
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing MarketType from value: '" <> e
           <> "'. Accepted values: spot"

instance ToText MarketType where
    toText = \case
        Spot -> "spot"

instance Hashable     MarketType
instance NFData       MarketType
instance ToByteString MarketType
instance ToQuery      MarketType
instance ToHeader     MarketType

instance FromXML MarketType where
    parseXML = parseXMLText "MarketType"

data MembershipType
  = MTIgmp
  | MTStatic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MembershipType where
    parser = takeLowerText >>= \case
        "igmp" -> pure MTIgmp
        "static" -> pure MTStatic
        e -> fromTextError $ "Failure parsing MembershipType from value: '" <> e
           <> "'. Accepted values: igmp, static"

instance ToText MembershipType where
    toText = \case
        MTIgmp -> "igmp"
        MTStatic -> "static"

instance Hashable     MembershipType
instance NFData       MembershipType
instance ToByteString MembershipType
instance ToQuery      MembershipType
instance ToHeader     MembershipType

instance FromXML MembershipType where
    parseXML = parseXMLText "MembershipType"

data ModifyAvailabilityZoneOptInStatus
  = NotOptedIn
  | OptedIn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModifyAvailabilityZoneOptInStatus where
    parser = takeLowerText >>= \case
        "not-opted-in" -> pure NotOptedIn
        "opted-in" -> pure OptedIn
        e -> fromTextError $ "Failure parsing ModifyAvailabilityZoneOptInStatus from value: '" <> e
           <> "'. Accepted values: not-opted-in, opted-in"

instance ToText ModifyAvailabilityZoneOptInStatus where
    toText = \case
        NotOptedIn -> "not-opted-in"
        OptedIn -> "opted-in"

instance Hashable     ModifyAvailabilityZoneOptInStatus
instance NFData       ModifyAvailabilityZoneOptInStatus
instance ToByteString ModifyAvailabilityZoneOptInStatus
instance ToQuery      ModifyAvailabilityZoneOptInStatus
instance ToHeader     ModifyAvailabilityZoneOptInStatus

data MonitoringState
  = MSDisabled
  | MSDisabling
  | MSEnabled
  | MSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MonitoringState where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSDisabled
        "disabling" -> pure MSDisabling
        "enabled" -> pure MSEnabled
        "pending" -> pure MSPending
        e -> fromTextError $ "Failure parsing MonitoringState from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, pending"

instance ToText MonitoringState where
    toText = \case
        MSDisabled -> "disabled"
        MSDisabling -> "disabling"
        MSEnabled -> "enabled"
        MSPending -> "pending"

instance Hashable     MonitoringState
instance NFData       MonitoringState
instance ToByteString MonitoringState
instance ToQuery      MonitoringState
instance ToHeader     MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"

data MoveStatus
  = MovingToVPC
  | RestoringToClassic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MoveStatus where
    parser = takeLowerText >>= \case
        "movingtovpc" -> pure MovingToVPC
        "restoringtoclassic" -> pure RestoringToClassic
        e -> fromTextError $ "Failure parsing MoveStatus from value: '" <> e
           <> "'. Accepted values: movingtovpc, restoringtoclassic"

instance ToText MoveStatus where
    toText = \case
        MovingToVPC -> "movingToVpc"
        RestoringToClassic -> "restoringToClassic"

instance Hashable     MoveStatus
instance NFData       MoveStatus
instance ToByteString MoveStatus
instance ToQuery      MoveStatus
instance ToHeader     MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"

data MulticastSupportValue
  = MSVDisable
  | MSVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MulticastSupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure MSVDisable
        "enable" -> pure MSVEnable
        e -> fromTextError $ "Failure parsing MulticastSupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText MulticastSupportValue where
    toText = \case
        MSVDisable -> "disable"
        MSVEnable -> "enable"

instance Hashable     MulticastSupportValue
instance NFData       MulticastSupportValue
instance ToByteString MulticastSupportValue
instance ToQuery      MulticastSupportValue
instance ToHeader     MulticastSupportValue

instance FromXML MulticastSupportValue where
    parseXML = parseXMLText "MulticastSupportValue"

data NatGatewayState
  = NGSAvailable
  | NGSDeleted
  | NGSDeleting
  | NGSFailed
  | NGSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NatGatewayState where
    parser = takeLowerText >>= \case
        "available" -> pure NGSAvailable
        "deleted" -> pure NGSDeleted
        "deleting" -> pure NGSDeleting
        "failed" -> pure NGSFailed
        "pending" -> pure NGSPending
        e -> fromTextError $ "Failure parsing NatGatewayState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText NatGatewayState where
    toText = \case
        NGSAvailable -> "available"
        NGSDeleted -> "deleted"
        NGSDeleting -> "deleting"
        NGSFailed -> "failed"
        NGSPending -> "pending"

instance Hashable     NatGatewayState
instance NFData       NatGatewayState
instance ToByteString NatGatewayState
instance ToQuery      NatGatewayState
instance ToHeader     NatGatewayState

instance FromXML NatGatewayState where
    parseXML = parseXMLText "NatGatewayState"

data NetworkInterfaceAttribute
  = NIAAttachment
  | NIADescription
  | NIAGroupSet
  | NIASourceDestCheck
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceAttribute where
    parser = takeLowerText >>= \case
        "attachment" -> pure NIAAttachment
        "description" -> pure NIADescription
        "groupset" -> pure NIAGroupSet
        "sourcedestcheck" -> pure NIASourceDestCheck
        e -> fromTextError $ "Failure parsing NetworkInterfaceAttribute from value: '" <> e
           <> "'. Accepted values: attachment, description, groupset, sourcedestcheck"

instance ToText NetworkInterfaceAttribute where
    toText = \case
        NIAAttachment -> "attachment"
        NIADescription -> "description"
        NIAGroupSet -> "groupSet"
        NIASourceDestCheck -> "sourceDestCheck"

instance Hashable     NetworkInterfaceAttribute
instance NFData       NetworkInterfaceAttribute
instance ToByteString NetworkInterfaceAttribute
instance ToQuery      NetworkInterfaceAttribute
instance ToHeader     NetworkInterfaceAttribute

data NetworkInterfaceCreationType =
  Efa
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceCreationType where
    parser = takeLowerText >>= \case
        "efa" -> pure Efa
        e -> fromTextError $ "Failure parsing NetworkInterfaceCreationType from value: '" <> e
           <> "'. Accepted values: efa"

instance ToText NetworkInterfaceCreationType where
    toText = \case
        Efa -> "efa"

instance Hashable     NetworkInterfaceCreationType
instance NFData       NetworkInterfaceCreationType
instance ToByteString NetworkInterfaceCreationType
instance ToQuery      NetworkInterfaceCreationType
instance ToHeader     NetworkInterfaceCreationType

data NetworkInterfacePermissionStateCode
  = NIPSCGranted
  | NIPSCPending
  | NIPSCRevoked
  | NIPSCRevoking
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfacePermissionStateCode where
    parser = takeLowerText >>= \case
        "granted" -> pure NIPSCGranted
        "pending" -> pure NIPSCPending
        "revoked" -> pure NIPSCRevoked
        "revoking" -> pure NIPSCRevoking
        e -> fromTextError $ "Failure parsing NetworkInterfacePermissionStateCode from value: '" <> e
           <> "'. Accepted values: granted, pending, revoked, revoking"

instance ToText NetworkInterfacePermissionStateCode where
    toText = \case
        NIPSCGranted -> "granted"
        NIPSCPending -> "pending"
        NIPSCRevoked -> "revoked"
        NIPSCRevoking -> "revoking"

instance Hashable     NetworkInterfacePermissionStateCode
instance NFData       NetworkInterfacePermissionStateCode
instance ToByteString NetworkInterfacePermissionStateCode
instance ToQuery      NetworkInterfacePermissionStateCode
instance ToHeader     NetworkInterfacePermissionStateCode

instance FromXML NetworkInterfacePermissionStateCode where
    parseXML = parseXMLText "NetworkInterfacePermissionStateCode"

data NetworkInterfaceStatus
  = NISAssociated
  | NISAttaching
  | NISAvailable
  | NISDetaching
  | NISInUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceStatus where
    parser = takeLowerText >>= \case
        "associated" -> pure NISAssociated
        "attaching" -> pure NISAttaching
        "available" -> pure NISAvailable
        "detaching" -> pure NISDetaching
        "in-use" -> pure NISInUse
        e -> fromTextError $ "Failure parsing NetworkInterfaceStatus from value: '" <> e
           <> "'. Accepted values: associated, attaching, available, detaching, in-use"

instance ToText NetworkInterfaceStatus where
    toText = \case
        NISAssociated -> "associated"
        NISAttaching -> "attaching"
        NISAvailable -> "available"
        NISDetaching -> "detaching"
        NISInUse -> "in-use"

instance Hashable     NetworkInterfaceStatus
instance NFData       NetworkInterfaceStatus
instance ToByteString NetworkInterfaceStatus
instance ToQuery      NetworkInterfaceStatus
instance ToHeader     NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    parseXML = parseXMLText "NetworkInterfaceStatus"

data NetworkInterfaceType
  = NITEfa
  | NITInterface
  | NITNatGateway
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceType where
    parser = takeLowerText >>= \case
        "efa" -> pure NITEfa
        "interface" -> pure NITInterface
        "natgateway" -> pure NITNatGateway
        e -> fromTextError $ "Failure parsing NetworkInterfaceType from value: '" <> e
           <> "'. Accepted values: efa, interface, natgateway"

instance ToText NetworkInterfaceType where
    toText = \case
        NITEfa -> "efa"
        NITInterface -> "interface"
        NITNatGateway -> "natGateway"

instance Hashable     NetworkInterfaceType
instance NFData       NetworkInterfaceType
instance ToByteString NetworkInterfaceType
instance ToQuery      NetworkInterfaceType
instance ToHeader     NetworkInterfaceType

instance FromXML NetworkInterfaceType where
    parseXML = parseXMLText "NetworkInterfaceType"

data OfferingClassType
  = OCTConvertible
  | OCTStandard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingClassType where
    parser = takeLowerText >>= \case
        "convertible" -> pure OCTConvertible
        "standard" -> pure OCTStandard
        e -> fromTextError $ "Failure parsing OfferingClassType from value: '" <> e
           <> "'. Accepted values: convertible, standard"

instance ToText OfferingClassType where
    toText = \case
        OCTConvertible -> "convertible"
        OCTStandard -> "standard"

instance Hashable     OfferingClassType
instance NFData       OfferingClassType
instance ToByteString OfferingClassType
instance ToQuery      OfferingClassType
instance ToHeader     OfferingClassType

instance FromXML OfferingClassType where
    parseXML = parseXMLText "OfferingClassType"

data OfferingTypeValues
  = AllUpfront
  | HeavyUtilization
  | LightUtilization
  | MediumUtilization
  | NoUpfront
  | PartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingTypeValues where
    parser = takeLowerText >>= \case
        "all upfront" -> pure AllUpfront
        "heavy utilization" -> pure HeavyUtilization
        "light utilization" -> pure LightUtilization
        "medium utilization" -> pure MediumUtilization
        "no upfront" -> pure NoUpfront
        "partial upfront" -> pure PartialUpfront
        e -> fromTextError $ "Failure parsing OfferingTypeValues from value: '" <> e
           <> "'. Accepted values: all upfront, heavy utilization, light utilization, medium utilization, no upfront, partial upfront"

instance ToText OfferingTypeValues where
    toText = \case
        AllUpfront -> "All Upfront"
        HeavyUtilization -> "Heavy Utilization"
        LightUtilization -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"
        NoUpfront -> "No Upfront"
        PartialUpfront -> "Partial Upfront"

instance Hashable     OfferingTypeValues
instance NFData       OfferingTypeValues
instance ToByteString OfferingTypeValues
instance ToQuery      OfferingTypeValues
instance ToHeader     OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"

data OnDemandAllocationStrategy
  = ODASLowestPrice
  | ODASPrioritized
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OnDemandAllocationStrategy where
    parser = takeLowerText >>= \case
        "lowestprice" -> pure ODASLowestPrice
        "prioritized" -> pure ODASPrioritized
        e -> fromTextError $ "Failure parsing OnDemandAllocationStrategy from value: '" <> e
           <> "'. Accepted values: lowestprice, prioritized"

instance ToText OnDemandAllocationStrategy where
    toText = \case
        ODASLowestPrice -> "lowestPrice"
        ODASPrioritized -> "prioritized"

instance Hashable     OnDemandAllocationStrategy
instance NFData       OnDemandAllocationStrategy
instance ToByteString OnDemandAllocationStrategy
instance ToQuery      OnDemandAllocationStrategy
instance ToHeader     OnDemandAllocationStrategy

instance FromXML OnDemandAllocationStrategy where
    parseXML = parseXMLText "OnDemandAllocationStrategy"

data OperationType
  = Add
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: add, remove"

instance ToText OperationType where
    toText = \case
        Add -> "add"
        Remove -> "remove"

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

data PaymentOption
  = POAllUpfront
  | PONoUpfront
  | POPartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PaymentOption where
    parser = takeLowerText >>= \case
        "allupfront" -> pure POAllUpfront
        "noupfront" -> pure PONoUpfront
        "partialupfront" -> pure POPartialUpfront
        e -> fromTextError $ "Failure parsing PaymentOption from value: '" <> e
           <> "'. Accepted values: allupfront, noupfront, partialupfront"

instance ToText PaymentOption where
    toText = \case
        POAllUpfront -> "AllUpfront"
        PONoUpfront -> "NoUpfront"
        POPartialUpfront -> "PartialUpfront"

instance Hashable     PaymentOption
instance NFData       PaymentOption
instance ToByteString PaymentOption
instance ToQuery      PaymentOption
instance ToHeader     PaymentOption

instance FromXML PaymentOption where
    parseXML = parseXMLText "PaymentOption"

data PermissionGroup =
  All
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PermissionGroup where
    parser = takeLowerText >>= \case
        "all" -> pure All
        e -> fromTextError $ "Failure parsing PermissionGroup from value: '" <> e
           <> "'. Accepted values: all"

instance ToText PermissionGroup where
    toText = \case
        All -> "all"

instance Hashable     PermissionGroup
instance NFData       PermissionGroup
instance ToByteString PermissionGroup
instance ToQuery      PermissionGroup
instance ToHeader     PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"

data PlacementGroupState
  = PGSAvailable
  | PGSDeleted
  | PGSDeleting
  | PGSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementGroupState where
    parser = takeLowerText >>= \case
        "available" -> pure PGSAvailable
        "deleted" -> pure PGSDeleted
        "deleting" -> pure PGSDeleting
        "pending" -> pure PGSPending
        e -> fromTextError $ "Failure parsing PlacementGroupState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText PlacementGroupState where
    toText = \case
        PGSAvailable -> "available"
        PGSDeleted -> "deleted"
        PGSDeleting -> "deleting"
        PGSPending -> "pending"

instance Hashable     PlacementGroupState
instance NFData       PlacementGroupState
instance ToByteString PlacementGroupState
instance ToQuery      PlacementGroupState
instance ToHeader     PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"

data PlacementGroupStrategy
  = PGSCluster
  | PGSPartition
  | PGSSpread
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementGroupStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure PGSCluster
        "partition" -> pure PGSPartition
        "spread" -> pure PGSSpread
        e -> fromTextError $ "Failure parsing PlacementGroupStrategy from value: '" <> e
           <> "'. Accepted values: cluster, partition, spread"

instance ToText PlacementGroupStrategy where
    toText = \case
        PGSCluster -> "cluster"
        PGSPartition -> "partition"
        PGSSpread -> "spread"

instance Hashable     PlacementGroupStrategy
instance NFData       PlacementGroupStrategy
instance ToByteString PlacementGroupStrategy
instance ToQuery      PlacementGroupStrategy
instance ToHeader     PlacementGroupStrategy

instance FromXML PlacementGroupStrategy where
    parseXML = parseXMLText "PlacementGroupStrategy"

data PlacementStrategy
  = Cluster
  | Partition
  | Spread
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "partition" -> pure Partition
        "spread" -> pure Spread
        e -> fromTextError $ "Failure parsing PlacementStrategy from value: '" <> e
           <> "'. Accepted values: cluster, partition, spread"

instance ToText PlacementStrategy where
    toText = \case
        Cluster -> "cluster"
        Partition -> "partition"
        Spread -> "spread"

instance Hashable     PlacementStrategy
instance NFData       PlacementStrategy
instance ToByteString PlacementStrategy
instance ToQuery      PlacementStrategy
instance ToHeader     PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"

data PlatformValues =
  Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformValues where
    parser = takeLowerText >>= \case
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing PlatformValues from value: '" <> e
           <> "'. Accepted values: windows"

instance ToText PlatformValues where
    toText = \case
        Windows -> "Windows"

instance Hashable     PlatformValues
instance NFData       PlatformValues
instance ToByteString PlatformValues
instance ToQuery      PlatformValues
instance ToHeader     PlatformValues

instance FromXML PlatformValues where
    parseXML = parseXMLText "PlatformValues"

data PrefixListState
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | ModifyComplete
  | ModifyFailed
  | ModifyInProgress
  | RestoreComplete
  | RestoreFailed
  | RestoreInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PrefixListState where
    parser = takeLowerText >>= \case
        "create-complete" -> pure CreateComplete
        "create-failed" -> pure CreateFailed
        "create-in-progress" -> pure CreateInProgress
        "delete-complete" -> pure DeleteComplete
        "delete-failed" -> pure DeleteFailed
        "delete-in-progress" -> pure DeleteInProgress
        "modify-complete" -> pure ModifyComplete
        "modify-failed" -> pure ModifyFailed
        "modify-in-progress" -> pure ModifyInProgress
        "restore-complete" -> pure RestoreComplete
        "restore-failed" -> pure RestoreFailed
        "restore-in-progress" -> pure RestoreInProgress
        e -> fromTextError $ "Failure parsing PrefixListState from value: '" <> e
           <> "'. Accepted values: create-complete, create-failed, create-in-progress, delete-complete, delete-failed, delete-in-progress, modify-complete, modify-failed, modify-in-progress, restore-complete, restore-failed, restore-in-progress"

instance ToText PrefixListState where
    toText = \case
        CreateComplete -> "create-complete"
        CreateFailed -> "create-failed"
        CreateInProgress -> "create-in-progress"
        DeleteComplete -> "delete-complete"
        DeleteFailed -> "delete-failed"
        DeleteInProgress -> "delete-in-progress"
        ModifyComplete -> "modify-complete"
        ModifyFailed -> "modify-failed"
        ModifyInProgress -> "modify-in-progress"
        RestoreComplete -> "restore-complete"
        RestoreFailed -> "restore-failed"
        RestoreInProgress -> "restore-in-progress"

instance Hashable     PrefixListState
instance NFData       PrefixListState
instance ToByteString PrefixListState
instance ToQuery      PrefixListState
instance ToHeader     PrefixListState

instance FromXML PrefixListState where
    parseXML = parseXMLText "PrefixListState"

data PrincipalType
  = PTAccount
  | PTAll
  | PTOrganizationUnit
  | PTRole
  | PTService
  | PTUser
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PrincipalType where
    parser = takeLowerText >>= \case
        "account" -> pure PTAccount
        "all" -> pure PTAll
        "organizationunit" -> pure PTOrganizationUnit
        "role" -> pure PTRole
        "service" -> pure PTService
        "user" -> pure PTUser
        e -> fromTextError $ "Failure parsing PrincipalType from value: '" <> e
           <> "'. Accepted values: account, all, organizationunit, role, service, user"

instance ToText PrincipalType where
    toText = \case
        PTAccount -> "Account"
        PTAll -> "All"
        PTOrganizationUnit -> "OrganizationUnit"
        PTRole -> "Role"
        PTService -> "Service"
        PTUser -> "User"

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance FromXML PrincipalType where
    parseXML = parseXMLText "PrincipalType"

data ProductCodeValues
  = Devpay
  | Marketplace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductCodeValues where
    parser = takeLowerText >>= \case
        "devpay" -> pure Devpay
        "marketplace" -> pure Marketplace
        e -> fromTextError $ "Failure parsing ProductCodeValues from value: '" <> e
           <> "'. Accepted values: devpay, marketplace"

instance ToText ProductCodeValues where
    toText = \case
        Devpay -> "devpay"
        Marketplace -> "marketplace"

instance Hashable     ProductCodeValues
instance NFData       ProductCodeValues
instance ToByteString ProductCodeValues
instance ToQuery      ProductCodeValues
instance ToHeader     ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"

data Protocol
  = PTCP
  | PUdp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Protocol where
    parser = takeLowerText >>= \case
        "tcp" -> pure PTCP
        "udp" -> pure PUdp
        e -> fromTextError $ "Failure parsing Protocol from value: '" <> e
           <> "'. Accepted values: tcp, udp"

instance ToText Protocol where
    toText = \case
        PTCP -> "tcp"
        PUdp -> "udp"

instance Hashable     Protocol
instance NFData       Protocol
instance ToByteString Protocol
instance ToQuery      Protocol
instance ToHeader     Protocol

instance FromXML Protocol where
    parseXML = parseXMLText "Protocol"

data ProtocolValue =
  Gre
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProtocolValue where
    parser = takeLowerText >>= \case
        "gre" -> pure Gre
        e -> fromTextError $ "Failure parsing ProtocolValue from value: '" <> e
           <> "'. Accepted values: gre"

instance ToText ProtocolValue where
    toText = \case
        Gre -> "gre"

instance Hashable     ProtocolValue
instance NFData       ProtocolValue
instance ToByteString ProtocolValue
instance ToQuery      ProtocolValue
instance ToHeader     ProtocolValue

instance FromXML ProtocolValue where
    parseXML = parseXMLText "ProtocolValue"

data RIProductDescription
  = RIDLinuxUnix
  | RIDLinuxUnixAmazonVPC
  | RIDWindows
  | RIDWindowsAmazonVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RIProductDescription where
    parser = takeLowerText >>= \case
        "linux/unix" -> pure RIDLinuxUnix
        "linux/unix (amazon vpc)" -> pure RIDLinuxUnixAmazonVPC
        "windows" -> pure RIDWindows
        "windows (amazon vpc)" -> pure RIDWindowsAmazonVPC
        e -> fromTextError $ "Failure parsing RIProductDescription from value: '" <> e
           <> "'. Accepted values: linux/unix, linux/unix (amazon vpc), windows, windows (amazon vpc)"

instance ToText RIProductDescription where
    toText = \case
        RIDLinuxUnix -> "Linux/UNIX"
        RIDLinuxUnixAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        RIDWindows -> "Windows"
        RIDWindowsAmazonVPC -> "Windows (Amazon VPC)"

instance Hashable     RIProductDescription
instance NFData       RIProductDescription
instance ToByteString RIProductDescription
instance ToQuery      RIProductDescription
instance ToHeader     RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"

data RecurringChargeFrequency =
  Hourly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "hourly" -> pure Hourly
        e -> fromTextError $ "Failure parsing RecurringChargeFrequency from value: '" <> e
           <> "'. Accepted values: hourly"

instance ToText RecurringChargeFrequency where
    toText = \case
        Hourly -> "Hourly"

instance Hashable     RecurringChargeFrequency
instance NFData       RecurringChargeFrequency
instance ToByteString RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    parseXML = parseXMLText "RecurringChargeFrequency"

data ReplacementStrategy =
  RSLaunch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplacementStrategy where
    parser = takeLowerText >>= \case
        "launch" -> pure RSLaunch
        e -> fromTextError $ "Failure parsing ReplacementStrategy from value: '" <> e
           <> "'. Accepted values: launch"

instance ToText ReplacementStrategy where
    toText = \case
        RSLaunch -> "launch"

instance Hashable     ReplacementStrategy
instance NFData       ReplacementStrategy
instance ToByteString ReplacementStrategy
instance ToQuery      ReplacementStrategy
instance ToHeader     ReplacementStrategy

instance FromXML ReplacementStrategy where
    parseXML = parseXMLText "ReplacementStrategy"

data ReportInstanceReasonCodes
  = InstanceStuckInState
  | NotAcceptingCredentials
  | Other
  | PasswordNotAvailable
  | PerformanceEBSVolume
  | PerformanceInstanceStore
  | PerformanceNetwork
  | PerformanceOther
  | Unresponsive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportInstanceReasonCodes where
    parser = takeLowerText >>= \case
        "instance-stuck-in-state" -> pure InstanceStuckInState
        "not-accepting-credentials" -> pure NotAcceptingCredentials
        "other" -> pure Other
        "password-not-available" -> pure PasswordNotAvailable
        "performance-ebs-volume" -> pure PerformanceEBSVolume
        "performance-instance-store" -> pure PerformanceInstanceStore
        "performance-network" -> pure PerformanceNetwork
        "performance-other" -> pure PerformanceOther
        "unresponsive" -> pure Unresponsive
        e -> fromTextError $ "Failure parsing ReportInstanceReasonCodes from value: '" <> e
           <> "'. Accepted values: instance-stuck-in-state, not-accepting-credentials, other, password-not-available, performance-ebs-volume, performance-instance-store, performance-network, performance-other, unresponsive"

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckInState -> "instance-stuck-in-state"
        NotAcceptingCredentials -> "not-accepting-credentials"
        Other -> "other"
        PasswordNotAvailable -> "password-not-available"
        PerformanceEBSVolume -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork -> "performance-network"
        PerformanceOther -> "performance-other"
        Unresponsive -> "unresponsive"

instance Hashable     ReportInstanceReasonCodes
instance NFData       ReportInstanceReasonCodes
instance ToByteString ReportInstanceReasonCodes
instance ToQuery      ReportInstanceReasonCodes
instance ToHeader     ReportInstanceReasonCodes

data ReportStatusType
  = RSTImpaired
  | RSTOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportStatusType where
    parser = takeLowerText >>= \case
        "impaired" -> pure RSTImpaired
        "ok" -> pure RSTOK
        e -> fromTextError $ "Failure parsing ReportStatusType from value: '" <> e
           <> "'. Accepted values: impaired, ok"

instance ToText ReportStatusType where
    toText = \case
        RSTImpaired -> "impaired"
        RSTOK -> "ok"

instance Hashable     ReportStatusType
instance NFData       ReportStatusType
instance ToByteString ReportStatusType
instance ToQuery      ReportStatusType
instance ToHeader     ReportStatusType

data ReservationState
  = RSActive
  | RSPaymentFailed
  | RSPaymentPending
  | RSRetired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservationState where
    parser = takeLowerText >>= \case
        "active" -> pure RSActive
        "payment-failed" -> pure RSPaymentFailed
        "payment-pending" -> pure RSPaymentPending
        "retired" -> pure RSRetired
        e -> fromTextError $ "Failure parsing ReservationState from value: '" <> e
           <> "'. Accepted values: active, payment-failed, payment-pending, retired"

instance ToText ReservationState where
    toText = \case
        RSActive -> "active"
        RSPaymentFailed -> "payment-failed"
        RSPaymentPending -> "payment-pending"
        RSRetired -> "retired"

instance Hashable     ReservationState
instance NFData       ReservationState
instance ToByteString ReservationState
instance ToQuery      ReservationState
instance ToHeader     ReservationState

instance FromXML ReservationState where
    parseXML = parseXMLText "ReservationState"

data ReservedInstanceState
  = Active
  | PaymentFailed
  | PaymentPending
  | Queued
  | QueuedDeleted
  | Retired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservedInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "payment-failed" -> pure PaymentFailed
        "payment-pending" -> pure PaymentPending
        "queued" -> pure Queued
        "queued-deleted" -> pure QueuedDeleted
        "retired" -> pure Retired
        e -> fromTextError $ "Failure parsing ReservedInstanceState from value: '" <> e
           <> "'. Accepted values: active, payment-failed, payment-pending, queued, queued-deleted, retired"

instance ToText ReservedInstanceState where
    toText = \case
        Active -> "active"
        PaymentFailed -> "payment-failed"
        PaymentPending -> "payment-pending"
        Queued -> "queued"
        QueuedDeleted -> "queued-deleted"
        Retired -> "retired"

instance Hashable     ReservedInstanceState
instance NFData       ReservedInstanceState
instance ToByteString ReservedInstanceState
instance ToQuery      ReservedInstanceState
instance ToHeader     ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"

data ResetFpgaImageAttributeName =
  LoadPermission
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResetFpgaImageAttributeName where
    parser = takeLowerText >>= \case
        "loadpermission" -> pure LoadPermission
        e -> fromTextError $ "Failure parsing ResetFpgaImageAttributeName from value: '" <> e
           <> "'. Accepted values: loadpermission"

instance ToText ResetFpgaImageAttributeName where
    toText = \case
        LoadPermission -> "loadPermission"

instance Hashable     ResetFpgaImageAttributeName
instance NFData       ResetFpgaImageAttributeName
instance ToByteString ResetFpgaImageAttributeName
instance ToQuery      ResetFpgaImageAttributeName
instance ToHeader     ResetFpgaImageAttributeName

data ResetImageAttributeName =
  RIANLaunchPermission
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResetImageAttributeName where
    parser = takeLowerText >>= \case
        "launchpermission" -> pure RIANLaunchPermission
        e -> fromTextError $ "Failure parsing ResetImageAttributeName from value: '" <> e
           <> "'. Accepted values: launchpermission"

instance ToText ResetImageAttributeName where
    toText = \case
        RIANLaunchPermission -> "launchPermission"

instance Hashable     ResetImageAttributeName
instance NFData       ResetImageAttributeName
instance ToByteString ResetImageAttributeName
instance ToQuery      ResetImageAttributeName
instance ToHeader     ResetImageAttributeName

data ResourceType
  = RTClientVPNEndpoint
  | RTCustomerGateway
  | RTDHCPOptions
  | RTDedicatedHost
  | RTEgressOnlyInternetGateway
  | RTElasticGpu
  | RTElasticIP
  | RTExportImageTask
  | RTExportInstanceTask
  | RTFleet
  | RTFpgaImage
  | RTHostReservation
  | RTImage
  | RTImportImageTask
  | RTImportSnapshotTask
  | RTInstance
  | RTInternetGateway
  | RTKeyPair
  | RTLaunchTemplate
  | RTLocalGatewayRouteTableVPCAssociation
  | RTNatgateway
  | RTNetworkACL
  | RTNetworkInsightsAnalysis
  | RTNetworkInsightsPath
  | RTNetworkInterface
  | RTPlacementGroup
  | RTReservedInstances
  | RTRouteTable
  | RTSecurityGroup
  | RTSnapshot
  | RTSpotFleetRequest
  | RTSpotInstancesRequest
  | RTSubnet
  | RTTrafficMirrorFilter
  | RTTrafficMirrorSession
  | RTTrafficMirrorTarget
  | RTTransitGateway
  | RTTransitGatewayAttachment
  | RTTransitGatewayConnectPeer
  | RTTransitGatewayMulticastDomain
  | RTTransitGatewayRouteTable
  | RTVPC
  | RTVPCFlowLog
  | RTVPCPeeringConnection
  | RTVPNConnection
  | RTVPNGateway
  | RTVolume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "client-vpn-endpoint" -> pure RTClientVPNEndpoint
        "customer-gateway" -> pure RTCustomerGateway
        "dhcp-options" -> pure RTDHCPOptions
        "dedicated-host" -> pure RTDedicatedHost
        "egress-only-internet-gateway" -> pure RTEgressOnlyInternetGateway
        "elastic-gpu" -> pure RTElasticGpu
        "elastic-ip" -> pure RTElasticIP
        "export-image-task" -> pure RTExportImageTask
        "export-instance-task" -> pure RTExportInstanceTask
        "fleet" -> pure RTFleet
        "fpga-image" -> pure RTFpgaImage
        "host-reservation" -> pure RTHostReservation
        "image" -> pure RTImage
        "import-image-task" -> pure RTImportImageTask
        "import-snapshot-task" -> pure RTImportSnapshotTask
        "instance" -> pure RTInstance
        "internet-gateway" -> pure RTInternetGateway
        "key-pair" -> pure RTKeyPair
        "launch-template" -> pure RTLaunchTemplate
        "local-gateway-route-table-vpc-association" -> pure RTLocalGatewayRouteTableVPCAssociation
        "natgateway" -> pure RTNatgateway
        "network-acl" -> pure RTNetworkACL
        "network-insights-analysis" -> pure RTNetworkInsightsAnalysis
        "network-insights-path" -> pure RTNetworkInsightsPath
        "network-interface" -> pure RTNetworkInterface
        "placement-group" -> pure RTPlacementGroup
        "reserved-instances" -> pure RTReservedInstances
        "route-table" -> pure RTRouteTable
        "security-group" -> pure RTSecurityGroup
        "snapshot" -> pure RTSnapshot
        "spot-fleet-request" -> pure RTSpotFleetRequest
        "spot-instances-request" -> pure RTSpotInstancesRequest
        "subnet" -> pure RTSubnet
        "traffic-mirror-filter" -> pure RTTrafficMirrorFilter
        "traffic-mirror-session" -> pure RTTrafficMirrorSession
        "traffic-mirror-target" -> pure RTTrafficMirrorTarget
        "transit-gateway" -> pure RTTransitGateway
        "transit-gateway-attachment" -> pure RTTransitGatewayAttachment
        "transit-gateway-connect-peer" -> pure RTTransitGatewayConnectPeer
        "transit-gateway-multicast-domain" -> pure RTTransitGatewayMulticastDomain
        "transit-gateway-route-table" -> pure RTTransitGatewayRouteTable
        "vpc" -> pure RTVPC
        "vpc-flow-log" -> pure RTVPCFlowLog
        "vpc-peering-connection" -> pure RTVPCPeeringConnection
        "vpn-connection" -> pure RTVPNConnection
        "vpn-gateway" -> pure RTVPNGateway
        "volume" -> pure RTVolume
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: client-vpn-endpoint, customer-gateway, dhcp-options, dedicated-host, egress-only-internet-gateway, elastic-gpu, elastic-ip, export-image-task, export-instance-task, fleet, fpga-image, host-reservation, image, import-image-task, import-snapshot-task, instance, internet-gateway, key-pair, launch-template, local-gateway-route-table-vpc-association, natgateway, network-acl, network-insights-analysis, network-insights-path, network-interface, placement-group, reserved-instances, route-table, security-group, snapshot, spot-fleet-request, spot-instances-request, subnet, traffic-mirror-filter, traffic-mirror-session, traffic-mirror-target, transit-gateway, transit-gateway-attachment, transit-gateway-connect-peer, transit-gateway-multicast-domain, transit-gateway-route-table, vpc, vpc-flow-log, vpc-peering-connection, vpn-connection, vpn-gateway, volume"

instance ToText ResourceType where
    toText = \case
        RTClientVPNEndpoint -> "client-vpn-endpoint"
        RTCustomerGateway -> "customer-gateway"
        RTDHCPOptions -> "dhcp-options"
        RTDedicatedHost -> "dedicated-host"
        RTEgressOnlyInternetGateway -> "egress-only-internet-gateway"
        RTElasticGpu -> "elastic-gpu"
        RTElasticIP -> "elastic-ip"
        RTExportImageTask -> "export-image-task"
        RTExportInstanceTask -> "export-instance-task"
        RTFleet -> "fleet"
        RTFpgaImage -> "fpga-image"
        RTHostReservation -> "host-reservation"
        RTImage -> "image"
        RTImportImageTask -> "import-image-task"
        RTImportSnapshotTask -> "import-snapshot-task"
        RTInstance -> "instance"
        RTInternetGateway -> "internet-gateway"
        RTKeyPair -> "key-pair"
        RTLaunchTemplate -> "launch-template"
        RTLocalGatewayRouteTableVPCAssociation -> "local-gateway-route-table-vpc-association"
        RTNatgateway -> "natgateway"
        RTNetworkACL -> "network-acl"
        RTNetworkInsightsAnalysis -> "network-insights-analysis"
        RTNetworkInsightsPath -> "network-insights-path"
        RTNetworkInterface -> "network-interface"
        RTPlacementGroup -> "placement-group"
        RTReservedInstances -> "reserved-instances"
        RTRouteTable -> "route-table"
        RTSecurityGroup -> "security-group"
        RTSnapshot -> "snapshot"
        RTSpotFleetRequest -> "spot-fleet-request"
        RTSpotInstancesRequest -> "spot-instances-request"
        RTSubnet -> "subnet"
        RTTrafficMirrorFilter -> "traffic-mirror-filter"
        RTTrafficMirrorSession -> "traffic-mirror-session"
        RTTrafficMirrorTarget -> "traffic-mirror-target"
        RTTransitGateway -> "transit-gateway"
        RTTransitGatewayAttachment -> "transit-gateway-attachment"
        RTTransitGatewayConnectPeer -> "transit-gateway-connect-peer"
        RTTransitGatewayMulticastDomain -> "transit-gateway-multicast-domain"
        RTTransitGatewayRouteTable -> "transit-gateway-route-table"
        RTVPC -> "vpc"
        RTVPCFlowLog -> "vpc-flow-log"
        RTVPCPeeringConnection -> "vpc-peering-connection"
        RTVPNConnection -> "vpn-connection"
        RTVPNGateway -> "vpn-gateway"
        RTVolume -> "volume"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"

data RootDeviceType
  = EBS
  | InstanceStore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RootDeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fromTextError $ "Failure parsing RootDeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText RootDeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable     RootDeviceType
instance NFData       RootDeviceType
instance ToByteString RootDeviceType
instance ToQuery      RootDeviceType
instance ToHeader     RootDeviceType

instance FromXML RootDeviceType where
    parseXML = parseXMLText "RootDeviceType"

data RouteOrigin
  = CreateRoute
  | CreateRouteTable
  | EnableVGWRoutePropagation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteOrigin where
    parser = takeLowerText >>= \case
        "createroute" -> pure CreateRoute
        "createroutetable" -> pure CreateRouteTable
        "enablevgwroutepropagation" -> pure EnableVGWRoutePropagation
        e -> fromTextError $ "Failure parsing RouteOrigin from value: '" <> e
           <> "'. Accepted values: createroute, createroutetable, enablevgwroutepropagation"

instance ToText RouteOrigin where
    toText = \case
        CreateRoute -> "CreateRoute"
        CreateRouteTable -> "CreateRouteTable"
        EnableVGWRoutePropagation -> "EnableVgwRoutePropagation"

instance Hashable     RouteOrigin
instance NFData       RouteOrigin
instance ToByteString RouteOrigin
instance ToQuery      RouteOrigin
instance ToHeader     RouteOrigin

instance FromXML RouteOrigin where
    parseXML = parseXMLText "RouteOrigin"

data RouteState
  = RActive
  | RBlackhole
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteState where
    parser = takeLowerText >>= \case
        "active" -> pure RActive
        "blackhole" -> pure RBlackhole
        e -> fromTextError $ "Failure parsing RouteState from value: '" <> e
           <> "'. Accepted values: active, blackhole"

instance ToText RouteState where
    toText = \case
        RActive -> "active"
        RBlackhole -> "blackhole"

instance Hashable     RouteState
instance NFData       RouteState
instance ToByteString RouteState
instance ToQuery      RouteState
instance ToHeader     RouteState

instance FromXML RouteState where
    parseXML = parseXMLText "RouteState"

data RouteTableAssociationStateCode
  = RTASCAssociated
  | RTASCAssociating
  | RTASCDisassociated
  | RTASCDisassociating
  | RTASCFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteTableAssociationStateCode where
    parser = takeLowerText >>= \case
        "associated" -> pure RTASCAssociated
        "associating" -> pure RTASCAssociating
        "disassociated" -> pure RTASCDisassociated
        "disassociating" -> pure RTASCDisassociating
        "failed" -> pure RTASCFailed
        e -> fromTextError $ "Failure parsing RouteTableAssociationStateCode from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed"

instance ToText RouteTableAssociationStateCode where
    toText = \case
        RTASCAssociated -> "associated"
        RTASCAssociating -> "associating"
        RTASCDisassociated -> "disassociated"
        RTASCDisassociating -> "disassociating"
        RTASCFailed -> "failed"

instance Hashable     RouteTableAssociationStateCode
instance NFData       RouteTableAssociationStateCode
instance ToByteString RouteTableAssociationStateCode
instance ToQuery      RouteTableAssociationStateCode
instance ToHeader     RouteTableAssociationStateCode

instance FromXML RouteTableAssociationStateCode where
    parseXML = parseXMLText "RouteTableAssociationStateCode"

data RuleAction
  = Allow
  | Deny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RuleAction where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        e -> fromTextError $ "Failure parsing RuleAction from value: '" <> e
           <> "'. Accepted values: allow, deny"

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny -> "deny"

instance Hashable     RuleAction
instance NFData       RuleAction
instance ToByteString RuleAction
instance ToQuery      RuleAction
instance ToHeader     RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"

data Scope
  = SAvailabilityZone
  | SRegion
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scope where
    parser = takeLowerText >>= \case
        "availability zone" -> pure SAvailabilityZone
        "region" -> pure SRegion
        e -> fromTextError $ "Failure parsing Scope from value: '" <> e
           <> "'. Accepted values: availability zone, region"

instance ToText Scope where
    toText = \case
        SAvailabilityZone -> "Availability Zone"
        SRegion -> "Region"

instance Hashable     Scope
instance NFData       Scope
instance ToByteString Scope
instance ToQuery      Scope
instance ToHeader     Scope

instance FromXML Scope where
    parseXML = parseXMLText "Scope"

data SelfServicePortal
  = SSPDisabled
  | SSPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SelfServicePortal where
    parser = takeLowerText >>= \case
        "disabled" -> pure SSPDisabled
        "enabled" -> pure SSPEnabled
        e -> fromTextError $ "Failure parsing SelfServicePortal from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText SelfServicePortal where
    toText = \case
        SSPDisabled -> "disabled"
        SSPEnabled -> "enabled"

instance Hashable     SelfServicePortal
instance NFData       SelfServicePortal
instance ToByteString SelfServicePortal
instance ToQuery      SelfServicePortal
instance ToHeader     SelfServicePortal

data ServiceState
  = SerAvailable
  | SerDeleted
  | SerDeleting
  | SerFailed
  | SerPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceState where
    parser = takeLowerText >>= \case
        "available" -> pure SerAvailable
        "deleted" -> pure SerDeleted
        "deleting" -> pure SerDeleting
        "failed" -> pure SerFailed
        "pending" -> pure SerPending
        e -> fromTextError $ "Failure parsing ServiceState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText ServiceState where
    toText = \case
        SerAvailable -> "Available"
        SerDeleted -> "Deleted"
        SerDeleting -> "Deleting"
        SerFailed -> "Failed"
        SerPending -> "Pending"

instance Hashable     ServiceState
instance NFData       ServiceState
instance ToByteString ServiceState
instance ToQuery      ServiceState
instance ToHeader     ServiceState

instance FromXML ServiceState where
    parseXML = parseXMLText "ServiceState"

data ServiceType
  = Gateway
  | GatewayLoadBalancer
  | Interface
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceType where
    parser = takeLowerText >>= \case
        "gateway" -> pure Gateway
        "gatewayloadbalancer" -> pure GatewayLoadBalancer
        "interface" -> pure Interface
        e -> fromTextError $ "Failure parsing ServiceType from value: '" <> e
           <> "'. Accepted values: gateway, gatewayloadbalancer, interface"

instance ToText ServiceType where
    toText = \case
        Gateway -> "Gateway"
        GatewayLoadBalancer -> "GatewayLoadBalancer"
        Interface -> "Interface"

instance Hashable     ServiceType
instance NFData       ServiceType
instance ToByteString ServiceType
instance ToQuery      ServiceType
instance ToHeader     ServiceType

instance FromXML ServiceType where
    parseXML = parseXMLText "ServiceType"

data ShutdownBehavior
  = SBStop
  | SBTerminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ShutdownBehavior where
    parser = takeLowerText >>= \case
        "stop" -> pure SBStop
        "terminate" -> pure SBTerminate
        e -> fromTextError $ "Failure parsing ShutdownBehavior from value: '" <> e
           <> "'. Accepted values: stop, terminate"

instance ToText ShutdownBehavior where
    toText = \case
        SBStop -> "stop"
        SBTerminate -> "terminate"

instance Hashable     ShutdownBehavior
instance NFData       ShutdownBehavior
instance ToByteString ShutdownBehavior
instance ToQuery      ShutdownBehavior
instance ToHeader     ShutdownBehavior

instance FromXML ShutdownBehavior where
    parseXML = parseXMLText "ShutdownBehavior"

data SnapshotAttributeName
  = SANCreateVolumePermission
  | SANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotAttributeName where
    parser = takeLowerText >>= \case
        "createvolumepermission" -> pure SANCreateVolumePermission
        "productcodes" -> pure SANProductCodes
        e -> fromTextError $ "Failure parsing SnapshotAttributeName from value: '" <> e
           <> "'. Accepted values: createvolumepermission, productcodes"

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes -> "productCodes"

instance Hashable     SnapshotAttributeName
instance NFData       SnapshotAttributeName
instance ToByteString SnapshotAttributeName
instance ToQuery      SnapshotAttributeName
instance ToHeader     SnapshotAttributeName

data SnapshotState
  = SSCompleted
  | SSError'
  | SSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "error" -> pure SSError'
        "pending" -> pure SSPending
        e -> fromTextError $ "Failure parsing SnapshotState from value: '" <> e
           <> "'. Accepted values: completed, error, pending"

instance ToText SnapshotState where
    toText = \case
        SSCompleted -> "completed"
        SSError' -> "error"
        SSPending -> "pending"

instance Hashable     SnapshotState
instance NFData       SnapshotState
instance ToByteString SnapshotState
instance ToQuery      SnapshotState
instance ToHeader     SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"

data SpotAllocationStrategy
  = CapacityOptimized
  | Diversified
  | LowestPrice
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotAllocationStrategy where
    parser = takeLowerText >>= \case
        "capacity-optimized" -> pure CapacityOptimized
        "diversified" -> pure Diversified
        "lowest-price" -> pure LowestPrice
        e -> fromTextError $ "Failure parsing SpotAllocationStrategy from value: '" <> e
           <> "'. Accepted values: capacity-optimized, diversified, lowest-price"

instance ToText SpotAllocationStrategy where
    toText = \case
        CapacityOptimized -> "capacity-optimized"
        Diversified -> "diversified"
        LowestPrice -> "lowest-price"

instance Hashable     SpotAllocationStrategy
instance NFData       SpotAllocationStrategy
instance ToByteString SpotAllocationStrategy
instance ToQuery      SpotAllocationStrategy
instance ToHeader     SpotAllocationStrategy

instance FromXML SpotAllocationStrategy where
    parseXML = parseXMLText "SpotAllocationStrategy"

data SpotInstanceInterruptionBehavior
  = SIIBHibernate
  | SIIBStop
  | SIIBTerminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceInterruptionBehavior where
    parser = takeLowerText >>= \case
        "hibernate" -> pure SIIBHibernate
        "stop" -> pure SIIBStop
        "terminate" -> pure SIIBTerminate
        e -> fromTextError $ "Failure parsing SpotInstanceInterruptionBehavior from value: '" <> e
           <> "'. Accepted values: hibernate, stop, terminate"

instance ToText SpotInstanceInterruptionBehavior where
    toText = \case
        SIIBHibernate -> "hibernate"
        SIIBStop -> "stop"
        SIIBTerminate -> "terminate"

instance Hashable     SpotInstanceInterruptionBehavior
instance NFData       SpotInstanceInterruptionBehavior
instance ToByteString SpotInstanceInterruptionBehavior
instance ToQuery      SpotInstanceInterruptionBehavior
instance ToHeader     SpotInstanceInterruptionBehavior

instance FromXML SpotInstanceInterruptionBehavior where
    parseXML = parseXMLText "SpotInstanceInterruptionBehavior"

data SpotInstanceState
  = SISActive
  | SISCancelled
  | SISClosed
  | SISFailed
  | SISOpen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure SISActive
        "cancelled" -> pure SISCancelled
        "closed" -> pure SISClosed
        "failed" -> pure SISFailed
        "open" -> pure SISOpen
        e -> fromTextError $ "Failure parsing SpotInstanceState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, failed, open"

instance ToText SpotInstanceState where
    toText = \case
        SISActive -> "active"
        SISCancelled -> "cancelled"
        SISClosed -> "closed"
        SISFailed -> "failed"
        SISOpen -> "open"

instance Hashable     SpotInstanceState
instance NFData       SpotInstanceState
instance ToByteString SpotInstanceState
instance ToQuery      SpotInstanceState
instance ToHeader     SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"

data SpotInstanceType
  = OneTime
  | Persistent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceType where
    parser = takeLowerText >>= \case
        "one-time" -> pure OneTime
        "persistent" -> pure Persistent
        e -> fromTextError $ "Failure parsing SpotInstanceType from value: '" <> e
           <> "'. Accepted values: one-time, persistent"

instance ToText SpotInstanceType where
    toText = \case
        OneTime -> "one-time"
        Persistent -> "persistent"

instance Hashable     SpotInstanceType
instance NFData       SpotInstanceType
instance ToByteString SpotInstanceType
instance ToQuery      SpotInstanceType
instance ToHeader     SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"

data State
  = SAvailable
  | SDeleted
  | SDeleting
  | SExpired
  | SFailed
  | SPending
  | SPendingAcceptance
  | SRejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText State where
    parser = takeLowerText >>= \case
        "available" -> pure SAvailable
        "deleted" -> pure SDeleted
        "deleting" -> pure SDeleting
        "expired" -> pure SExpired
        "failed" -> pure SFailed
        "pending" -> pure SPending
        "pendingacceptance" -> pure SPendingAcceptance
        "rejected" -> pure SRejected
        e -> fromTextError $ "Failure parsing State from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, expired, failed, pending, pendingacceptance, rejected"

instance ToText State where
    toText = \case
        SAvailable -> "Available"
        SDeleted -> "Deleted"
        SDeleting -> "Deleting"
        SExpired -> "Expired"
        SFailed -> "Failed"
        SPending -> "Pending"
        SPendingAcceptance -> "PendingAcceptance"
        SRejected -> "Rejected"

instance Hashable     State
instance NFData       State
instance ToByteString State
instance ToQuery      State
instance ToHeader     State

instance FromXML State where
    parseXML = parseXMLText "State"

data StaticSourcesSupportValue
  = SSSVDisable
  | SSSVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StaticSourcesSupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure SSSVDisable
        "enable" -> pure SSSVEnable
        e -> fromTextError $ "Failure parsing StaticSourcesSupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText StaticSourcesSupportValue where
    toText = \case
        SSSVDisable -> "disable"
        SSSVEnable -> "enable"

instance Hashable     StaticSourcesSupportValue
instance NFData       StaticSourcesSupportValue
instance ToByteString StaticSourcesSupportValue
instance ToQuery      StaticSourcesSupportValue
instance ToHeader     StaticSourcesSupportValue

instance FromXML StaticSourcesSupportValue where
    parseXML = parseXMLText "StaticSourcesSupportValue"

data StatusName =
  Reachability
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusName where
    parser = takeLowerText >>= \case
        "reachability" -> pure Reachability
        e -> fromTextError $ "Failure parsing StatusName from value: '" <> e
           <> "'. Accepted values: reachability"

instance ToText StatusName where
    toText = \case
        Reachability -> "reachability"

instance Hashable     StatusName
instance NFData       StatusName
instance ToByteString StatusName
instance ToQuery      StatusName
instance ToHeader     StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"

data StatusType
  = STFailed
  | STInitializing
  | STInsufficientData
  | STPassed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure STFailed
        "initializing" -> pure STInitializing
        "insufficient-data" -> pure STInsufficientData
        "passed" -> pure STPassed
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: failed, initializing, insufficient-data, passed"

instance ToText StatusType where
    toText = \case
        STFailed -> "failed"
        STInitializing -> "initializing"
        STInsufficientData -> "insufficient-data"
        STPassed -> "passed"

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SubnetCidrBlockStateCode
  = SCBSCAssociated
  | SCBSCAssociating
  | SCBSCDisassociated
  | SCBSCDisassociating
  | SCBSCFailed
  | SCBSCFailing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubnetCidrBlockStateCode where
    parser = takeLowerText >>= \case
        "associated" -> pure SCBSCAssociated
        "associating" -> pure SCBSCAssociating
        "disassociated" -> pure SCBSCDisassociated
        "disassociating" -> pure SCBSCDisassociating
        "failed" -> pure SCBSCFailed
        "failing" -> pure SCBSCFailing
        e -> fromTextError $ "Failure parsing SubnetCidrBlockStateCode from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText SubnetCidrBlockStateCode where
    toText = \case
        SCBSCAssociated -> "associated"
        SCBSCAssociating -> "associating"
        SCBSCDisassociated -> "disassociated"
        SCBSCDisassociating -> "disassociating"
        SCBSCFailed -> "failed"
        SCBSCFailing -> "failing"

instance Hashable     SubnetCidrBlockStateCode
instance NFData       SubnetCidrBlockStateCode
instance ToByteString SubnetCidrBlockStateCode
instance ToQuery      SubnetCidrBlockStateCode
instance ToHeader     SubnetCidrBlockStateCode

instance FromXML SubnetCidrBlockStateCode where
    parseXML = parseXMLText "SubnetCidrBlockStateCode"

data SubnetState
  = SubAvailable
  | SubPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubnetState where
    parser = takeLowerText >>= \case
        "available" -> pure SubAvailable
        "pending" -> pure SubPending
        e -> fromTextError $ "Failure parsing SubnetState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText SubnetState where
    toText = \case
        SubAvailable -> "available"
        SubPending -> "pending"

instance Hashable     SubnetState
instance NFData       SubnetState
instance ToByteString SubnetState
instance ToQuery      SubnetState
instance ToHeader     SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"

data SummaryStatus
  = SSImpaired
  | SSInitializing
  | SSInsufficientData
  | SSNotApplicable
  | SSOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SummaryStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure SSImpaired
        "initializing" -> pure SSInitializing
        "insufficient-data" -> pure SSInsufficientData
        "not-applicable" -> pure SSNotApplicable
        "ok" -> pure SSOK
        e -> fromTextError $ "Failure parsing SummaryStatus from value: '" <> e
           <> "'. Accepted values: impaired, initializing, insufficient-data, not-applicable, ok"

instance ToText SummaryStatus where
    toText = \case
        SSImpaired -> "impaired"
        SSInitializing -> "initializing"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable -> "not-applicable"
        SSOK -> "ok"

instance Hashable     SummaryStatus
instance NFData       SummaryStatus
instance ToByteString SummaryStatus
instance ToQuery      SummaryStatus
instance ToHeader     SummaryStatus

instance FromXML SummaryStatus where
    parseXML = parseXMLText "SummaryStatus"

data TelemetryStatus
  = Down
  | UP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TelemetryStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up" -> pure UP
        e -> fromTextError $ "Failure parsing TelemetryStatus from value: '" <> e
           <> "'. Accepted values: down, up"

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        UP -> "UP"

instance Hashable     TelemetryStatus
instance NFData       TelemetryStatus
instance ToByteString TelemetryStatus
instance ToQuery      TelemetryStatus
instance ToHeader     TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

data Tenancy
  = Dedicated
  | Default
  | Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Tenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default" -> pure Default
        "host" -> pure Host
        e -> fromTextError $ "Failure parsing Tenancy from value: '" <> e
           <> "'. Accepted values: dedicated, default, host"

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default -> "default"
        Host -> "host"

instance Hashable     Tenancy
instance NFData       Tenancy
instance ToByteString Tenancy
instance ToQuery      Tenancy
instance ToHeader     Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"

data TrafficDirection
  = Egress
  | Ingress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficDirection where
    parser = takeLowerText >>= \case
        "egress" -> pure Egress
        "ingress" -> pure Ingress
        e -> fromTextError $ "Failure parsing TrafficDirection from value: '" <> e
           <> "'. Accepted values: egress, ingress"

instance ToText TrafficDirection where
    toText = \case
        Egress -> "egress"
        Ingress -> "ingress"

instance Hashable     TrafficDirection
instance NFData       TrafficDirection
instance ToByteString TrafficDirection
instance ToQuery      TrafficDirection
instance ToHeader     TrafficDirection

instance FromXML TrafficDirection where
    parseXML = parseXMLText "TrafficDirection"

data TrafficMirrorFilterRuleField
  = TMFRFDescription
  | TMFRFDestinationPortRange
  | TMFRFProtocol
  | TMFRFSourcePortRange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficMirrorFilterRuleField where
    parser = takeLowerText >>= \case
        "description" -> pure TMFRFDescription
        "destination-port-range" -> pure TMFRFDestinationPortRange
        "protocol" -> pure TMFRFProtocol
        "source-port-range" -> pure TMFRFSourcePortRange
        e -> fromTextError $ "Failure parsing TrafficMirrorFilterRuleField from value: '" <> e
           <> "'. Accepted values: description, destination-port-range, protocol, source-port-range"

instance ToText TrafficMirrorFilterRuleField where
    toText = \case
        TMFRFDescription -> "description"
        TMFRFDestinationPortRange -> "destination-port-range"
        TMFRFProtocol -> "protocol"
        TMFRFSourcePortRange -> "source-port-range"

instance Hashable     TrafficMirrorFilterRuleField
instance NFData       TrafficMirrorFilterRuleField
instance ToByteString TrafficMirrorFilterRuleField
instance ToQuery      TrafficMirrorFilterRuleField
instance ToHeader     TrafficMirrorFilterRuleField

data TrafficMirrorNetworkService =
  AmazonDNS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficMirrorNetworkService where
    parser = takeLowerText >>= \case
        "amazon-dns" -> pure AmazonDNS
        e -> fromTextError $ "Failure parsing TrafficMirrorNetworkService from value: '" <> e
           <> "'. Accepted values: amazon-dns"

instance ToText TrafficMirrorNetworkService where
    toText = \case
        AmazonDNS -> "amazon-dns"

instance Hashable     TrafficMirrorNetworkService
instance NFData       TrafficMirrorNetworkService
instance ToByteString TrafficMirrorNetworkService
instance ToQuery      TrafficMirrorNetworkService
instance ToHeader     TrafficMirrorNetworkService

instance FromXML TrafficMirrorNetworkService where
    parseXML = parseXMLText "TrafficMirrorNetworkService"

data TrafficMirrorRuleAction
  = Accept
  | Reject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficMirrorRuleAction where
    parser = takeLowerText >>= \case
        "accept" -> pure Accept
        "reject" -> pure Reject
        e -> fromTextError $ "Failure parsing TrafficMirrorRuleAction from value: '" <> e
           <> "'. Accepted values: accept, reject"

instance ToText TrafficMirrorRuleAction where
    toText = \case
        Accept -> "accept"
        Reject -> "reject"

instance Hashable     TrafficMirrorRuleAction
instance NFData       TrafficMirrorRuleAction
instance ToByteString TrafficMirrorRuleAction
instance ToQuery      TrafficMirrorRuleAction
instance ToHeader     TrafficMirrorRuleAction

instance FromXML TrafficMirrorRuleAction where
    parseXML = parseXMLText "TrafficMirrorRuleAction"

data TrafficMirrorSessionField
  = TMSFDescription
  | TMSFPacketLength
  | TMSFVirtualNetworkId
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficMirrorSessionField where
    parser = takeLowerText >>= \case
        "description" -> pure TMSFDescription
        "packet-length" -> pure TMSFPacketLength
        "virtual-network-id" -> pure TMSFVirtualNetworkId
        e -> fromTextError $ "Failure parsing TrafficMirrorSessionField from value: '" <> e
           <> "'. Accepted values: description, packet-length, virtual-network-id"

instance ToText TrafficMirrorSessionField where
    toText = \case
        TMSFDescription -> "description"
        TMSFPacketLength -> "packet-length"
        TMSFVirtualNetworkId -> "virtual-network-id"

instance Hashable     TrafficMirrorSessionField
instance NFData       TrafficMirrorSessionField
instance ToByteString TrafficMirrorSessionField
instance ToQuery      TrafficMirrorSessionField
instance ToHeader     TrafficMirrorSessionField

data TrafficMirrorTargetType
  = NetworkInterface
  | NetworkLoadBalancer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficMirrorTargetType where
    parser = takeLowerText >>= \case
        "network-interface" -> pure NetworkInterface
        "network-load-balancer" -> pure NetworkLoadBalancer
        e -> fromTextError $ "Failure parsing TrafficMirrorTargetType from value: '" <> e
           <> "'. Accepted values: network-interface, network-load-balancer"

instance ToText TrafficMirrorTargetType where
    toText = \case
        NetworkInterface -> "network-interface"
        NetworkLoadBalancer -> "network-load-balancer"

instance Hashable     TrafficMirrorTargetType
instance NFData       TrafficMirrorTargetType
instance ToByteString TrafficMirrorTargetType
instance ToQuery      TrafficMirrorTargetType
instance ToHeader     TrafficMirrorTargetType

instance FromXML TrafficMirrorTargetType where
    parseXML = parseXMLText "TrafficMirrorTargetType"

data TrafficType
  = TTAccept
  | TTAll
  | TTReject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficType where
    parser = takeLowerText >>= \case
        "accept" -> pure TTAccept
        "all" -> pure TTAll
        "reject" -> pure TTReject
        e -> fromTextError $ "Failure parsing TrafficType from value: '" <> e
           <> "'. Accepted values: accept, all, reject"

instance ToText TrafficType where
    toText = \case
        TTAccept -> "ACCEPT"
        TTAll -> "ALL"
        TTReject -> "REJECT"

instance Hashable     TrafficType
instance NFData       TrafficType
instance ToByteString TrafficType
instance ToQuery      TrafficType
instance ToHeader     TrafficType

instance FromXML TrafficType where
    parseXML = parseXMLText "TrafficType"

data TransitGatewayAssociationState
  = Associated
  | Associating
  | Disassociated
  | Disassociating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayAssociationState where
    parser = takeLowerText >>= \case
        "associated" -> pure Associated
        "associating" -> pure Associating
        "disassociated" -> pure Disassociated
        "disassociating" -> pure Disassociating
        e -> fromTextError $ "Failure parsing TransitGatewayAssociationState from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText TransitGatewayAssociationState where
    toText = \case
        Associated -> "associated"
        Associating -> "associating"
        Disassociated -> "disassociated"
        Disassociating -> "disassociating"

instance Hashable     TransitGatewayAssociationState
instance NFData       TransitGatewayAssociationState
instance ToByteString TransitGatewayAssociationState
instance ToQuery      TransitGatewayAssociationState
instance ToHeader     TransitGatewayAssociationState

instance FromXML TransitGatewayAssociationState where
    parseXML = parseXMLText "TransitGatewayAssociationState"

data TransitGatewayAttachmentResourceType
  = Connect
  | DirectConnectGateway
  | Peering
  | TgwPeering
  | VPC
  | VPN
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayAttachmentResourceType where
    parser = takeLowerText >>= \case
        "connect" -> pure Connect
        "direct-connect-gateway" -> pure DirectConnectGateway
        "peering" -> pure Peering
        "tgw-peering" -> pure TgwPeering
        "vpc" -> pure VPC
        "vpn" -> pure VPN
        e -> fromTextError $ "Failure parsing TransitGatewayAttachmentResourceType from value: '" <> e
           <> "'. Accepted values: connect, direct-connect-gateway, peering, tgw-peering, vpc, vpn"

instance ToText TransitGatewayAttachmentResourceType where
    toText = \case
        Connect -> "connect"
        DirectConnectGateway -> "direct-connect-gateway"
        Peering -> "peering"
        TgwPeering -> "tgw-peering"
        VPC -> "vpc"
        VPN -> "vpn"

instance Hashable     TransitGatewayAttachmentResourceType
instance NFData       TransitGatewayAttachmentResourceType
instance ToByteString TransitGatewayAttachmentResourceType
instance ToQuery      TransitGatewayAttachmentResourceType
instance ToHeader     TransitGatewayAttachmentResourceType

instance FromXML TransitGatewayAttachmentResourceType where
    parseXML = parseXMLText "TransitGatewayAttachmentResourceType"

data TransitGatewayAttachmentState
  = TGASAvailable
  | TGASDeleted
  | TGASDeleting
  | TGASFailed
  | TGASFailing
  | TGASInitiating
  | TGASInitiatingRequest
  | TGASModifying
  | TGASPending
  | TGASPendingAcceptance
  | TGASRejected
  | TGASRejecting
  | TGASRollingBack
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayAttachmentState where
    parser = takeLowerText >>= \case
        "available" -> pure TGASAvailable
        "deleted" -> pure TGASDeleted
        "deleting" -> pure TGASDeleting
        "failed" -> pure TGASFailed
        "failing" -> pure TGASFailing
        "initiating" -> pure TGASInitiating
        "initiatingrequest" -> pure TGASInitiatingRequest
        "modifying" -> pure TGASModifying
        "pending" -> pure TGASPending
        "pendingacceptance" -> pure TGASPendingAcceptance
        "rejected" -> pure TGASRejected
        "rejecting" -> pure TGASRejecting
        "rollingback" -> pure TGASRollingBack
        e -> fromTextError $ "Failure parsing TransitGatewayAttachmentState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, failed, failing, initiating, initiatingrequest, modifying, pending, pendingacceptance, rejected, rejecting, rollingback"

instance ToText TransitGatewayAttachmentState where
    toText = \case
        TGASAvailable -> "available"
        TGASDeleted -> "deleted"
        TGASDeleting -> "deleting"
        TGASFailed -> "failed"
        TGASFailing -> "failing"
        TGASInitiating -> "initiating"
        TGASInitiatingRequest -> "initiatingRequest"
        TGASModifying -> "modifying"
        TGASPending -> "pending"
        TGASPendingAcceptance -> "pendingAcceptance"
        TGASRejected -> "rejected"
        TGASRejecting -> "rejecting"
        TGASRollingBack -> "rollingBack"

instance Hashable     TransitGatewayAttachmentState
instance NFData       TransitGatewayAttachmentState
instance ToByteString TransitGatewayAttachmentState
instance ToQuery      TransitGatewayAttachmentState
instance ToHeader     TransitGatewayAttachmentState

instance FromXML TransitGatewayAttachmentState where
    parseXML = parseXMLText "TransitGatewayAttachmentState"

data TransitGatewayConnectPeerState
  = TGCPSAvailable
  | TGCPSDeleted
  | TGCPSDeleting
  | TGCPSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayConnectPeerState where
    parser = takeLowerText >>= \case
        "available" -> pure TGCPSAvailable
        "deleted" -> pure TGCPSDeleted
        "deleting" -> pure TGCPSDeleting
        "pending" -> pure TGCPSPending
        e -> fromTextError $ "Failure parsing TransitGatewayConnectPeerState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText TransitGatewayConnectPeerState where
    toText = \case
        TGCPSAvailable -> "available"
        TGCPSDeleted -> "deleted"
        TGCPSDeleting -> "deleting"
        TGCPSPending -> "pending"

instance Hashable     TransitGatewayConnectPeerState
instance NFData       TransitGatewayConnectPeerState
instance ToByteString TransitGatewayConnectPeerState
instance ToQuery      TransitGatewayConnectPeerState
instance ToHeader     TransitGatewayConnectPeerState

instance FromXML TransitGatewayConnectPeerState where
    parseXML = parseXMLText "TransitGatewayConnectPeerState"

data TransitGatewayMulitcastDomainAssociationState
  = TGMDASAssociated
  | TGMDASAssociating
  | TGMDASDisassociated
  | TGMDASDisassociating
  | TGMDASFailed
  | TGMDASPendingAcceptance
  | TGMDASRejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayMulitcastDomainAssociationState where
    parser = takeLowerText >>= \case
        "associated" -> pure TGMDASAssociated
        "associating" -> pure TGMDASAssociating
        "disassociated" -> pure TGMDASDisassociated
        "disassociating" -> pure TGMDASDisassociating
        "failed" -> pure TGMDASFailed
        "pendingacceptance" -> pure TGMDASPendingAcceptance
        "rejected" -> pure TGMDASRejected
        e -> fromTextError $ "Failure parsing TransitGatewayMulitcastDomainAssociationState from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, pendingacceptance, rejected"

instance ToText TransitGatewayMulitcastDomainAssociationState where
    toText = \case
        TGMDASAssociated -> "associated"
        TGMDASAssociating -> "associating"
        TGMDASDisassociated -> "disassociated"
        TGMDASDisassociating -> "disassociating"
        TGMDASFailed -> "failed"
        TGMDASPendingAcceptance -> "pendingAcceptance"
        TGMDASRejected -> "rejected"

instance Hashable     TransitGatewayMulitcastDomainAssociationState
instance NFData       TransitGatewayMulitcastDomainAssociationState
instance ToByteString TransitGatewayMulitcastDomainAssociationState
instance ToQuery      TransitGatewayMulitcastDomainAssociationState
instance ToHeader     TransitGatewayMulitcastDomainAssociationState

instance FromXML TransitGatewayMulitcastDomainAssociationState where
    parseXML = parseXMLText "TransitGatewayMulitcastDomainAssociationState"

data TransitGatewayMulticastDomainState
  = TGMDSAvailable
  | TGMDSDeleted
  | TGMDSDeleting
  | TGMDSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayMulticastDomainState where
    parser = takeLowerText >>= \case
        "available" -> pure TGMDSAvailable
        "deleted" -> pure TGMDSDeleted
        "deleting" -> pure TGMDSDeleting
        "pending" -> pure TGMDSPending
        e -> fromTextError $ "Failure parsing TransitGatewayMulticastDomainState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText TransitGatewayMulticastDomainState where
    toText = \case
        TGMDSAvailable -> "available"
        TGMDSDeleted -> "deleted"
        TGMDSDeleting -> "deleting"
        TGMDSPending -> "pending"

instance Hashable     TransitGatewayMulticastDomainState
instance NFData       TransitGatewayMulticastDomainState
instance ToByteString TransitGatewayMulticastDomainState
instance ToQuery      TransitGatewayMulticastDomainState
instance ToHeader     TransitGatewayMulticastDomainState

instance FromXML TransitGatewayMulticastDomainState where
    parseXML = parseXMLText "TransitGatewayMulticastDomainState"

data TransitGatewayPrefixListReferenceState
  = TGPLRSAvailable
  | TGPLRSDeleting
  | TGPLRSModifying
  | TGPLRSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayPrefixListReferenceState where
    parser = takeLowerText >>= \case
        "available" -> pure TGPLRSAvailable
        "deleting" -> pure TGPLRSDeleting
        "modifying" -> pure TGPLRSModifying
        "pending" -> pure TGPLRSPending
        e -> fromTextError $ "Failure parsing TransitGatewayPrefixListReferenceState from value: '" <> e
           <> "'. Accepted values: available, deleting, modifying, pending"

instance ToText TransitGatewayPrefixListReferenceState where
    toText = \case
        TGPLRSAvailable -> "available"
        TGPLRSDeleting -> "deleting"
        TGPLRSModifying -> "modifying"
        TGPLRSPending -> "pending"

instance Hashable     TransitGatewayPrefixListReferenceState
instance NFData       TransitGatewayPrefixListReferenceState
instance ToByteString TransitGatewayPrefixListReferenceState
instance ToQuery      TransitGatewayPrefixListReferenceState
instance ToHeader     TransitGatewayPrefixListReferenceState

instance FromXML TransitGatewayPrefixListReferenceState where
    parseXML = parseXMLText "TransitGatewayPrefixListReferenceState"

data TransitGatewayPropagationState
  = Disabled
  | Disabling
  | Enabled
  | Enabling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayPropagationState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "disabling" -> pure Disabling
        "enabled" -> pure Enabled
        "enabling" -> pure Enabling
        e -> fromTextError $ "Failure parsing TransitGatewayPropagationState from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText TransitGatewayPropagationState where
    toText = \case
        Disabled -> "disabled"
        Disabling -> "disabling"
        Enabled -> "enabled"
        Enabling -> "enabling"

instance Hashable     TransitGatewayPropagationState
instance NFData       TransitGatewayPropagationState
instance ToByteString TransitGatewayPropagationState
instance ToQuery      TransitGatewayPropagationState
instance ToHeader     TransitGatewayPropagationState

instance FromXML TransitGatewayPropagationState where
    parseXML = parseXMLText "TransitGatewayPropagationState"

data TransitGatewayRouteState
  = TGRSActive
  | TGRSBlackhole
  | TGRSDeleted
  | TGRSDeleting
  | TGRSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayRouteState where
    parser = takeLowerText >>= \case
        "active" -> pure TGRSActive
        "blackhole" -> pure TGRSBlackhole
        "deleted" -> pure TGRSDeleted
        "deleting" -> pure TGRSDeleting
        "pending" -> pure TGRSPending
        e -> fromTextError $ "Failure parsing TransitGatewayRouteState from value: '" <> e
           <> "'. Accepted values: active, blackhole, deleted, deleting, pending"

instance ToText TransitGatewayRouteState where
    toText = \case
        TGRSActive -> "active"
        TGRSBlackhole -> "blackhole"
        TGRSDeleted -> "deleted"
        TGRSDeleting -> "deleting"
        TGRSPending -> "pending"

instance Hashable     TransitGatewayRouteState
instance NFData       TransitGatewayRouteState
instance ToByteString TransitGatewayRouteState
instance ToQuery      TransitGatewayRouteState
instance ToHeader     TransitGatewayRouteState

instance FromXML TransitGatewayRouteState where
    parseXML = parseXMLText "TransitGatewayRouteState"

data TransitGatewayRouteTableState
  = TGRTSAvailable
  | TGRTSDeleted
  | TGRTSDeleting
  | TGRTSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayRouteTableState where
    parser = takeLowerText >>= \case
        "available" -> pure TGRTSAvailable
        "deleted" -> pure TGRTSDeleted
        "deleting" -> pure TGRTSDeleting
        "pending" -> pure TGRTSPending
        e -> fromTextError $ "Failure parsing TransitGatewayRouteTableState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText TransitGatewayRouteTableState where
    toText = \case
        TGRTSAvailable -> "available"
        TGRTSDeleted -> "deleted"
        TGRTSDeleting -> "deleting"
        TGRTSPending -> "pending"

instance Hashable     TransitGatewayRouteTableState
instance NFData       TransitGatewayRouteTableState
instance ToByteString TransitGatewayRouteTableState
instance ToQuery      TransitGatewayRouteTableState
instance ToHeader     TransitGatewayRouteTableState

instance FromXML TransitGatewayRouteTableState where
    parseXML = parseXMLText "TransitGatewayRouteTableState"

data TransitGatewayRouteType
  = TGRTPropagated
  | TGRTStatic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayRouteType where
    parser = takeLowerText >>= \case
        "propagated" -> pure TGRTPropagated
        "static" -> pure TGRTStatic
        e -> fromTextError $ "Failure parsing TransitGatewayRouteType from value: '" <> e
           <> "'. Accepted values: propagated, static"

instance ToText TransitGatewayRouteType where
    toText = \case
        TGRTPropagated -> "propagated"
        TGRTStatic -> "static"

instance Hashable     TransitGatewayRouteType
instance NFData       TransitGatewayRouteType
instance ToByteString TransitGatewayRouteType
instance ToQuery      TransitGatewayRouteType
instance ToHeader     TransitGatewayRouteType

instance FromXML TransitGatewayRouteType where
    parseXML = parseXMLText "TransitGatewayRouteType"

data TransitGatewayState
  = TGSAvailable
  | TGSDeleted
  | TGSDeleting
  | TGSModifying
  | TGSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitGatewayState where
    parser = takeLowerText >>= \case
        "available" -> pure TGSAvailable
        "deleted" -> pure TGSDeleted
        "deleting" -> pure TGSDeleting
        "modifying" -> pure TGSModifying
        "pending" -> pure TGSPending
        e -> fromTextError $ "Failure parsing TransitGatewayState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, modifying, pending"

instance ToText TransitGatewayState where
    toText = \case
        TGSAvailable -> "available"
        TGSDeleted -> "deleted"
        TGSDeleting -> "deleting"
        TGSModifying -> "modifying"
        TGSPending -> "pending"

instance Hashable     TransitGatewayState
instance NFData       TransitGatewayState
instance ToByteString TransitGatewayState
instance ToQuery      TransitGatewayState
instance ToHeader     TransitGatewayState

instance FromXML TransitGatewayState where
    parseXML = parseXMLText "TransitGatewayState"

data TransportProtocol
  = TCP
  | Udp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransportProtocol where
    parser = takeLowerText >>= \case
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing TransportProtocol from value: '" <> e
           <> "'. Accepted values: tcp, udp"

instance ToText TransportProtocol where
    toText = \case
        TCP -> "tcp"
        Udp -> "udp"

instance Hashable     TransportProtocol
instance NFData       TransportProtocol
instance ToByteString TransportProtocol
instance ToQuery      TransportProtocol
instance ToHeader     TransportProtocol

instance FromXML TransportProtocol where
    parseXML = parseXMLText "TransportProtocol"

data TunnelInsideIPVersion
  = IPV4
  | IPV6
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TunnelInsideIPVersion where
    parser = takeLowerText >>= \case
        "ipv4" -> pure IPV4
        "ipv6" -> pure IPV6
        e -> fromTextError $ "Failure parsing TunnelInsideIPVersion from value: '" <> e
           <> "'. Accepted values: ipv4, ipv6"

instance ToText TunnelInsideIPVersion where
    toText = \case
        IPV4 -> "ipv4"
        IPV6 -> "ipv6"

instance Hashable     TunnelInsideIPVersion
instance NFData       TunnelInsideIPVersion
instance ToByteString TunnelInsideIPVersion
instance ToQuery      TunnelInsideIPVersion
instance ToHeader     TunnelInsideIPVersion

instance FromXML TunnelInsideIPVersion where
    parseXML = parseXMLText "TunnelInsideIPVersion"

data UnlimitedSupportedInstanceFamily
  = T2
  | T3
  | T3a
  | T4g
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UnlimitedSupportedInstanceFamily where
    parser = takeLowerText >>= \case
        "t2" -> pure T2
        "t3" -> pure T3
        "t3a" -> pure T3a
        "t4g" -> pure T4g
        e -> fromTextError $ "Failure parsing UnlimitedSupportedInstanceFamily from value: '" <> e
           <> "'. Accepted values: t2, t3, t3a, t4g"

instance ToText UnlimitedSupportedInstanceFamily where
    toText = \case
        T2 -> "t2"
        T3 -> "t3"
        T3a -> "t3a"
        T4g -> "t4g"

instance Hashable     UnlimitedSupportedInstanceFamily
instance NFData       UnlimitedSupportedInstanceFamily
instance ToByteString UnlimitedSupportedInstanceFamily
instance ToQuery      UnlimitedSupportedInstanceFamily
instance ToHeader     UnlimitedSupportedInstanceFamily

instance FromXML UnlimitedSupportedInstanceFamily where
    parseXML = parseXMLText "UnlimitedSupportedInstanceFamily"

data UnsuccessfulInstanceCreditSpecificationErrorCode
  = IncorrectInstanceState
  | InstanceCreditSpecification_NotSupported
  | InvalidInstanceId_Malformed
  | InvalidInstanceId_NotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UnsuccessfulInstanceCreditSpecificationErrorCode where
    parser = takeLowerText >>= \case
        "incorrectinstancestate" -> pure IncorrectInstanceState
        "instancecreditspecification.notsupported" -> pure InstanceCreditSpecification_NotSupported
        "invalidinstanceid.malformed" -> pure InvalidInstanceId_Malformed
        "invalidinstanceid.notfound" -> pure InvalidInstanceId_NotFound
        e -> fromTextError $ "Failure parsing UnsuccessfulInstanceCreditSpecificationErrorCode from value: '" <> e
           <> "'. Accepted values: incorrectinstancestate, instancecreditspecification.notsupported, invalidinstanceid.malformed, invalidinstanceid.notfound"

instance ToText UnsuccessfulInstanceCreditSpecificationErrorCode where
    toText = \case
        IncorrectInstanceState -> "IncorrectInstanceState"
        InstanceCreditSpecification_NotSupported -> "InstanceCreditSpecification.NotSupported"
        InvalidInstanceId_Malformed -> "InvalidInstanceID.Malformed"
        InvalidInstanceId_NotFound -> "InvalidInstanceID.NotFound"

instance Hashable     UnsuccessfulInstanceCreditSpecificationErrorCode
instance NFData       UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToByteString UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToQuery      UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToHeader     UnsuccessfulInstanceCreditSpecificationErrorCode

instance FromXML UnsuccessfulInstanceCreditSpecificationErrorCode where
    parseXML = parseXMLText "UnsuccessfulInstanceCreditSpecificationErrorCode"

data UsageClassType
  = UCTOnDemand
  | UCTSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UsageClassType where
    parser = takeLowerText >>= \case
        "on-demand" -> pure UCTOnDemand
        "spot" -> pure UCTSpot
        e -> fromTextError $ "Failure parsing UsageClassType from value: '" <> e
           <> "'. Accepted values: on-demand, spot"

instance ToText UsageClassType where
    toText = \case
        UCTOnDemand -> "on-demand"
        UCTSpot -> "spot"

instance Hashable     UsageClassType
instance NFData       UsageClassType
instance ToByteString UsageClassType
instance ToQuery      UsageClassType
instance ToHeader     UsageClassType

instance FromXML UsageClassType where
    parseXML = parseXMLText "UsageClassType"

data VPCAttributeName
  = EnableDNSHostnames
  | EnableDNSSupport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCAttributeName where
    parser = takeLowerText >>= \case
        "enablednshostnames" -> pure EnableDNSHostnames
        "enablednssupport" -> pure EnableDNSSupport
        e -> fromTextError $ "Failure parsing VPCAttributeName from value: '" <> e
           <> "'. Accepted values: enablednshostnames, enablednssupport"

instance ToText VPCAttributeName where
    toText = \case
        EnableDNSHostnames -> "enableDnsHostnames"
        EnableDNSSupport -> "enableDnsSupport"

instance Hashable     VPCAttributeName
instance NFData       VPCAttributeName
instance ToByteString VPCAttributeName
instance ToQuery      VPCAttributeName
instance ToHeader     VPCAttributeName

data VPCCidrBlockStateCode
  = VCBSCAssociated
  | VCBSCAssociating
  | VCBSCDisassociated
  | VCBSCDisassociating
  | VCBSCFailed
  | VCBSCFailing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCCidrBlockStateCode where
    parser = takeLowerText >>= \case
        "associated" -> pure VCBSCAssociated
        "associating" -> pure VCBSCAssociating
        "disassociated" -> pure VCBSCDisassociated
        "disassociating" -> pure VCBSCDisassociating
        "failed" -> pure VCBSCFailed
        "failing" -> pure VCBSCFailing
        e -> fromTextError $ "Failure parsing VPCCidrBlockStateCode from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText VPCCidrBlockStateCode where
    toText = \case
        VCBSCAssociated -> "associated"
        VCBSCAssociating -> "associating"
        VCBSCDisassociated -> "disassociated"
        VCBSCDisassociating -> "disassociating"
        VCBSCFailed -> "failed"
        VCBSCFailing -> "failing"

instance Hashable     VPCCidrBlockStateCode
instance NFData       VPCCidrBlockStateCode
instance ToByteString VPCCidrBlockStateCode
instance ToQuery      VPCCidrBlockStateCode
instance ToHeader     VPCCidrBlockStateCode

instance FromXML VPCCidrBlockStateCode where
    parseXML = parseXMLText "VPCCidrBlockStateCode"

data VPCEndpointType
  = VETGateway
  | VETGatewayLoadBalancer
  | VETInterface
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCEndpointType where
    parser = takeLowerText >>= \case
        "gateway" -> pure VETGateway
        "gatewayloadbalancer" -> pure VETGatewayLoadBalancer
        "interface" -> pure VETInterface
        e -> fromTextError $ "Failure parsing VPCEndpointType from value: '" <> e
           <> "'. Accepted values: gateway, gatewayloadbalancer, interface"

instance ToText VPCEndpointType where
    toText = \case
        VETGateway -> "Gateway"
        VETGatewayLoadBalancer -> "GatewayLoadBalancer"
        VETInterface -> "Interface"

instance Hashable     VPCEndpointType
instance NFData       VPCEndpointType
instance ToByteString VPCEndpointType
instance ToQuery      VPCEndpointType
instance ToHeader     VPCEndpointType

instance FromXML VPCEndpointType where
    parseXML = parseXMLText "VPCEndpointType"

data VPCPeeringConnectionStateReasonCode
  = VPCSRCActive
  | VPCSRCDeleted
  | VPCSRCDeleting
  | VPCSRCExpired
  | VPCSRCFailed
  | VPCSRCInitiatingRequest
  | VPCSRCPendingAcceptance
  | VPCSRCProvisioning
  | VPCSRCRejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCPeeringConnectionStateReasonCode where
    parser = takeLowerText >>= \case
        "active" -> pure VPCSRCActive
        "deleted" -> pure VPCSRCDeleted
        "deleting" -> pure VPCSRCDeleting
        "expired" -> pure VPCSRCExpired
        "failed" -> pure VPCSRCFailed
        "initiating-request" -> pure VPCSRCInitiatingRequest
        "pending-acceptance" -> pure VPCSRCPendingAcceptance
        "provisioning" -> pure VPCSRCProvisioning
        "rejected" -> pure VPCSRCRejected
        e -> fromTextError $ "Failure parsing VPCPeeringConnectionStateReasonCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleting, expired, failed, initiating-request, pending-acceptance, provisioning, rejected"

instance ToText VPCPeeringConnectionStateReasonCode where
    toText = \case
        VPCSRCActive -> "active"
        VPCSRCDeleted -> "deleted"
        VPCSRCDeleting -> "deleting"
        VPCSRCExpired -> "expired"
        VPCSRCFailed -> "failed"
        VPCSRCInitiatingRequest -> "initiating-request"
        VPCSRCPendingAcceptance -> "pending-acceptance"
        VPCSRCProvisioning -> "provisioning"
        VPCSRCRejected -> "rejected"

instance Hashable     VPCPeeringConnectionStateReasonCode
instance NFData       VPCPeeringConnectionStateReasonCode
instance ToByteString VPCPeeringConnectionStateReasonCode
instance ToQuery      VPCPeeringConnectionStateReasonCode
instance ToHeader     VPCPeeringConnectionStateReasonCode

instance FromXML VPCPeeringConnectionStateReasonCode where
    parseXML = parseXMLText "VPCPeeringConnectionStateReasonCode"

data VPCState
  = VPCSAvailable
  | VPCSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCState where
    parser = takeLowerText >>= \case
        "available" -> pure VPCSAvailable
        "pending" -> pure VPCSPending
        e -> fromTextError $ "Failure parsing VPCState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText VPCState where
    toText = \case
        VPCSAvailable -> "available"
        VPCSPending -> "pending"

instance Hashable     VPCState
instance NFData       VPCState
instance ToByteString VPCState
instance ToQuery      VPCState
instance ToHeader     VPCState

instance FromXML VPCState where
    parseXML = parseXMLText "VPCState"

data VPCTenancy =
  VTDefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCTenancy where
    parser = takeLowerText >>= \case
        "default" -> pure VTDefault
        e -> fromTextError $ "Failure parsing VPCTenancy from value: '" <> e
           <> "'. Accepted values: default"

instance ToText VPCTenancy where
    toText = \case
        VTDefault -> "default"

instance Hashable     VPCTenancy
instance NFData       VPCTenancy
instance ToByteString VPCTenancy
instance ToQuery      VPCTenancy
instance ToHeader     VPCTenancy

data VPNEcmpSupportValue
  = VESVDisable
  | VESVEnable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNEcmpSupportValue where
    parser = takeLowerText >>= \case
        "disable" -> pure VESVDisable
        "enable" -> pure VESVEnable
        e -> fromTextError $ "Failure parsing VPNEcmpSupportValue from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText VPNEcmpSupportValue where
    toText = \case
        VESVDisable -> "disable"
        VESVEnable -> "enable"

instance Hashable     VPNEcmpSupportValue
instance NFData       VPNEcmpSupportValue
instance ToByteString VPNEcmpSupportValue
instance ToQuery      VPNEcmpSupportValue
instance ToHeader     VPNEcmpSupportValue

instance FromXML VPNEcmpSupportValue where
    parseXML = parseXMLText "VPNEcmpSupportValue"

data VPNProtocol =
  Openvpn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNProtocol where
    parser = takeLowerText >>= \case
        "openvpn" -> pure Openvpn
        e -> fromTextError $ "Failure parsing VPNProtocol from value: '" <> e
           <> "'. Accepted values: openvpn"

instance ToText VPNProtocol where
    toText = \case
        Openvpn -> "openvpn"

instance Hashable     VPNProtocol
instance NFData       VPNProtocol
instance ToByteString VPNProtocol
instance ToQuery      VPNProtocol
instance ToHeader     VPNProtocol

instance FromXML VPNProtocol where
    parseXML = parseXMLText "VPNProtocol"

data VPNState
  = VSAvailable
  | VSDeleted
  | VSDeleting
  | VSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNState where
    parser = takeLowerText >>= \case
        "available" -> pure VSAvailable
        "deleted" -> pure VSDeleted
        "deleting" -> pure VSDeleting
        "pending" -> pure VSPending
        e -> fromTextError $ "Failure parsing VPNState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText VPNState where
    toText = \case
        VSAvailable -> "available"
        VSDeleted -> "deleted"
        VSDeleting -> "deleting"
        VSPending -> "pending"

instance Hashable     VPNState
instance NFData       VPNState
instance ToByteString VPNState
instance ToQuery      VPNState
instance ToHeader     VPNState

instance FromXML VPNState where
    parseXML = parseXMLText "VPNState"

data VPNStaticRouteSource =
  Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNStaticRouteSource where
    parser = takeLowerText >>= \case
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing VPNStaticRouteSource from value: '" <> e
           <> "'. Accepted values: static"

instance ToText VPNStaticRouteSource where
    toText = \case
        Static -> "Static"

instance Hashable     VPNStaticRouteSource
instance NFData       VPNStaticRouteSource
instance ToByteString VPNStaticRouteSource
instance ToQuery      VPNStaticRouteSource
instance ToHeader     VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
    parseXML = parseXMLText "VPNStaticRouteSource"

data VirtualizationType
  = HVM
  | Paravirtual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fromTextError $ "Failure parsing VirtualizationType from value: '" <> e
           <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable     VirtualizationType
instance NFData       VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromXML VirtualizationType where
    parseXML = parseXMLText "VirtualizationType"

data VolumeAttachmentState
  = VAttached
  | VAttaching
  | VBusy
  | VDetached
  | VDetaching
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeAttachmentState where
    parser = takeLowerText >>= \case
        "attached" -> pure VAttached
        "attaching" -> pure VAttaching
        "busy" -> pure VBusy
        "detached" -> pure VDetached
        "detaching" -> pure VDetaching
        e -> fromTextError $ "Failure parsing VolumeAttachmentState from value: '" <> e
           <> "'. Accepted values: attached, attaching, busy, detached, detaching"

instance ToText VolumeAttachmentState where
    toText = \case
        VAttached -> "attached"
        VAttaching -> "attaching"
        VBusy -> "busy"
        VDetached -> "detached"
        VDetaching -> "detaching"

instance Hashable     VolumeAttachmentState
instance NFData       VolumeAttachmentState
instance ToByteString VolumeAttachmentState
instance ToQuery      VolumeAttachmentState
instance ToHeader     VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"

data VolumeAttributeName
  = VANAutoEnableIO
  | VANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeAttributeName where
    parser = takeLowerText >>= \case
        "autoenableio" -> pure VANAutoEnableIO
        "productcodes" -> pure VANProductCodes
        e -> fromTextError $ "Failure parsing VolumeAttributeName from value: '" <> e
           <> "'. Accepted values: autoenableio, productcodes"

instance ToText VolumeAttributeName where
    toText = \case
        VANAutoEnableIO -> "autoEnableIO"
        VANProductCodes -> "productCodes"

instance Hashable     VolumeAttributeName
instance NFData       VolumeAttributeName
instance ToByteString VolumeAttributeName
instance ToQuery      VolumeAttributeName
instance ToHeader     VolumeAttributeName

data VolumeModificationState
  = Completed
  | Failed
  | Modifying
  | Optimizing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeModificationState where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "modifying" -> pure Modifying
        "optimizing" -> pure Optimizing
        e -> fromTextError $ "Failure parsing VolumeModificationState from value: '" <> e
           <> "'. Accepted values: completed, failed, modifying, optimizing"

instance ToText VolumeModificationState where
    toText = \case
        Completed -> "completed"
        Failed -> "failed"
        Modifying -> "modifying"
        Optimizing -> "optimizing"

instance Hashable     VolumeModificationState
instance NFData       VolumeModificationState
instance ToByteString VolumeModificationState
instance ToQuery      VolumeModificationState
instance ToHeader     VolumeModificationState

instance FromXML VolumeModificationState where
    parseXML = parseXMLText "VolumeModificationState"

data VolumeState
  = VAvailable
  | VCreating
  | VDeleted
  | VDeleting
  | VError'
  | VInUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeState where
    parser = takeLowerText >>= \case
        "available" -> pure VAvailable
        "creating" -> pure VCreating
        "deleted" -> pure VDeleted
        "deleting" -> pure VDeleting
        "error" -> pure VError'
        "in-use" -> pure VInUse
        e -> fromTextError $ "Failure parsing VolumeState from value: '" <> e
           <> "'. Accepted values: available, creating, deleted, deleting, error, in-use"

instance ToText VolumeState where
    toText = \case
        VAvailable -> "available"
        VCreating -> "creating"
        VDeleted -> "deleted"
        VDeleting -> "deleting"
        VError' -> "error"
        VInUse -> "in-use"

instance Hashable     VolumeState
instance NFData       VolumeState
instance ToByteString VolumeState
instance ToQuery      VolumeState
instance ToHeader     VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"

data VolumeStatusInfoStatus
  = Impaired
  | InsufficientData
  | OK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeStatusInfoStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "insufficient-data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing VolumeStatusInfoStatus from value: '" <> e
           <> "'. Accepted values: impaired, insufficient-data, ok"

instance ToText VolumeStatusInfoStatus where
    toText = \case
        Impaired -> "impaired"
        InsufficientData -> "insufficient-data"
        OK -> "ok"

instance Hashable     VolumeStatusInfoStatus
instance NFData       VolumeStatusInfoStatus
instance ToByteString VolumeStatusInfoStatus
instance ToQuery      VolumeStatusInfoStatus
instance ToHeader     VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"

data VolumeStatusName
  = IOEnabled
  | IOPerformance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeStatusName where
    parser = takeLowerText >>= \case
        "io-enabled" -> pure IOEnabled
        "io-performance" -> pure IOPerformance
        e -> fromTextError $ "Failure parsing VolumeStatusName from value: '" <> e
           <> "'. Accepted values: io-enabled, io-performance"

instance ToText VolumeStatusName where
    toText = \case
        IOEnabled -> "io-enabled"
        IOPerformance -> "io-performance"

instance Hashable     VolumeStatusName
instance NFData       VolumeStatusName
instance ToByteString VolumeStatusName
instance ToQuery      VolumeStatusName
instance ToHeader     VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"

data VolumeType
  = GP2
  | GP3
  | IO1
  | IO2
  | SC1
  | ST1
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "gp3" -> pure GP3
        "io1" -> pure IO1
        "io2" -> pure IO2
        "sc1" -> pure SC1
        "st1" -> pure ST1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, gp3, io1, io2, sc1, st1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        GP3 -> "gp3"
        IO1 -> "io1"
        IO2 -> "io2"
        SC1 -> "sc1"
        ST1 -> "st1"
        Standard -> "standard"

instance Hashable     VolumeType
instance NFData       VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"
