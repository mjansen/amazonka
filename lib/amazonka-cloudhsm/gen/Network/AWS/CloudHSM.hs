{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudHSM Service__
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
module Network.AWS.CloudHSM
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** CloudHsmServiceException
    _CloudHsmServiceException,

    -- ** CloudHsmInternalException
    _CloudHsmInternalException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteHapg
    module Network.AWS.CloudHSM.DeleteHapg,

    -- ** ListHapgs (Paginated)
    module Network.AWS.CloudHSM.ListHapgs,

    -- ** ModifyLunaClient
    module Network.AWS.CloudHSM.ModifyLunaClient,

    -- ** ListHsms (Paginated)
    module Network.AWS.CloudHSM.ListHsms,

    -- ** DescribeLunaClient
    module Network.AWS.CloudHSM.DescribeLunaClient,

    -- ** ListTagsForResource
    module Network.AWS.CloudHSM.ListTagsForResource,

    -- ** CreateHapg
    module Network.AWS.CloudHSM.CreateHapg,

    -- ** CreateHsm
    module Network.AWS.CloudHSM.CreateHsm,

    -- ** RemoveTagsFromResource
    module Network.AWS.CloudHSM.RemoveTagsFromResource,

    -- ** DescribeHapg
    module Network.AWS.CloudHSM.DescribeHapg,

    -- ** CreateLunaClient
    module Network.AWS.CloudHSM.CreateLunaClient,

    -- ** ListLunaClients (Paginated)
    module Network.AWS.CloudHSM.ListLunaClients,

    -- ** AddTagsToResource
    module Network.AWS.CloudHSM.AddTagsToResource,

    -- ** GetConfig
    module Network.AWS.CloudHSM.GetConfig,

    -- ** DeleteHsm
    module Network.AWS.CloudHSM.DeleteHsm,

    -- ** DescribeHsm
    module Network.AWS.CloudHSM.DescribeHsm,

    -- ** ModifyHapg
    module Network.AWS.CloudHSM.ModifyHapg,

    -- ** DeleteLunaClient
    module Network.AWS.CloudHSM.DeleteLunaClient,

    -- ** ModifyHsm
    module Network.AWS.CloudHSM.ModifyHsm,

    -- ** ListAvailableZones
    module Network.AWS.CloudHSM.ListAvailableZones,

    -- * Types

    -- ** ClientArn
    ClientArn (..),

    -- ** IamRoleArn
    IamRoleArn (..),

    -- ** PaginationToken
    PaginationToken (..),

    -- ** ClientLabel
    ClientLabel (..),

    -- ** IpAddress
    IpAddress (..),

    -- ** EniId
    EniId (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** ClientToken
    ClientToken (..),

    -- ** HsmSerialNumber
    HsmSerialNumber (..),

    -- ** String
    String (..),

    -- ** CertificateFingerprint
    CertificateFingerprint (..),

    -- ** CloudHsmObjectState
    CloudHsmObjectState (..),

    -- ** VpcId
    VpcId (..),

    -- ** SubscriptionType
    SubscriptionType (..),

    -- ** SubnetId
    SubnetId (..),

    -- ** SshKey
    SshKey (..),

    -- ** PartitionArn
    PartitionArn (..),

    -- ** HsmStatus
    HsmStatus (..),

    -- ** PartitionSerial
    PartitionSerial (..),

    -- ** Certificate
    Certificate (..),

    -- ** TagKey
    TagKey (..),

    -- ** ExternalId
    ExternalId (..),

    -- ** HapgArn
    HapgArn (..),

    -- ** ClientVersion
    ClientVersion (..),

    -- ** AZ
    AZ (..),

    -- ** HsmArn
    HsmArn (..),

    -- ** Label
    Label (..),

    -- ** NextToken
    NextToken (..),

    -- ** Status
    Status (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ResourceArn
    ResourceArn (..),

    -- ** LastModifiedTimestamp
    LastModifiedTimestamp (..),

    -- ** AvailabilityZone
    AvailabilityZone (..),

    -- ** ServerCertLastUpdated
    ServerCertLastUpdated (..),

    -- ** SshKeyLastUpdated
    SshKeyLastUpdated (..),

    -- ** SubscriptionEndDate
    SubscriptionEndDate (..),

    -- ** SubscriptionStartDate
    SubscriptionStartDate (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.CloudHSM.AddTagsToResource
import Network.AWS.CloudHSM.CreateHapg
import Network.AWS.CloudHSM.CreateHsm
import Network.AWS.CloudHSM.CreateLunaClient
import Network.AWS.CloudHSM.DeleteHapg
import Network.AWS.CloudHSM.DeleteHsm
import Network.AWS.CloudHSM.DeleteLunaClient
import Network.AWS.CloudHSM.DescribeHapg
import Network.AWS.CloudHSM.DescribeHsm
import Network.AWS.CloudHSM.DescribeLunaClient
import Network.AWS.CloudHSM.GetConfig
import Network.AWS.CloudHSM.ListAvailableZones
import Network.AWS.CloudHSM.ListHapgs
import Network.AWS.CloudHSM.ListHsms
import Network.AWS.CloudHSM.ListLunaClients
import Network.AWS.CloudHSM.ListTagsForResource
import Network.AWS.CloudHSM.ModifyHapg
import Network.AWS.CloudHSM.ModifyHsm
import Network.AWS.CloudHSM.ModifyLunaClient
import Network.AWS.CloudHSM.RemoveTagsFromResource
import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudHSM'.

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
