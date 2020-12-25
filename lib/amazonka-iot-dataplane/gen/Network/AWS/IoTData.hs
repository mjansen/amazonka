{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS IoT__
--
-- AWS IoT-Data enables secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. It implements a broker for applications and things to publish messages over HTTP (Publish) and retrieve, update, and delete shadows. A shadow is a persistent representation of your things and their state in the AWS cloud.
-- Find the endpoint address for actions in the AWS IoT data plane by running this CLI command:
-- @aws iot describe-endpoint --endpoint-type iot:Data-ATS@
-- The service name used by <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html AWS Signature Version 4> to sign requests is: /iotdevicegateway/ .
module Network.AWS.IoTData
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** RequestEntityTooLargeException
    _RequestEntityTooLargeException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** UnsupportedDocumentEncodingException
    _UnsupportedDocumentEncodingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetThingShadow
    module Network.AWS.IoTData.GetThingShadow,

    -- ** ListNamedShadowsForThing
    module Network.AWS.IoTData.ListNamedShadowsForThing,

    -- ** DeleteThingShadow
    module Network.AWS.IoTData.DeleteThingShadow,

    -- ** UpdateThingShadow
    module Network.AWS.IoTData.UpdateThingShadow,

    -- ** Publish
    module Network.AWS.IoTData.Publish,

    -- * Types

    -- ** ShadowName
    ShadowName (..),

    -- ** Topic
    Topic (..),

    -- ** NextToken
    NextToken (..),

    -- ** ThingName
    ThingName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.IoTData.DeleteThingShadow
import Network.AWS.IoTData.GetThingShadow
import Network.AWS.IoTData.ListNamedShadowsForThing
import Network.AWS.IoTData.Publish
import Network.AWS.IoTData.Types
import Network.AWS.IoTData.UpdateThingShadow
import Network.AWS.IoTData.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTData'.

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
