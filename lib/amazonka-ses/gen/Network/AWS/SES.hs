{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Simple Email Service__
--
-- This document contains reference information for the <https://aws.amazon.com/ses/ Amazon Simple Email Service> (Amazon SES) API, version 2010-12-01. This document is best used in conjunction with the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide> .
module Network.AWS.SES
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidConfigurationSetException
    _InvalidConfigurationSetException,

    -- ** InvalidSNSDestinationException
    _InvalidSNSDestinationException,

    -- ** TemplateDoesNotExistException
    _TemplateDoesNotExistException,

    -- ** ConfigurationSetSendingPausedException
    _ConfigurationSetSendingPausedException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** ProductionAccessNotGrantedException
    _ProductionAccessNotGrantedException,

    -- ** RuleDoesNotExistException
    _RuleDoesNotExistException,

    -- ** MessageRejected
    _MessageRejected,

    -- ** InvalidRenderingParameterException
    _InvalidRenderingParameterException,

    -- ** MissingRenderingAttributeException
    _MissingRenderingAttributeException,

    -- ** FromEmailAddressNotVerifiedException
    _FromEmailAddressNotVerifiedException,

    -- ** RuleSetDoesNotExistException
    _RuleSetDoesNotExistException,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** InvalidFirehoseDestinationException
    _InvalidFirehoseDestinationException,

    -- ** ConfigurationSetAlreadyExistsException
    _ConfigurationSetAlreadyExistsException,

    -- ** CustomVerificationEmailInvalidContentException
    _CustomVerificationEmailInvalidContentException,

    -- ** InvalidTrackingOptionsException
    _InvalidTrackingOptionsException,

    -- ** AccountSendingPausedException
    _AccountSendingPausedException,

    -- ** EventDestinationDoesNotExistException
    _EventDestinationDoesNotExistException,

    -- ** CustomVerificationEmailTemplateAlreadyExistsException
    _CustomVerificationEmailTemplateAlreadyExistsException,

    -- ** CustomVerificationEmailTemplateDoesNotExistException
    _CustomVerificationEmailTemplateDoesNotExistException,

    -- ** InvalidCloudWatchDestinationException
    _InvalidCloudWatchDestinationException,

    -- ** InvalidDeliveryOptionsException
    _InvalidDeliveryOptionsException,

    -- ** InvalidLambdaFunctionException
    _InvalidLambdaFunctionException,

    -- ** TrackingOptionsDoesNotExistException
    _TrackingOptionsDoesNotExistException,

    -- ** InvalidTemplateException
    _InvalidTemplateException,

    -- ** ConfigurationSetDoesNotExistException
    _ConfigurationSetDoesNotExistException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** InvalidS3ConfigurationException
    _InvalidS3ConfigurationException,

    -- ** TrackingOptionsAlreadyExistsException
    _TrackingOptionsAlreadyExistsException,

    -- ** InvalidSnsTopicException
    _InvalidSnsTopicException,

    -- ** EventDestinationAlreadyExistsException
    _EventDestinationAlreadyExistsException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateTemplate
    module Network.AWS.SES.CreateTemplate,

    -- ** DeleteConfigurationSetTrackingOptions
    module Network.AWS.SES.DeleteConfigurationSetTrackingOptions,

    -- ** UpdateConfigurationSetTrackingOptions
    module Network.AWS.SES.UpdateConfigurationSetTrackingOptions,

    -- ** CreateReceiptRuleSet
    module Network.AWS.SES.CreateReceiptRuleSet,

    -- ** SetIdentityHeadersInNotificationsEnabled
    module Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled,

    -- ** GetSendQuota
    module Network.AWS.SES.GetSendQuota,

    -- ** PutConfigurationSetDeliveryOptions
    module Network.AWS.SES.PutConfigurationSetDeliveryOptions,

    -- ** DescribeConfigurationSet
    module Network.AWS.SES.DescribeConfigurationSet,

    -- ** PutIdentityPolicy
    module Network.AWS.SES.PutIdentityPolicy,

    -- ** DeleteCustomVerificationEmailTemplate
    module Network.AWS.SES.DeleteCustomVerificationEmailTemplate,

    -- ** DeleteIdentityPolicy
    module Network.AWS.SES.DeleteIdentityPolicy,

    -- ** UpdateCustomVerificationEmailTemplate
    module Network.AWS.SES.UpdateCustomVerificationEmailTemplate,

    -- ** SendCustomVerificationEmail
    module Network.AWS.SES.SendCustomVerificationEmail,

    -- ** GetIdentityNotificationAttributes
    module Network.AWS.SES.GetIdentityNotificationAttributes,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    module Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled,

    -- ** ListIdentityPolicies
    module Network.AWS.SES.ListIdentityPolicies,

    -- ** SetIdentityDkimEnabled
    module Network.AWS.SES.SetIdentityDkimEnabled,

    -- ** ListReceiptFilters
    module Network.AWS.SES.ListReceiptFilters,

    -- ** DescribeReceiptRuleSet
    module Network.AWS.SES.DescribeReceiptRuleSet,

    -- ** GetIdentityMailFromDomainAttributes
    module Network.AWS.SES.GetIdentityMailFromDomainAttributes,

    -- ** CreateReceiptFilter
    module Network.AWS.SES.CreateReceiptFilter,

    -- ** UpdateConfigurationSetEventDestination
    module Network.AWS.SES.UpdateConfigurationSetEventDestination,

    -- ** DeleteConfigurationSetEventDestination
    module Network.AWS.SES.DeleteConfigurationSetEventDestination,

    -- ** SetIdentityMailFromDomain
    module Network.AWS.SES.SetIdentityMailFromDomain,

    -- ** SetIdentityFeedbackForwardingEnabled
    module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled,

    -- ** ListConfigurationSets (Paginated)
    module Network.AWS.SES.ListConfigurationSets,

    -- ** DeleteConfigurationSet
    module Network.AWS.SES.DeleteConfigurationSet,

    -- ** GetIdentityVerificationAttributes
    module Network.AWS.SES.GetIdentityVerificationAttributes,

    -- ** GetIdentityPolicies
    module Network.AWS.SES.GetIdentityPolicies,

    -- ** ListTemplates (Paginated)
    module Network.AWS.SES.ListTemplates,

    -- ** VerifyDomainIdentity
    module Network.AWS.SES.VerifyDomainIdentity,

    -- ** UpdateTemplate
    module Network.AWS.SES.UpdateTemplate,

    -- ** DeleteTemplate
    module Network.AWS.SES.DeleteTemplate,

    -- ** ReorderReceiptRuleSet
    module Network.AWS.SES.ReorderReceiptRuleSet,

    -- ** ListReceiptRuleSets (Paginated)
    module Network.AWS.SES.ListReceiptRuleSets,

    -- ** DeleteReceiptRuleSet
    module Network.AWS.SES.DeleteReceiptRuleSet,

    -- ** SetReceiptRulePosition
    module Network.AWS.SES.SetReceiptRulePosition,

    -- ** SendBounce
    module Network.AWS.SES.SendBounce,

    -- ** GetIdentityDkimAttributes
    module Network.AWS.SES.GetIdentityDkimAttributes,

    -- ** SendTemplatedEmail
    module Network.AWS.SES.SendTemplatedEmail,

    -- ** VerifyDomainDkim
    module Network.AWS.SES.VerifyDomainDkim,

    -- ** TestRenderTemplate
    module Network.AWS.SES.TestRenderTemplate,

    -- ** SendBulkTemplatedEmail
    module Network.AWS.SES.SendBulkTemplatedEmail,

    -- ** SendRawEmail
    module Network.AWS.SES.SendRawEmail,

    -- ** GetSendStatistics
    module Network.AWS.SES.GetSendStatistics,

    -- ** ListCustomVerificationEmailTemplates (Paginated)
    module Network.AWS.SES.ListCustomVerificationEmailTemplates,

    -- ** DeleteIdentity
    module Network.AWS.SES.DeleteIdentity,

    -- ** DescribeReceiptRule
    module Network.AWS.SES.DescribeReceiptRule,

    -- ** ListIdentities (Paginated)
    module Network.AWS.SES.ListIdentities,

    -- ** UpdateConfigurationSetSendingEnabled
    module Network.AWS.SES.UpdateConfigurationSetSendingEnabled,

    -- ** CreateCustomVerificationEmailTemplate
    module Network.AWS.SES.CreateCustomVerificationEmailTemplate,

    -- ** VerifyEmailIdentity
    module Network.AWS.SES.VerifyEmailIdentity,

    -- ** VerifyEmailAddress
    module Network.AWS.SES.VerifyEmailAddress,

    -- ** DeleteVerifiedEmailAddress
    module Network.AWS.SES.DeleteVerifiedEmailAddress,

    -- ** DeleteReceiptFilter
    module Network.AWS.SES.DeleteReceiptFilter,

    -- ** ListVerifiedEmailAddresses
    module Network.AWS.SES.ListVerifiedEmailAddresses,

    -- ** GetCustomVerificationEmailTemplate
    module Network.AWS.SES.GetCustomVerificationEmailTemplate,

    -- ** SetIdentityNotificationTopic
    module Network.AWS.SES.SetIdentityNotificationTopic,

    -- ** SendEmail
    module Network.AWS.SES.SendEmail,

    -- ** DeleteReceiptRule
    module Network.AWS.SES.DeleteReceiptRule,

    -- ** UpdateReceiptRule
    module Network.AWS.SES.UpdateReceiptRule,

    -- ** CloneReceiptRuleSet
    module Network.AWS.SES.CloneReceiptRuleSet,

    -- ** CreateConfigurationSetEventDestination
    module Network.AWS.SES.CreateConfigurationSetEventDestination,

    -- ** GetAccountSendingEnabled
    module Network.AWS.SES.GetAccountSendingEnabled,

    -- ** CreateReceiptRule
    module Network.AWS.SES.CreateReceiptRule,

    -- ** GetTemplate
    module Network.AWS.SES.GetTemplate,

    -- ** SetActiveReceiptRuleSet
    module Network.AWS.SES.SetActiveReceiptRuleSet,

    -- ** CreateConfigurationSet
    module Network.AWS.SES.CreateConfigurationSet,

    -- ** UpdateAccountSendingEnabled
    module Network.AWS.SES.UpdateAccountSendingEnabled,

    -- ** CreateConfigurationSetTrackingOptions
    module Network.AWS.SES.CreateConfigurationSetTrackingOptions,

    -- ** DescribeActiveReceiptRuleSet
    module Network.AWS.SES.DescribeActiveReceiptRuleSet,

    -- * Types

    -- ** Subject
    Subject (..),

    -- ** BulkEmailDestinationStatus
    BulkEmailDestinationStatus (..),
    mkBulkEmailDestinationStatus,
    bedsError,
    bedsMessageId,
    bedsStatus,

    -- ** ReceiptFilterPolicy
    ReceiptFilterPolicy (..),

    -- ** ExtensionFieldName
    ExtensionFieldName (..),

    -- ** Destination
    Destination (..),
    mkDestination,
    dBccAddresses,
    dCcAddresses,
    dToAddresses,

    -- ** HeaderValue
    HeaderValue (..),

    -- ** TemplateName
    TemplateName (..),

    -- ** ReceiptRuleSetName
    ReceiptRuleSetName (..),

    -- ** PolicyName
    PolicyName (..),

    -- ** S3KeyPrefix
    S3KeyPrefix (..),

    -- ** DiagnosticCode
    DiagnosticCode (..),

    -- ** StopScope
    StopScope (..),

    -- ** VerificationToken
    VerificationToken (..),

    -- ** IdentityDkimAttributes
    IdentityDkimAttributes (..),
    mkIdentityDkimAttributes,
    idaDkimEnabled,
    idaDkimVerificationStatus,
    idaDkimTokens,

    -- ** TextPart
    TextPart (..),

    -- ** MessageDsn
    MessageDsn (..),
    mkMessageDsn,
    mdReportingMta,
    mdArrivalDate,
    mdExtensionFields,

    -- ** ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (..),
    mkReceiptRuleSetMetadata,
    rrsmCreatedTimestamp,
    rrsmName,

    -- ** ReceiptRuleName
    ReceiptRuleName (..),

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    mkKinesisFirehoseDestination,
    kfdIAMRoleARN,
    kfdDeliveryStreamARN,

    -- ** TemplateMetadata
    TemplateMetadata (..),
    mkTemplateMetadata,
    tmCreatedTimestamp,
    tmName,

    -- ** DsnStatus
    DsnStatus (..),

    -- ** ConfigurationSetName
    ConfigurationSetName (..),

    -- ** BounceSmtpReplyCode
    BounceSmtpReplyCode (..),

    -- ** AddHeaderAction
    AddHeaderAction (..),
    mkAddHeaderAction,
    ahaHeaderName,
    ahaHeaderValue,

    -- ** BulkEmailDestination
    BulkEmailDestination (..),
    mkBulkEmailDestination,
    bedDestination,
    bedReplacementTags,
    bedReplacementTemplateData,

    -- ** SNSAction
    SNSAction (..),
    mkSNSAction,
    snsaTopicArn,
    snsaEncoding,

    -- ** CustomMailFromStatus
    CustomMailFromStatus (..),

    -- ** Body
    Body (..),
    mkBody,
    bHtml,
    bText,

    -- ** EventDestination
    EventDestination (..),
    mkEventDestination,
    edName,
    edMatchingEventTypes,
    edCloudWatchDestination,
    edEnabled,
    edKinesisFirehoseDestination,
    edSNSDestination,

    -- ** CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (..),
    mkCloudWatchDimensionConfiguration,
    cwdcDimensionName,
    cwdcDimensionValueSource,
    cwdcDefaultDimensionValue,

    -- ** DefaultDimensionValue
    DefaultDimensionValue (..),

    -- ** HeaderName
    HeaderName (..),

    -- ** Domain
    Domain (..),

    -- ** InvocationType
    InvocationType (..),

    -- ** FromAddress
    FromAddress (..),

    -- ** BulkEmailStatus
    BulkEmailStatus (..),

    -- ** IdentityVerificationAttributes
    IdentityVerificationAttributes (..),
    mkIdentityVerificationAttributes,
    ivaVerificationStatus,
    ivaVerificationToken,

    -- ** Error
    Error (..),

    -- ** SendDataPoint
    SendDataPoint (..),
    mkSendDataPoint,
    sdpBounces,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpRejects,
    sdpTimestamp,

    -- ** ExtensionFieldValue
    ExtensionFieldValue (..),

    -- ** SubjectPart
    SubjectPart (..),

    -- ** ReceiptFilterName
    ReceiptFilterName (..),

    -- ** Explanation
    Explanation (..),

    -- ** Address
    Address (..),

    -- ** ConfigurationSetAttribute
    ConfigurationSetAttribute (..),

    -- ** WorkmailAction
    WorkmailAction (..),
    mkWorkmailAction,
    waOrganizationArn,
    waTopicArn,

    -- ** IdentityType
    IdentityType (..),

    -- ** MessageTagName
    MessageTagName (..),

    -- ** DimensionValueSource
    DimensionValueSource (..),

    -- ** IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes (..),
    mkIdentityMailFromDomainAttributes,
    imfdaMailFromDomain,
    imfdaMailFromDomainStatus,
    imfdaBehaviorOnMXFailure,

    -- ** BounceAction
    BounceAction (..),
    mkBounceAction,
    baSmtpReplyCode,
    baMessage,
    baSender,
    baStatusCode,
    baTopicArn,

    -- ** LambdaAction
    LambdaAction (..),
    mkLambdaAction,
    laFunctionArn,
    laInvocationType,
    laTopicArn,

    -- ** FailureRedirectionURL
    FailureRedirectionURL (..),

    -- ** MessageTag
    MessageTag (..),
    mkMessageTag,
    mtName,
    mtValue,

    -- ** Content
    Content (..),
    mkContent,
    cData,
    cCharset,

    -- ** BouncedRecipientInfo
    BouncedRecipientInfo (..),
    mkBouncedRecipientInfo,
    briRecipient,
    briBounceType,
    briRecipientArn,
    briRecipientDsnFields,

    -- ** CloudWatchDestination
    CloudWatchDestination (..),
    mkCloudWatchDestination,
    cwdDimensionConfigurations,

    -- ** IdentityNotificationAttributes
    IdentityNotificationAttributes (..),
    mkIdentityNotificationAttributes,
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInDeliveryNotificationsEnabled,

    -- ** RemoteMta
    RemoteMta (..),

    -- ** NextToken
    NextToken (..),

    -- ** Cidr
    Cidr (..),

    -- ** ReceiptFilter
    ReceiptFilter (..),
    mkReceiptFilter,
    rfName,
    rfIpFilter,

    -- ** EventType
    EventType (..),

    -- ** DeliveryOptions
    DeliveryOptions (..),
    mkDeliveryOptions,
    doTlsPolicy,

    -- ** DsnAction
    DsnAction (..),

    -- ** EventDestinationName
    EventDestinationName (..),

    -- ** TrackingOptions
    TrackingOptions (..),
    mkTrackingOptions,
    toCustomRedirectDomain,

    -- ** DimensionName
    DimensionName (..),

    -- ** RawMessage
    RawMessage (..),
    mkRawMessage,
    rmData,

    -- ** ReceiptRule
    ReceiptRule (..),
    mkReceiptRule,
    rrName,
    rrActions,
    rrEnabled,
    rrRecipients,
    rrScanEnabled,
    rrTlsPolicy,

    -- ** SNSDestination
    SNSDestination (..),
    mkSNSDestination,
    snsdTopicARN,

    -- ** HtmlPart
    HtmlPart (..),

    -- ** Charset
    Charset (..),

    -- ** SuccessRedirectionURL
    SuccessRedirectionURL (..),

    -- ** ReceiptAction
    ReceiptAction (..),
    mkReceiptAction,
    raAddHeaderAction,
    raBounceAction,
    raLambdaAction,
    raS3Action,
    raSNSAction,
    raStopAction,
    raWorkmailAction,

    -- ** ConfigurationSet
    ConfigurationSet (..),
    mkConfigurationSet,
    csName,

    -- ** ReceiptIpFilter
    ReceiptIpFilter (..),
    mkReceiptIpFilter,
    rifPolicy,
    rifCidr,

    -- ** NotificationType
    NotificationType (..),

    -- ** VerificationStatus
    VerificationStatus (..),

    -- ** Policy
    Policy (..),

    -- ** BehaviorOnMXFailure
    BehaviorOnMXFailure (..),

    -- ** Template
    Template (..),
    mkTemplate,
    tTemplateName,
    tHtmlPart,
    tSubjectPart,
    tTextPart,

    -- ** TemplateData
    TemplateData (..),

    -- ** BounceType
    BounceType (..),

    -- ** ReputationOptions
    ReputationOptions (..),
    mkReputationOptions,
    roLastFreshStart,
    roReputationMetricsEnabled,
    roSendingEnabled,

    -- ** BounceMessage
    BounceMessage (..),

    -- ** ExtensionField
    ExtensionField (..),
    mkExtensionField,
    efName,
    efValue,

    -- ** Message
    Message (..),
    mkMessage,
    mSubject,
    mBody,

    -- ** AmazonResourceName
    AmazonResourceName (..),

    -- ** RecipientDsnFields
    RecipientDsnFields (..),
    mkRecipientDsnFields,
    rdfAction,
    rdfStatus,
    rdfDiagnosticCode,
    rdfExtensionFields,
    rdfFinalRecipient,
    rdfLastAttemptDate,
    rdfRemoteMta,

    -- ** SNSActionEncoding
    SNSActionEncoding (..),

    -- ** TlsPolicy
    TlsPolicy (..),

    -- ** Recipient
    Recipient (..),

    -- ** RenderedTemplate
    RenderedTemplate (..),

    -- ** ReportingMta
    ReportingMta (..),

    -- ** CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate (..),
    mkCustomVerificationEmailTemplate,
    cvetFailureRedirectionURL,
    cvetFromEmailAddress,
    cvetSuccessRedirectionURL,
    cvetTemplateName,
    cvetTemplateSubject,

    -- ** Identity
    Identity (..),

    -- ** StopAction
    StopAction (..),
    mkStopAction,
    sScope,
    sTopicArn,

    -- ** S3Action
    S3Action (..),
    mkS3Action,
    saBucketName,
    saKmsKeyArn,
    saObjectKeyPrefix,
    saTopicArn,

    -- ** MessageId
    MessageId (..),

    -- ** CustomRedirectDomain
    CustomRedirectDomain (..),

    -- ** TemplateContent
    TemplateContent (..),

    -- ** RuleSetName
    RuleSetName (..),

    -- ** Name
    Name (..),

    -- ** MailFromDomain
    MailFromDomain (..),

    -- ** Source
    Source (..),

    -- ** DefaultTemplateData
    DefaultTemplateData (..),

    -- ** ReturnPath
    ReturnPath (..),

    -- ** ReturnPathArn
    ReturnPathArn (..),

    -- ** SourceArn
    SourceArn (..),

    -- ** TemplateArn
    TemplateArn (..),

    -- ** FromArn
    FromArn (..),

    -- ** IAMRoleARN
    IAMRoleARN (..),

    -- ** DeliveryStreamARN
    DeliveryStreamARN (..),

    -- ** OriginalMessageId
    OriginalMessageId (..),

    -- ** BounceSender
    BounceSender (..),

    -- ** BounceSenderArn
    BounceSenderArn (..),

    -- ** ReplacementTemplateData
    ReplacementTemplateData (..),

    -- ** TopicArn
    TopicArn (..),

    -- ** OrganizationArn
    OrganizationArn (..),

    -- ** StatusCode
    StatusCode (..),

    -- ** FunctionArn
    FunctionArn (..),

    -- ** Value
    Value (..),

    -- ** Data
    Data (..),

    -- ** RecipientArn
    RecipientArn (..),

    -- ** SnsTopic
    SnsTopic (..),

    -- ** BounceTopic
    BounceTopic (..),

    -- ** ComplaintTopic
    ComplaintTopic (..),

    -- ** DeliveryTopic
    DeliveryTopic (..),

    -- ** TopicARN
    TopicARN (..),

    -- ** BucketName
    BucketName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.CloneReceiptRuleSet
import Network.AWS.SES.CreateConfigurationSet
import Network.AWS.SES.CreateConfigurationSetEventDestination
import Network.AWS.SES.CreateConfigurationSetTrackingOptions
import Network.AWS.SES.CreateCustomVerificationEmailTemplate
import Network.AWS.SES.CreateReceiptFilter
import Network.AWS.SES.CreateReceiptRule
import Network.AWS.SES.CreateReceiptRuleSet
import Network.AWS.SES.CreateTemplate
import Network.AWS.SES.DeleteConfigurationSet
import Network.AWS.SES.DeleteConfigurationSetEventDestination
import Network.AWS.SES.DeleteConfigurationSetTrackingOptions
import Network.AWS.SES.DeleteCustomVerificationEmailTemplate
import Network.AWS.SES.DeleteIdentity
import Network.AWS.SES.DeleteIdentityPolicy
import Network.AWS.SES.DeleteReceiptFilter
import Network.AWS.SES.DeleteReceiptRule
import Network.AWS.SES.DeleteReceiptRuleSet
import Network.AWS.SES.DeleteTemplate
import Network.AWS.SES.DeleteVerifiedEmailAddress
import Network.AWS.SES.DescribeActiveReceiptRuleSet
import Network.AWS.SES.DescribeConfigurationSet
import Network.AWS.SES.DescribeReceiptRule
import Network.AWS.SES.DescribeReceiptRuleSet
import Network.AWS.SES.GetAccountSendingEnabled
import Network.AWS.SES.GetCustomVerificationEmailTemplate
import Network.AWS.SES.GetIdentityDkimAttributes
import Network.AWS.SES.GetIdentityMailFromDomainAttributes
import Network.AWS.SES.GetIdentityNotificationAttributes
import Network.AWS.SES.GetIdentityPolicies
import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.GetSendQuota
import Network.AWS.SES.GetSendStatistics
import Network.AWS.SES.GetTemplate
import Network.AWS.SES.ListConfigurationSets
import Network.AWS.SES.ListCustomVerificationEmailTemplates
import Network.AWS.SES.ListIdentities
import Network.AWS.SES.ListIdentityPolicies
import Network.AWS.SES.ListReceiptFilters
import Network.AWS.SES.ListReceiptRuleSets
import Network.AWS.SES.ListTemplates
import Network.AWS.SES.ListVerifiedEmailAddresses
import Network.AWS.SES.PutConfigurationSetDeliveryOptions
import Network.AWS.SES.PutIdentityPolicy
import Network.AWS.SES.ReorderReceiptRuleSet
import Network.AWS.SES.SendBounce
import Network.AWS.SES.SendBulkTemplatedEmail
import Network.AWS.SES.SendCustomVerificationEmail
import Network.AWS.SES.SendEmail
import Network.AWS.SES.SendRawEmail
import Network.AWS.SES.SendTemplatedEmail
import Network.AWS.SES.SetActiveReceiptRuleSet
import Network.AWS.SES.SetIdentityDkimEnabled
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
import Network.AWS.SES.SetIdentityMailFromDomain
import Network.AWS.SES.SetIdentityNotificationTopic
import Network.AWS.SES.SetReceiptRulePosition
import Network.AWS.SES.TestRenderTemplate
import Network.AWS.SES.Types
import Network.AWS.SES.UpdateAccountSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetEventDestination
import Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
import Network.AWS.SES.UpdateConfigurationSetSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetTrackingOptions
import Network.AWS.SES.UpdateCustomVerificationEmailTemplate
import Network.AWS.SES.UpdateReceiptRule
import Network.AWS.SES.UpdateTemplate
import Network.AWS.SES.VerifyDomainDkim
import Network.AWS.SES.VerifyDomainIdentity
import Network.AWS.SES.VerifyEmailAddress
import Network.AWS.SES.VerifyEmailIdentity
import Network.AWS.SES.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SES'.

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
