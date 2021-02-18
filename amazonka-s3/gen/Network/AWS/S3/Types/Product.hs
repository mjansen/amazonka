{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Sum

-- | Specifies the days since the initiation of an incomplete multipart upload that Amazon S3 will wait before permanently removing all parts of the upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'abortIncompleteMultipartUpload' smart constructor.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { _aimuDaysAfterInitiation :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortIncompleteMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aimuDaysAfterInitiation' - Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
abortIncompleteMultipartUpload
    :: AbortIncompleteMultipartUpload
abortIncompleteMultipartUpload =
  AbortIncompleteMultipartUpload' {_aimuDaysAfterInitiation = Nothing}


-- | Specifies the number of days after which Amazon S3 aborts an incomplete multipart upload.
aimuDaysAfterInitiation :: Lens' AbortIncompleteMultipartUpload (Maybe Int)
aimuDaysAfterInitiation = lens _aimuDaysAfterInitiation (\ s a -> s{_aimuDaysAfterInitiation = a})

instance FromXML AbortIncompleteMultipartUpload where
        parseXML x
          = AbortIncompleteMultipartUpload' <$>
              (x .@? "DaysAfterInitiation")

instance Hashable AbortIncompleteMultipartUpload
         where

instance NFData AbortIncompleteMultipartUpload where

instance ToXML AbortIncompleteMultipartUpload where
        toXML AbortIncompleteMultipartUpload'{..}
          = mconcat
              ["DaysAfterInitiation" @= _aimuDaysAfterInitiation]

-- | Configures the transfer acceleration state for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Amazon S3 Transfer Acceleration> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'accelerateConfiguration' smart constructor.
newtype AccelerateConfiguration = AccelerateConfiguration'
  { _acStatus :: Maybe BucketAccelerateStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acStatus' - Specifies the transfer acceleration status of the bucket.
accelerateConfiguration
    :: AccelerateConfiguration
accelerateConfiguration = AccelerateConfiguration' {_acStatus = Nothing}


-- | Specifies the transfer acceleration status of the bucket.
acStatus :: Lens' AccelerateConfiguration (Maybe BucketAccelerateStatus)
acStatus = lens _acStatus (\ s a -> s{_acStatus = a})

instance Hashable AccelerateConfiguration where

instance NFData AccelerateConfiguration where

instance ToXML AccelerateConfiguration where
        toXML AccelerateConfiguration'{..}
          = mconcat ["Status" @= _acStatus]

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
--
--
-- /See:/ 'accessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { _acpGrants :: !(Maybe [Grant])
  , _acpOwner  :: !(Maybe Owner)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessControlPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acpGrants' - A list of grants.
--
-- * 'acpOwner' - Container for the bucket owner's display name and ID.
accessControlPolicy
    :: AccessControlPolicy
accessControlPolicy =
  AccessControlPolicy' {_acpGrants = Nothing, _acpOwner = Nothing}


-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\ s a -> s{_acpGrants = a}) . _Default . _Coerce

-- | Container for the bucket owner's display name and ID.
acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\ s a -> s{_acpOwner = a})

instance Hashable AccessControlPolicy where

instance NFData AccessControlPolicy where

instance ToXML AccessControlPolicy where
        toXML AccessControlPolicy'{..}
          = mconcat
              ["AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _acpGrants),
               "Owner" @= _acpOwner]

-- | A container for information about access control for replicas.
--
--
--
-- /See:/ 'accessControlTranslation' smart constructor.
newtype AccessControlTranslation = AccessControlTranslation'
  { _actOwner :: OwnerOverride
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessControlTranslation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actOwner' - Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
accessControlTranslation
    :: OwnerOverride -- ^ 'actOwner'
    -> AccessControlTranslation
accessControlTranslation pOwner_ =
  AccessControlTranslation' {_actOwner = pOwner_}


-- | Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
actOwner :: Lens' AccessControlTranslation OwnerOverride
actOwner = lens _actOwner (\ s a -> s{_actOwner = a})

instance FromXML AccessControlTranslation where
        parseXML x
          = AccessControlTranslation' <$> (x .@ "Owner")

instance Hashable AccessControlTranslation where

instance NFData AccessControlTranslation where

instance ToXML AccessControlTranslation where
        toXML AccessControlTranslation'{..}
          = mconcat ["Owner" @= _actOwner]

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates in any combination, and an object must match all of the predicates for the filter to apply.
--
--
--
-- /See:/ 'analyticsAndOperator' smart constructor.
data AnalyticsAndOperator = AnalyticsAndOperator'
  { _aaoPrefix :: !(Maybe Text)
  , _aaoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaoPrefix' - The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
--
-- * 'aaoTags' - The list of tags to use when evaluating an AND predicate.
analyticsAndOperator
    :: AnalyticsAndOperator
analyticsAndOperator =
  AnalyticsAndOperator' {_aaoPrefix = Nothing, _aaoTags = Nothing}


-- | The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
aaoPrefix :: Lens' AnalyticsAndOperator (Maybe Text)
aaoPrefix = lens _aaoPrefix (\ s a -> s{_aaoPrefix = a})

-- | The list of tags to use when evaluating an AND predicate.
aaoTags :: Lens' AnalyticsAndOperator [Tag]
aaoTags = lens _aaoTags (\ s a -> s{_aaoTags = a}) . _Default . _Coerce

instance FromXML AnalyticsAndOperator where
        parseXML x
          = AnalyticsAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable AnalyticsAndOperator where

instance NFData AnalyticsAndOperator where

instance ToXML AnalyticsAndOperator where
        toXML AnalyticsAndOperator'{..}
          = mconcat
              ["Prefix" @= _aaoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _aaoTags)]

-- | Specifies the configuration and any analyses for the analytics filter of an Amazon S3 bucket.
--
--
--
-- /See:/ 'analyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { _acFilter               :: !(Maybe AnalyticsFilter)
  , _acId                   :: !Text
  , _acStorageClassAnalysis :: !StorageClassAnalysis
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acFilter' - The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- * 'acId' - The ID that identifies the analytics configuration.
--
-- * 'acStorageClassAnalysis' - Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
analyticsConfiguration
    :: Text -- ^ 'acId'
    -> StorageClassAnalysis -- ^ 'acStorageClassAnalysis'
    -> AnalyticsConfiguration
analyticsConfiguration pId_ pStorageClassAnalysis_ =
  AnalyticsConfiguration'
    { _acFilter = Nothing
    , _acId = pId_
    , _acStorageClassAnalysis = pStorageClassAnalysis_
    }


-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
acFilter :: Lens' AnalyticsConfiguration (Maybe AnalyticsFilter)
acFilter = lens _acFilter (\ s a -> s{_acFilter = a})

-- | The ID that identifies the analytics configuration.
acId :: Lens' AnalyticsConfiguration Text
acId = lens _acId (\ s a -> s{_acId = a})

-- | Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
acStorageClassAnalysis :: Lens' AnalyticsConfiguration StorageClassAnalysis
acStorageClassAnalysis = lens _acStorageClassAnalysis (\ s a -> s{_acStorageClassAnalysis = a})

instance FromXML AnalyticsConfiguration where
        parseXML x
          = AnalyticsConfiguration' <$>
              (x .@? "Filter") <*> (x .@ "Id") <*>
                (x .@ "StorageClassAnalysis")

instance Hashable AnalyticsConfiguration where

instance NFData AnalyticsConfiguration where

instance ToXML AnalyticsConfiguration where
        toXML AnalyticsConfiguration'{..}
          = mconcat
              ["Filter" @= _acFilter, "Id" @= _acId,
               "StorageClassAnalysis" @= _acStorageClassAnalysis]

-- | Where to publish the analytics results.
--
--
--
-- /See:/ 'analyticsExportDestination' smart constructor.
newtype AnalyticsExportDestination = AnalyticsExportDestination'
  { _aedS3BucketDestination :: AnalyticsS3BucketDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsExportDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aedS3BucketDestination' - A destination signifying output to an S3 bucket.
analyticsExportDestination
    :: AnalyticsS3BucketDestination -- ^ 'aedS3BucketDestination'
    -> AnalyticsExportDestination
analyticsExportDestination pS3BucketDestination_ =
  AnalyticsExportDestination' {_aedS3BucketDestination = pS3BucketDestination_}


-- | A destination signifying output to an S3 bucket.
aedS3BucketDestination :: Lens' AnalyticsExportDestination AnalyticsS3BucketDestination
aedS3BucketDestination = lens _aedS3BucketDestination (\ s a -> s{_aedS3BucketDestination = a})

instance FromXML AnalyticsExportDestination where
        parseXML x
          = AnalyticsExportDestination' <$>
              (x .@ "S3BucketDestination")

instance Hashable AnalyticsExportDestination where

instance NFData AnalyticsExportDestination where

instance ToXML AnalyticsExportDestination where
        toXML AnalyticsExportDestination'{..}
          = mconcat
              ["S3BucketDestination" @= _aedS3BucketDestination]

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
--
--
-- /See:/ 'analyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { _afTag    :: !(Maybe Tag)
  , _afPrefix :: !(Maybe Text)
  , _afAnd    :: !(Maybe AnalyticsAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afTag' - The tag to use when evaluating an analytics filter.
--
-- * 'afPrefix' - The prefix to use when evaluating an analytics filter.
--
-- * 'afAnd' - A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
analyticsFilter
    :: AnalyticsFilter
analyticsFilter =
  AnalyticsFilter' {_afTag = Nothing, _afPrefix = Nothing, _afAnd = Nothing}


-- | The tag to use when evaluating an analytics filter.
afTag :: Lens' AnalyticsFilter (Maybe Tag)
afTag = lens _afTag (\ s a -> s{_afTag = a})

-- | The prefix to use when evaluating an analytics filter.
afPrefix :: Lens' AnalyticsFilter (Maybe Text)
afPrefix = lens _afPrefix (\ s a -> s{_afPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
afAnd :: Lens' AnalyticsFilter (Maybe AnalyticsAndOperator)
afAnd = lens _afAnd (\ s a -> s{_afAnd = a})

instance FromXML AnalyticsFilter where
        parseXML x
          = AnalyticsFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable AnalyticsFilter where

instance NFData AnalyticsFilter where

instance ToXML AnalyticsFilter where
        toXML AnalyticsFilter'{..}
          = mconcat
              ["Tag" @= _afTag, "Prefix" @= _afPrefix,
               "And" @= _afAnd]

-- | Contains information about where to publish the analytics results.
--
--
--
-- /See:/ 'analyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { _asbdBucketAccountId :: !(Maybe Text)
  , _asbdPrefix          :: !(Maybe Text)
  , _asbdFormat          :: !AnalyticsS3ExportFileFormat
  , _asbdBucket          :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asbdBucketAccountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- * 'asbdPrefix' - The prefix to use when exporting data. The prefix is prepended to all results.
--
-- * 'asbdFormat' - Specifies the file format used when exporting data to Amazon S3.
--
-- * 'asbdBucket' - The Amazon Resource Name (ARN) of the bucket to which data is exported.
analyticsS3BucketDestination
    :: AnalyticsS3ExportFileFormat -- ^ 'asbdFormat'
    -> BucketName -- ^ 'asbdBucket'
    -> AnalyticsS3BucketDestination
analyticsS3BucketDestination pFormat_ pBucket_ =
  AnalyticsS3BucketDestination'
    { _asbdBucketAccountId = Nothing
    , _asbdPrefix = Nothing
    , _asbdFormat = pFormat_
    , _asbdBucket = pBucket_
    }


-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
asbdBucketAccountId :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdBucketAccountId = lens _asbdBucketAccountId (\ s a -> s{_asbdBucketAccountId = a})

-- | The prefix to use when exporting data. The prefix is prepended to all results.
asbdPrefix :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdPrefix = lens _asbdPrefix (\ s a -> s{_asbdPrefix = a})

-- | Specifies the file format used when exporting data to Amazon S3.
asbdFormat :: Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
asbdFormat = lens _asbdFormat (\ s a -> s{_asbdFormat = a})

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
asbdBucket :: Lens' AnalyticsS3BucketDestination BucketName
asbdBucket = lens _asbdBucket (\ s a -> s{_asbdBucket = a})

instance FromXML AnalyticsS3BucketDestination where
        parseXML x
          = AnalyticsS3BucketDestination' <$>
              (x .@? "BucketAccountId") <*> (x .@? "Prefix") <*>
                (x .@ "Format")
                <*> (x .@ "Bucket")

instance Hashable AnalyticsS3BucketDestination where

instance NFData AnalyticsS3BucketDestination where

instance ToXML AnalyticsS3BucketDestination where
        toXML AnalyticsS3BucketDestination'{..}
          = mconcat
              ["BucketAccountId" @= _asbdBucketAccountId,
               "Prefix" @= _asbdPrefix, "Format" @= _asbdFormat,
               "Bucket" @= _asbdBucket]

-- | In terms of implementation, a Bucket is a resource. An Amazon S3 bucket name is globally unique, and the namespace is shared by all AWS accounts.
--
--
--
-- /See:/ 'bucket' smart constructor.
data Bucket = Bucket'
  { _bCreationDate :: !ISO8601
  , _bName         :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationDate' - Date the bucket was created. This date can change when making changes to your bucket, such as editing its bucket policy.
--
-- * 'bName' - The name of the bucket.
bucket
    :: UTCTime -- ^ 'bCreationDate'
    -> BucketName -- ^ 'bName'
    -> Bucket
bucket pCreationDate_ pName_ =
  Bucket' {_bCreationDate = _Time # pCreationDate_, _bName = pName_}


-- | Date the bucket was created. This date can change when making changes to your bucket, such as editing its bucket policy.
bCreationDate :: Lens' Bucket UTCTime
bCreationDate = lens _bCreationDate (\ s a -> s{_bCreationDate = a}) . _Time

-- | The name of the bucket.
bName :: Lens' Bucket BucketName
bName = lens _bName (\ s a -> s{_bName = a})

instance FromXML Bucket where
        parseXML x
          = Bucket' <$> (x .@ "CreationDate") <*> (x .@ "Name")

instance Hashable Bucket where

instance NFData Bucket where

-- | Specifies the lifecycle configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'bucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { _blcRules :: [LifecycleRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blcRules' - A lifecycle rule for individual objects in an Amazon S3 bucket.
bucketLifecycleConfiguration
    :: BucketLifecycleConfiguration
bucketLifecycleConfiguration =
  BucketLifecycleConfiguration' {_blcRules = mempty}


-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
blcRules :: Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = lens _blcRules (\ s a -> s{_blcRules = a}) . _Coerce

instance Hashable BucketLifecycleConfiguration where

instance NFData BucketLifecycleConfiguration where

instance ToXML BucketLifecycleConfiguration where
        toXML BucketLifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _blcRules]

-- | Container for logging status information.
--
--
--
-- /See:/ 'bucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
  { _blsLoggingEnabled :: Maybe LoggingEnabled
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BucketLoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blsLoggingEnabled' - Undocumented member.
bucketLoggingStatus
    :: BucketLoggingStatus
bucketLoggingStatus = BucketLoggingStatus' {_blsLoggingEnabled = Nothing}


-- | Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\ s a -> s{_blsLoggingEnabled = a})

instance Hashable BucketLoggingStatus where

instance NFData BucketLoggingStatus where

instance ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus'{..}
          = mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'corsConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
  { _ccCORSRules :: [CORSRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CORSConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCORSRules' - A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
corsConfiguration
    :: CORSConfiguration
corsConfiguration = CORSConfiguration' {_ccCORSRules = mempty}


-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\ s a -> s{_ccCORSRules = a}) . _Coerce

instance Hashable CORSConfiguration where

instance NFData CORSConfiguration where

instance ToXML CORSConfiguration where
        toXML CORSConfiguration'{..}
          = mconcat [toXMLList "CORSRule" _ccCORSRules]

-- | Specifies a cross-origin access rule for an Amazon S3 bucket.
--
--
--
-- /See:/ 'corsRule' smart constructor.
data CORSRule = CORSRule'
  { _crMaxAgeSeconds  :: !(Maybe Int)
  , _crAllowedHeaders :: !(Maybe [Text])
  , _crExposeHeaders  :: !(Maybe [Text])
  , _crAllowedMethods :: ![Text]
  , _crAllowedOrigins :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crMaxAgeSeconds' - The time in seconds that your browser is to cache the preflight response for the specified resource.
--
-- * 'crAllowedHeaders' - Headers that are specified in the @Access-Control-Request-Headers@ header. These headers are allowed in a preflight OPTIONS request. In response to any preflight OPTIONS request, Amazon S3 returns any requested headers that are allowed.
--
-- * 'crExposeHeaders' - One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- * 'crAllowedMethods' - An HTTP method that you allow the origin to execute. Valid values are @GET@ , @PUT@ , @HEAD@ , @POST@ , and @DELETE@ .
--
-- * 'crAllowedOrigins' - One or more origins you want customers to be able to access the bucket from.
corsRule
    :: CORSRule
corsRule =
  CORSRule'
    { _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crExposeHeaders = Nothing
    , _crAllowedMethods = mempty
    , _crAllowedOrigins = mempty
    }


-- | The time in seconds that your browser is to cache the preflight response for the specified resource.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a})

-- | Headers that are specified in the @Access-Control-Request-Headers@ header. These headers are allowed in a preflight OPTIONS request. In response to any preflight OPTIONS request, Amazon S3 returns any requested headers that are allowed.
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default . _Coerce

-- | One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default . _Coerce

-- | An HTTP method that you allow the origin to execute. Valid values are @GET@ , @PUT@ , @HEAD@ , @POST@ , and @DELETE@ .
crAllowedMethods :: Lens' CORSRule [Text]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Coerce

-- | One or more origins you want customers to be able to access the bucket from.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Coerce

instance FromXML CORSRule where
        parseXML x
          = CORSRule' <$>
              (x .@? "MaxAgeSeconds") <*>
                (may (parseXMLList "AllowedHeader") x)
                <*> (may (parseXMLList "ExposeHeader") x)
                <*> (parseXMLList "AllowedMethod" x)
                <*> (parseXMLList "AllowedOrigin" x)

instance Hashable CORSRule where

instance NFData CORSRule where

instance ToXML CORSRule where
        toXML CORSRule'{..}
          = mconcat
              ["MaxAgeSeconds" @= _crMaxAgeSeconds,
               toXML
                 (toXMLList "AllowedHeader" <$> _crAllowedHeaders),
               toXML
                 (toXMLList "ExposeHeader" <$> _crExposeHeaders),
               toXMLList "AllowedMethod" _crAllowedMethods,
               toXMLList "AllowedOrigin" _crAllowedOrigins]

-- | Describes how an uncompressed comma-separated values (CSV)-formatted input object is formatted.
--
--
--
-- /See:/ 'csvInput' smart constructor.
data CSVInput = CSVInput'
  { _ciQuoteCharacter             :: !(Maybe Text)
  , _ciRecordDelimiter            :: !(Maybe Text)
  , _ciAllowQuotedRecordDelimiter :: !(Maybe Bool)
  , _ciFileHeaderInfo             :: !(Maybe FileHeaderInfo)
  , _ciQuoteEscapeCharacter       :: !(Maybe Text)
  , _ciComments                   :: !(Maybe Text)
  , _ciFieldDelimiter             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CSVInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciQuoteCharacter' - A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ . Type: String Default: @"@  Ancestors: @CSV@
--
-- * 'ciRecordDelimiter' - A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
--
-- * 'ciAllowQuotedRecordDelimiter' - Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
--
-- * 'ciFileHeaderInfo' - Describes the first line of input. Valid values are:     * @NONE@ : First line is not a header.     * @IGNORE@ : First line is a header, but you can't use the header values to indicate the column in an expression. You can use column position (such as _1, _2, …) to indicate the column (@SELECT s._1 FROM OBJECT s@ ).     * @Use@ : First line is a header, and you can use the header value to identify a column in an expression (@SELECT "name" FROM OBJECT@ ).
--
-- * 'ciQuoteEscapeCharacter' - A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
--
-- * 'ciComments' - A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
--
-- * 'ciFieldDelimiter' - A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
csvInput
    :: CSVInput
csvInput =
  CSVInput'
    { _ciQuoteCharacter = Nothing
    , _ciRecordDelimiter = Nothing
    , _ciAllowQuotedRecordDelimiter = Nothing
    , _ciFileHeaderInfo = Nothing
    , _ciQuoteEscapeCharacter = Nothing
    , _ciComments = Nothing
    , _ciFieldDelimiter = Nothing
    }


-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ . Type: String Default: @"@  Ancestors: @CSV@
ciQuoteCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteCharacter = lens _ciQuoteCharacter (\ s a -> s{_ciQuoteCharacter = a})

-- | A single character used to separate individual records in the input. Instead of the default value, you can specify an arbitrary delimiter.
ciRecordDelimiter :: Lens' CSVInput (Maybe Text)
ciRecordDelimiter = lens _ciRecordDelimiter (\ s a -> s{_ciRecordDelimiter = a})

-- | Specifies that CSV field values may contain quoted record delimiters and such records should be allowed. Default value is FALSE. Setting this value to TRUE may lower performance.
ciAllowQuotedRecordDelimiter :: Lens' CSVInput (Maybe Bool)
ciAllowQuotedRecordDelimiter = lens _ciAllowQuotedRecordDelimiter (\ s a -> s{_ciAllowQuotedRecordDelimiter = a})

-- | Describes the first line of input. Valid values are:     * @NONE@ : First line is not a header.     * @IGNORE@ : First line is a header, but you can't use the header values to indicate the column in an expression. You can use column position (such as _1, _2, …) to indicate the column (@SELECT s._1 FROM OBJECT s@ ).     * @Use@ : First line is a header, and you can use the header value to identify a column in an expression (@SELECT "name" FROM OBJECT@ ).
ciFileHeaderInfo :: Lens' CSVInput (Maybe FileHeaderInfo)
ciFileHeaderInfo = lens _ciFileHeaderInfo (\ s a -> s{_ciFileHeaderInfo = a})

-- | A single character used for escaping the quotation mark character inside an already escaped value. For example, the value """ a , b """ is parsed as " a , b ".
ciQuoteEscapeCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteEscapeCharacter = lens _ciQuoteEscapeCharacter (\ s a -> s{_ciQuoteEscapeCharacter = a})

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row. You can specify any character to indicate a comment line.
ciComments :: Lens' CSVInput (Maybe Text)
ciComments = lens _ciComments (\ s a -> s{_ciComments = a})

-- | A single character used to separate individual fields in a record. You can specify an arbitrary delimiter.
ciFieldDelimiter :: Lens' CSVInput (Maybe Text)
ciFieldDelimiter = lens _ciFieldDelimiter (\ s a -> s{_ciFieldDelimiter = a})

instance Hashable CSVInput where

instance NFData CSVInput where

instance ToXML CSVInput where
        toXML CSVInput'{..}
          = mconcat
              ["QuoteCharacter" @= _ciQuoteCharacter,
               "RecordDelimiter" @= _ciRecordDelimiter,
               "AllowQuotedRecordDelimiter" @=
                 _ciAllowQuotedRecordDelimiter,
               "FileHeaderInfo" @= _ciFileHeaderInfo,
               "QuoteEscapeCharacter" @= _ciQuoteEscapeCharacter,
               "Comments" @= _ciComments,
               "FieldDelimiter" @= _ciFieldDelimiter]

-- | Describes how uncompressed comma-separated values (CSV)-formatted results are formatted.
--
--
--
-- /See:/ 'csvOutput' smart constructor.
data CSVOutput = CSVOutput'
  { _coQuoteCharacter       :: !(Maybe Text)
  , _coQuoteFields          :: !(Maybe QuoteFields)
  , _coRecordDelimiter      :: !(Maybe Text)
  , _coQuoteEscapeCharacter :: !(Maybe Text)
  , _coFieldDelimiter       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CSVOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coQuoteCharacter' - A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- * 'coQuoteFields' - Indicates whether to use quotation marks around output fields.      * @ALWAYS@ : Always use quotation marks for output fields.     * @ASNEEDED@ : Use quotation marks for output fields when needed.
--
-- * 'coRecordDelimiter' - A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
--
-- * 'coQuoteEscapeCharacter' - The single character used for escaping the quote character inside an already escaped value.
--
-- * 'coFieldDelimiter' - The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
csvOutput
    :: CSVOutput
csvOutput =
  CSVOutput'
    { _coQuoteCharacter = Nothing
    , _coQuoteFields = Nothing
    , _coRecordDelimiter = Nothing
    , _coQuoteEscapeCharacter = Nothing
    , _coFieldDelimiter = Nothing
    }


-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
coQuoteCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteCharacter = lens _coQuoteCharacter (\ s a -> s{_coQuoteCharacter = a})

-- | Indicates whether to use quotation marks around output fields.      * @ALWAYS@ : Always use quotation marks for output fields.     * @ASNEEDED@ : Use quotation marks for output fields when needed.
coQuoteFields :: Lens' CSVOutput (Maybe QuoteFields)
coQuoteFields = lens _coQuoteFields (\ s a -> s{_coQuoteFields = a})

-- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
coRecordDelimiter :: Lens' CSVOutput (Maybe Text)
coRecordDelimiter = lens _coRecordDelimiter (\ s a -> s{_coRecordDelimiter = a})

-- | The single character used for escaping the quote character inside an already escaped value.
coQuoteEscapeCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteEscapeCharacter = lens _coQuoteEscapeCharacter (\ s a -> s{_coQuoteEscapeCharacter = a})

-- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
coFieldDelimiter :: Lens' CSVOutput (Maybe Text)
coFieldDelimiter = lens _coFieldDelimiter (\ s a -> s{_coFieldDelimiter = a})

instance Hashable CSVOutput where

instance NFData CSVOutput where

instance ToXML CSVOutput where
        toXML CSVOutput'{..}
          = mconcat
              ["QuoteCharacter" @= _coQuoteCharacter,
               "QuoteFields" @= _coQuoteFields,
               "RecordDelimiter" @= _coRecordDelimiter,
               "QuoteEscapeCharacter" @= _coQuoteEscapeCharacter,
               "FieldDelimiter" @= _coFieldDelimiter]

-- | Container for all (if there are any) keys between Prefix and the next occurrence of the string specified by a delimiter. CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix. For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/.
--
--
--
-- /See:/ 'commonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix'
  { _cpPrefix :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommonPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPrefix' - Container for the specified common prefix.
commonPrefix
    :: CommonPrefix
commonPrefix = CommonPrefix' {_cpPrefix = Nothing}


-- | Container for the specified common prefix.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\ s a -> s{_cpPrefix = a})

instance FromXML CommonPrefix where
        parseXML x = CommonPrefix' <$> (x .@? "Prefix")

instance Hashable CommonPrefix where

instance NFData CommonPrefix where

-- | The container for the completed multipart upload details.
--
--
--
-- /See:/ 'completedMultipartUpload' smart constructor.
newtype CompletedMultipartUpload = CompletedMultipartUpload'
  { _cmuParts :: Maybe (List1 CompletedPart)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompletedMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuParts' - Array of CompletedPart data types.
completedMultipartUpload
    :: CompletedMultipartUpload
completedMultipartUpload = CompletedMultipartUpload' {_cmuParts = Nothing}


-- | Array of CompletedPart data types.
cmuParts :: Lens' CompletedMultipartUpload (Maybe (NonEmpty CompletedPart))
cmuParts = lens _cmuParts (\ s a -> s{_cmuParts = a}) . mapping _List1

instance Hashable CompletedMultipartUpload where

instance NFData CompletedMultipartUpload where

instance ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload'{..}
          = mconcat [toXML (toXMLList "Part" <$> _cmuParts)]

-- | Details of the parts that were uploaded.
--
--
--
-- /See:/ 'completedPart' smart constructor.
data CompletedPart = CompletedPart'
  { _cpPartNumber :: !Int
  , _cpETag       :: !ETag
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompletedPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPartNumber' - Part number that identifies the part. This is a positive integer between 1 and 10,000.
--
-- * 'cpETag' - Entity tag returned when the part was uploaded.
completedPart
    :: Int -- ^ 'cpPartNumber'
    -> ETag -- ^ 'cpETag'
    -> CompletedPart
completedPart pPartNumber_ pETag_ =
  CompletedPart' {_cpPartNumber = pPartNumber_, _cpETag = pETag_}


-- | Part number that identifies the part. This is a positive integer between 1 and 10,000.
cpPartNumber :: Lens' CompletedPart Int
cpPartNumber = lens _cpPartNumber (\ s a -> s{_cpPartNumber = a})

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart ETag
cpETag = lens _cpETag (\ s a -> s{_cpETag = a})

instance Hashable CompletedPart where

instance NFData CompletedPart where

instance ToXML CompletedPart where
        toXML CompletedPart'{..}
          = mconcat
              ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cKeyPrefixEquals             :: !(Maybe Text)
  , _cHTTPErrorCodeReturnedEquals :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cKeyPrefixEquals' - The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
--
-- * 'cHTTPErrorCodeReturnedEquals' - The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
condition
    :: Condition
condition =
  Condition'
    {_cKeyPrefixEquals = Nothing, _cHTTPErrorCodeReturnedEquals = Nothing}


-- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals = lens _cKeyPrefixEquals (\ s a -> s{_cKeyPrefixEquals = a})

-- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
cHTTPErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHTTPErrorCodeReturnedEquals = lens _cHTTPErrorCodeReturnedEquals (\ s a -> s{_cHTTPErrorCodeReturnedEquals = a})

instance FromXML Condition where
        parseXML x
          = Condition' <$>
              (x .@? "KeyPrefixEquals") <*>
                (x .@? "HttpErrorCodeReturnedEquals")

instance Hashable Condition where

instance NFData Condition where

instance ToXML Condition where
        toXML Condition'{..}
          = mconcat
              ["KeyPrefixEquals" @= _cKeyPrefixEquals,
               "HttpErrorCodeReturnedEquals" @=
                 _cHTTPErrorCodeReturnedEquals]

-- |
--
--
--
-- /See:/ 'continuationEvent' smart constructor.
data ContinuationEvent =
  ContinuationEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinuationEvent' with the minimum fields required to make a request.
--
continuationEvent
    :: ContinuationEvent
continuationEvent = ContinuationEvent'


instance FromXML ContinuationEvent where
        parseXML = const (pure ContinuationEvent')

instance Hashable ContinuationEvent where

instance NFData ContinuationEvent where

-- | Container for all response elements.
--
--
--
-- /See:/ 'copyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { _corETag         :: !(Maybe ETag)
  , _corLastModified :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyObjectResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corETag' - Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
--
-- * 'corLastModified' - Returns the date that the object was last modified.
copyObjectResult
    :: CopyObjectResult
copyObjectResult =
  CopyObjectResult' {_corETag = Nothing, _corLastModified = Nothing}


-- | Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
corETag :: Lens' CopyObjectResult (Maybe ETag)
corETag = lens _corETag (\ s a -> s{_corETag = a})

-- | Returns the date that the object was last modified.
corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\ s a -> s{_corLastModified = a}) . mapping _Time

instance FromXML CopyObjectResult where
        parseXML x
          = CopyObjectResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyObjectResult where

instance NFData CopyObjectResult where

-- | Container for all response elements.
--
--
--
-- /See:/ 'copyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { _cprETag         :: !(Maybe ETag)
  , _cprLastModified :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyPartResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprETag' - Entity tag of the object.
--
-- * 'cprLastModified' - Date and time at which the object was uploaded.
copyPartResult
    :: CopyPartResult
copyPartResult =
  CopyPartResult' {_cprETag = Nothing, _cprLastModified = Nothing}


-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\ s a -> s{_cprETag = a})

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\ s a -> s{_cprLastModified = a}) . mapping _Time

instance FromXML CopyPartResult where
        parseXML x
          = CopyPartResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyPartResult where

instance NFData CopyPartResult where

-- | The configuration information for the bucket.
--
--
--
-- /See:/ 'createBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
  { _cbcLocationConstraint :: Maybe LocationConstraint
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBucketConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbcLocationConstraint' - Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
createBucketConfiguration
    :: CreateBucketConfiguration
createBucketConfiguration =
  CreateBucketConfiguration' {_cbcLocationConstraint = Nothing}


-- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe LocationConstraint)
cbcLocationConstraint = lens _cbcLocationConstraint (\ s a -> s{_cbcLocationConstraint = a})

instance Hashable CreateBucketConfiguration where

instance NFData CreateBucketConfiguration where

instance ToXML CreateBucketConfiguration where
        toXML CreateBucketConfiguration'{..}
          = mconcat
              ["LocationConstraint" @= _cbcLocationConstraint]

-- | The container element for specifying the default Object Lock retention settings for new objects placed in the specified bucket.
--
--
--
-- /See:/ 'defaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { _drDays  :: !(Maybe Int)
  , _drMode  :: !(Maybe ObjectLockRetentionMode)
  , _drYears :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefaultRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drDays' - The number of days that you want to specify for the default retention period.
--
-- * 'drMode' - The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
--
-- * 'drYears' - The number of years that you want to specify for the default retention period.
defaultRetention
    :: DefaultRetention
defaultRetention =
  DefaultRetention' {_drDays = Nothing, _drMode = Nothing, _drYears = Nothing}


-- | The number of days that you want to specify for the default retention period.
drDays :: Lens' DefaultRetention (Maybe Int)
drDays = lens _drDays (\ s a -> s{_drDays = a})

-- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
drMode :: Lens' DefaultRetention (Maybe ObjectLockRetentionMode)
drMode = lens _drMode (\ s a -> s{_drMode = a})

-- | The number of years that you want to specify for the default retention period.
drYears :: Lens' DefaultRetention (Maybe Int)
drYears = lens _drYears (\ s a -> s{_drYears = a})

instance FromXML DefaultRetention where
        parseXML x
          = DefaultRetention' <$>
              (x .@? "Days") <*> (x .@? "Mode") <*> (x .@? "Years")

instance Hashable DefaultRetention where

instance NFData DefaultRetention where

instance ToXML DefaultRetention where
        toXML DefaultRetention'{..}
          = mconcat
              ["Days" @= _drDays, "Mode" @= _drMode,
               "Years" @= _drYears]

-- | Container for the objects to delete.
--
--
--
-- /See:/ 'delete'' smart constructor.
data Delete = Delete'
  { _dQuiet   :: !(Maybe Bool)
  , _dObjects :: ![ObjectIdentifier]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dQuiet' - Element to enable quiet mode for the request. When you add this element, you must set its value to true.
--
-- * 'dObjects' - The objects to delete.
delete'
    :: Delete
delete' = Delete' {_dQuiet = Nothing, _dObjects = mempty}


-- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\ s a -> s{_dQuiet = a})

-- | The objects to delete.
dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\ s a -> s{_dObjects = a}) . _Coerce

instance Hashable Delete where

instance NFData Delete where

instance ToXML Delete where
        toXML Delete'{..}
          = mconcat
              ["Quiet" @= _dQuiet, toXMLList "Object" _dObjects]

-- | Information about the delete marker.
--
--
--
-- /See:/ 'deleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { _dmeVersionId    :: !(Maybe ObjectVersionId)
  , _dmeIsLatest     :: !(Maybe Bool)
  , _dmeOwner        :: !(Maybe Owner)
  , _dmeKey          :: !(Maybe ObjectKey)
  , _dmeLastModified :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMarkerEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmeVersionId' - Version ID of an object.
--
-- * 'dmeIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'dmeOwner' - The account that created the delete marker.>
--
-- * 'dmeKey' - The object key.
--
-- * 'dmeLastModified' - Date and time the object was last modified.
deleteMarkerEntry
    :: DeleteMarkerEntry
deleteMarkerEntry =
  DeleteMarkerEntry'
    { _dmeVersionId = Nothing
    , _dmeIsLatest = Nothing
    , _dmeOwner = Nothing
    , _dmeKey = Nothing
    , _dmeLastModified = Nothing
    }


-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\ s a -> s{_dmeVersionId = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\ s a -> s{_dmeIsLatest = a})

-- | The account that created the delete marker.>
dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\ s a -> s{_dmeOwner = a})

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\ s a -> s{_dmeKey = a})

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\ s a -> s{_dmeLastModified = a}) . mapping _Time

instance FromXML DeleteMarkerEntry where
        parseXML x
          = DeleteMarkerEntry' <$>
              (x .@? "VersionId") <*> (x .@? "IsLatest") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "LastModified")

instance Hashable DeleteMarkerEntry where

instance NFData DeleteMarkerEntry where

-- | Specifies whether Amazon S3 replicates delete markers. If you specify a @Filter@ in your replication configuration, you must also include a @DeleteMarkerReplication@ element. If your @Filter@ includes a @Tag@ element, the @DeleteMarkerReplication@ @Status@ must be set to Disabled, because Amazon S3 does not support replicating delete markers for tag-based rules. For an example configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-config-min-rule-config Basic Rule Configuration> .
--
--
-- For more information about delete marker replication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/delete-marker-replication.html Basic Rule Configuration> .
--
--
-- /See:/ 'deleteMarkerReplication' smart constructor.
newtype DeleteMarkerReplication = DeleteMarkerReplication'
  { _dmrStatus :: Maybe DeleteMarkerReplicationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMarkerReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrStatus' - Indicates whether to replicate delete markers.
deleteMarkerReplication
    :: DeleteMarkerReplication
deleteMarkerReplication = DeleteMarkerReplication' {_dmrStatus = Nothing}


-- | Indicates whether to replicate delete markers.
dmrStatus :: Lens' DeleteMarkerReplication (Maybe DeleteMarkerReplicationStatus)
dmrStatus = lens _dmrStatus (\ s a -> s{_dmrStatus = a})

instance FromXML DeleteMarkerReplication where
        parseXML x
          = DeleteMarkerReplication' <$> (x .@? "Status")

instance Hashable DeleteMarkerReplication where

instance NFData DeleteMarkerReplication where

instance ToXML DeleteMarkerReplication where
        toXML DeleteMarkerReplication'{..}
          = mconcat ["Status" @= _dmrStatus]

-- | Information about the deleted object.
--
--
--
-- /See:/ 'deletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { _dVersionId             :: !(Maybe ObjectVersionId)
  , _dDeleteMarker          :: !(Maybe Bool)
  , _dDeleteMarkerVersionId :: !(Maybe Text)
  , _dKey                   :: !(Maybe ObjectKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletedObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionId' - The version ID of the deleted object.
--
-- * 'dDeleteMarker' - Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
--
-- * 'dDeleteMarkerVersionId' - The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
--
-- * 'dKey' - The name of the deleted object.
deletedObject
    :: DeletedObject
deletedObject =
  DeletedObject'
    { _dVersionId = Nothing
    , _dDeleteMarker = Nothing
    , _dDeleteMarkerVersionId = Nothing
    , _dKey = Nothing
    }


-- | The version ID of the deleted object.
dVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dVersionId = lens _dVersionId (\ s a -> s{_dVersionId = a})

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
dDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dDeleteMarker = lens _dDeleteMarker (\ s a -> s{_dDeleteMarker = a})

-- | The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
dDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dDeleteMarkerVersionId = lens _dDeleteMarkerVersionId (\ s a -> s{_dDeleteMarkerVersionId = a})

-- | The name of the deleted object.
dKey :: Lens' DeletedObject (Maybe ObjectKey)
dKey = lens _dKey (\ s a -> s{_dKey = a})

instance FromXML DeletedObject where
        parseXML x
          = DeletedObject' <$>
              (x .@? "VersionId") <*> (x .@? "DeleteMarker") <*>
                (x .@? "DeleteMarkerVersionId")
                <*> (x .@? "Key")

instance Hashable DeletedObject where

instance NFData DeletedObject where

-- | Specifies information about where to publish analysis or configuration results for an Amazon S3 bucket and S3 Replication Time Control (S3 RTC).
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dMetrics                  :: !(Maybe Metrics)
  , _dAccessControlTranslation :: !(Maybe AccessControlTranslation)
  , _dAccount                  :: !(Maybe Text)
  , _dStorageClass             :: !(Maybe StorageClass)
  , _dEncryptionConfiguration  :: !(Maybe EncryptionConfiguration)
  , _dReplicationTime          :: !(Maybe ReplicationTime)
  , _dBucket                   :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMetrics' - A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- * 'dAccessControlTranslation' - Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
--
-- * 'dAccount' - Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'dStorageClass' - The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.  For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
--
-- * 'dEncryptionConfiguration' - A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
--
-- * 'dReplicationTime' - A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- * 'dBucket' - The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
destination
    :: BucketName -- ^ 'dBucket'
    -> Destination
destination pBucket_ =
  Destination'
    { _dMetrics = Nothing
    , _dAccessControlTranslation = Nothing
    , _dAccount = Nothing
    , _dStorageClass = Nothing
    , _dEncryptionConfiguration = Nothing
    , _dReplicationTime = Nothing
    , _dBucket = pBucket_
    }


-- | A container specifying replication metrics-related settings enabling replication metrics and events.
dMetrics :: Lens' Destination (Maybe Metrics)
dMetrics = lens _dMetrics (\ s a -> s{_dMetrics = a})

-- | Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
dAccessControlTranslation :: Lens' Destination (Maybe AccessControlTranslation)
dAccessControlTranslation = lens _dAccessControlTranslation (\ s a -> s{_dAccessControlTranslation = a})

-- | Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
dAccount :: Lens' Destination (Maybe Text)
dAccount = lens _dAccount (\ s a -> s{_dAccount = a})

-- | The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.  For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
dStorageClass :: Lens' Destination (Maybe StorageClass)
dStorageClass = lens _dStorageClass (\ s a -> s{_dStorageClass = a})

-- | A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
dEncryptionConfiguration :: Lens' Destination (Maybe EncryptionConfiguration)
dEncryptionConfiguration = lens _dEncryptionConfiguration (\ s a -> s{_dEncryptionConfiguration = a})

-- | A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
dReplicationTime :: Lens' Destination (Maybe ReplicationTime)
dReplicationTime = lens _dReplicationTime (\ s a -> s{_dReplicationTime = a})

-- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
dBucket :: Lens' Destination BucketName
dBucket = lens _dBucket (\ s a -> s{_dBucket = a})

instance FromXML Destination where
        parseXML x
          = Destination' <$>
              (x .@? "Metrics") <*>
                (x .@? "AccessControlTranslation")
                <*> (x .@? "Account")
                <*> (x .@? "StorageClass")
                <*> (x .@? "EncryptionConfiguration")
                <*> (x .@? "ReplicationTime")
                <*> (x .@ "Bucket")

instance Hashable Destination where

instance NFData Destination where

instance ToXML Destination where
        toXML Destination'{..}
          = mconcat
              ["Metrics" @= _dMetrics,
               "AccessControlTranslation" @=
                 _dAccessControlTranslation,
               "Account" @= _dAccount,
               "StorageClass" @= _dStorageClass,
               "EncryptionConfiguration" @=
                 _dEncryptionConfiguration,
               "ReplicationTime" @= _dReplicationTime,
               "Bucket" @= _dBucket]

-- | Contains the type of server-side encryption used.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eKMSKeyId       :: !(Maybe (Sensitive Text))
  , _eKMSContext     :: !(Maybe Text)
  , _eEncryptionType :: !ServerSideEncryption
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eKMSKeyId' - If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'eKMSContext' - If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
--
-- * 'eEncryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
encryption
    :: ServerSideEncryption -- ^ 'eEncryptionType'
    -> Encryption
encryption pEncryptionType_ =
  Encryption'
    { _eKMSKeyId = Nothing
    , _eKMSContext = Nothing
    , _eEncryptionType = pEncryptionType_
    }


-- | If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
eKMSKeyId :: Lens' Encryption (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a}) . mapping _Sensitive

-- | If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
eKMSContext :: Lens' Encryption (Maybe Text)
eKMSContext = lens _eKMSContext (\ s a -> s{_eKMSContext = a})

-- | The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
eEncryptionType :: Lens' Encryption ServerSideEncryption
eEncryptionType = lens _eEncryptionType (\ s a -> s{_eEncryptionType = a})

instance Hashable Encryption where

instance NFData Encryption where

instance ToXML Encryption where
        toXML Encryption'{..}
          = mconcat
              ["KMSKeyId" @= _eKMSKeyId,
               "KMSContext" @= _eKMSContext,
               "EncryptionType" @= _eEncryptionType]

-- | Specifies encryption-related information for an Amazon S3 bucket that is a destination for replicated objects.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
newtype EncryptionConfiguration = EncryptionConfiguration'
  { _ecReplicaKMSKeyId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecReplicaKMSKeyId' - Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
encryptionConfiguration
    :: EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration' {_ecReplicaKMSKeyId = Nothing}


-- | Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
ecReplicaKMSKeyId :: Lens' EncryptionConfiguration (Maybe Text)
ecReplicaKMSKeyId = lens _ecReplicaKMSKeyId (\ s a -> s{_ecReplicaKMSKeyId = a})

instance FromXML EncryptionConfiguration where
        parseXML x
          = EncryptionConfiguration' <$>
              (x .@? "ReplicaKmsKeyID")

instance Hashable EncryptionConfiguration where

instance NFData EncryptionConfiguration where

instance ToXML EncryptionConfiguration where
        toXML EncryptionConfiguration'{..}
          = mconcat ["ReplicaKmsKeyID" @= _ecReplicaKMSKeyId]

-- | A message that indicates the request is complete and no more messages will be sent. You should not assume that the request is complete until the client receives an @EndEvent@ .
--
--
--
-- /See:/ 'endEvent' smart constructor.
data EndEvent =
  EndEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndEvent' with the minimum fields required to make a request.
--
endEvent
    :: EndEvent
endEvent = EndEvent'


instance FromXML EndEvent where
        parseXML = const (pure EndEvent')

instance Hashable EndEvent where

instance NFData EndEvent where

-- | The error information.
--
--
--
-- /See:/ 'errorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument'
  { _edKey :: ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edKey' - The object key name to use when a 4XX class error occurs.
errorDocument
    :: ObjectKey -- ^ 'edKey'
    -> ErrorDocument
errorDocument pKey_ = ErrorDocument' {_edKey = pKey_}


-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\ s a -> s{_edKey = a})

instance FromXML ErrorDocument where
        parseXML x = ErrorDocument' <$> (x .@ "Key")

instance Hashable ErrorDocument where

instance NFData ErrorDocument where

instance ToXML ErrorDocument where
        toXML ErrorDocument'{..} = mconcat ["Key" @= _edKey]

-- | Optional configuration to replicate existing source bucket objects. For more information, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects> in the /Amazon S3 Developer Guide/ .
--
--
--
-- /See:/ 'existingObjectReplication' smart constructor.
newtype ExistingObjectReplication = ExistingObjectReplication'
  { _eorStatus :: ExistingObjectReplicationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExistingObjectReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eorStatus' -
existingObjectReplication
    :: ExistingObjectReplicationStatus -- ^ 'eorStatus'
    -> ExistingObjectReplication
existingObjectReplication pStatus_ =
  ExistingObjectReplication' {_eorStatus = pStatus_}


-- |
eorStatus :: Lens' ExistingObjectReplication ExistingObjectReplicationStatus
eorStatus = lens _eorStatus (\ s a -> s{_eorStatus = a})

instance FromXML ExistingObjectReplication where
        parseXML x
          = ExistingObjectReplication' <$> (x .@ "Status")

instance Hashable ExistingObjectReplication where

instance NFData ExistingObjectReplication where

instance ToXML ExistingObjectReplication where
        toXML ExistingObjectReplication'{..}
          = mconcat ["Status" @= _eorStatus]

-- | Specifies the Amazon S3 object key name to filter on and whether to filter on the suffix or prefix of the key name.
--
--
--
-- /See:/ 'filterRule' smart constructor.
data FilterRule = FilterRule'
  { _frValue :: !(Maybe Text)
  , _frName  :: !(Maybe FilterRuleName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frValue' - The value that the filter searches for in object key names.
--
-- * 'frName' - The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
filterRule
    :: FilterRule
filterRule = FilterRule' {_frValue = Nothing, _frName = Nothing}


-- | The value that the filter searches for in object key names.
frValue :: Lens' FilterRule (Maybe Text)
frValue = lens _frValue (\ s a -> s{_frValue = a})

-- | The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
frName :: Lens' FilterRule (Maybe FilterRuleName)
frName = lens _frName (\ s a -> s{_frName = a})

instance FromXML FilterRule where
        parseXML x
          = FilterRule' <$> (x .@? "Value") <*> (x .@? "Name")

instance Hashable FilterRule where

instance NFData FilterRule where

instance ToXML FilterRule where
        toXML FilterRule'{..}
          = mconcat ["Value" @= _frValue, "Name" @= _frName]

-- | Container for S3 Glacier job parameters.
--
--
--
-- /See:/ 'glacierJobParameters' smart constructor.
newtype GlacierJobParameters = GlacierJobParameters'
  { _gjpTier :: Tier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlacierJobParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjpTier' - Retrieval tier at which the restore will be processed.
glacierJobParameters
    :: Tier -- ^ 'gjpTier'
    -> GlacierJobParameters
glacierJobParameters pTier_ = GlacierJobParameters' {_gjpTier = pTier_}


-- | Retrieval tier at which the restore will be processed.
gjpTier :: Lens' GlacierJobParameters Tier
gjpTier = lens _gjpTier (\ s a -> s{_gjpTier = a})

instance Hashable GlacierJobParameters where

instance NFData GlacierJobParameters where

instance ToXML GlacierJobParameters where
        toXML GlacierJobParameters'{..}
          = mconcat ["Tier" @= _gjpTier]

-- | Container for grant information.
--
--
--
-- /See:/ 'grant' smart constructor.
data Grant = Grant'
  { _gPermission :: !(Maybe Permission)
  , _gGrantee    :: !(Maybe Grantee)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPermission' - Specifies the permission given to the grantee.
--
-- * 'gGrantee' - The person being granted permissions.
grant
    :: Grant
grant = Grant' {_gPermission = Nothing, _gGrantee = Nothing}


-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\ s a -> s{_gPermission = a})

-- | The person being granted permissions.
gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\ s a -> s{_gGrantee = a})

instance FromXML Grant where
        parseXML x
          = Grant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance Hashable Grant where

instance NFData Grant where

instance ToXML Grant where
        toXML Grant'{..}
          = mconcat
              ["Permission" @= _gPermission,
               "Grantee" @= _gGrantee]

-- | Container for the person being granted permissions.
--
--
--
-- /See:/ 'grantee' smart constructor.
data Grantee = Grantee'
  { _gURI          :: !(Maybe Text)
  , _gEmailAddress :: !(Maybe Text)
  , _gDisplayName  :: !(Maybe Text)
  , _gId           :: !(Maybe Text)
  , _gType         :: !Type
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Grantee' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gURI' - URI of the grantee group.
--
-- * 'gEmailAddress' - Email address of the grantee.
--
-- * 'gDisplayName' - Screen name of the grantee.
--
-- * 'gId' - The canonical user ID of the grantee.
--
-- * 'gType' - Type of grantee
grantee
    :: Type -- ^ 'gType'
    -> Grantee
grantee pType_ =
  Grantee'
    { _gURI = Nothing
    , _gEmailAddress = Nothing
    , _gDisplayName = Nothing
    , _gId = Nothing
    , _gType = pType_
    }


-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\ s a -> s{_gURI = a})

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\ s a -> s{_gEmailAddress = a})

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\ s a -> s{_gDisplayName = a})

-- | The canonical user ID of the grantee.
gId :: Lens' Grantee (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a})

-- | Type of grantee
gType :: Lens' Grantee Type
gType = lens _gType (\ s a -> s{_gType = a})

instance FromXML Grantee where
        parseXML x
          = Grantee' <$>
              (x .@? "URI") <*> (x .@? "EmailAddress") <*>
                (x .@? "DisplayName")
                <*> (x .@? "ID")
                <*> (x .@ "xsi:type")

instance Hashable Grantee where

instance NFData Grantee where

instance ToXML Grantee where
        toXML Grantee'{..}
          = mconcat
              ["URI" @= _gURI, "EmailAddress" @= _gEmailAddress,
               "DisplayName" @= _gDisplayName, "ID" @= _gId,
               "xsi:type" @@= _gType]

-- | Container for the @Suffix@ element.
--
--
--
-- /See:/ 'indexDocument' smart constructor.
newtype IndexDocument = IndexDocument'
  { _idSuffix :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSuffix' - A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
indexDocument
    :: Text -- ^ 'idSuffix'
    -> IndexDocument
indexDocument pSuffix_ = IndexDocument' {_idSuffix = pSuffix_}


-- | A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\ s a -> s{_idSuffix = a})

instance FromXML IndexDocument where
        parseXML x = IndexDocument' <$> (x .@ "Suffix")

instance Hashable IndexDocument where

instance NFData IndexDocument where

instance ToXML IndexDocument where
        toXML IndexDocument'{..}
          = mconcat ["Suffix" @= _idSuffix]

-- | Container element that identifies who initiated the multipart upload.
--
--
--
-- /See:/ 'initiator' smart constructor.
data Initiator = Initiator'
  { _iDisplayName :: !(Maybe Text)
  , _iId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Initiator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iDisplayName' - Name of the Principal.
--
-- * 'iId' - If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
initiator
    :: Initiator
initiator = Initiator' {_iDisplayName = Nothing, _iId = Nothing}


-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a})

-- | If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
iId :: Lens' Initiator (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a})

instance FromXML Initiator where
        parseXML x
          = Initiator' <$>
              (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Initiator where

instance NFData Initiator where

-- | Describes the serialization format of the object.
--
--
--
-- /See:/ 'inputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { _isJSON            :: !(Maybe JSONInput)
  , _isCSV             :: !(Maybe CSVInput)
  , _isParquet         :: !(Maybe ParquetInput)
  , _isCompressionType :: !(Maybe CompressionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isJSON' - Specifies JSON as object's input serialization format.
--
-- * 'isCSV' - Describes the serialization of a CSV-encoded object.
--
-- * 'isParquet' - Specifies Parquet as object's input serialization format.
--
-- * 'isCompressionType' - Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
inputSerialization
    :: InputSerialization
inputSerialization =
  InputSerialization'
    { _isJSON = Nothing
    , _isCSV = Nothing
    , _isParquet = Nothing
    , _isCompressionType = Nothing
    }


-- | Specifies JSON as object's input serialization format.
isJSON :: Lens' InputSerialization (Maybe JSONInput)
isJSON = lens _isJSON (\ s a -> s{_isJSON = a})

-- | Describes the serialization of a CSV-encoded object.
isCSV :: Lens' InputSerialization (Maybe CSVInput)
isCSV = lens _isCSV (\ s a -> s{_isCSV = a})

-- | Specifies Parquet as object's input serialization format.
isParquet :: Lens' InputSerialization (Maybe ParquetInput)
isParquet = lens _isParquet (\ s a -> s{_isParquet = a})

-- | Specifies object's compression format. Valid values: NONE, GZIP, BZIP2. Default Value: NONE.
isCompressionType :: Lens' InputSerialization (Maybe CompressionType)
isCompressionType = lens _isCompressionType (\ s a -> s{_isCompressionType = a})

instance Hashable InputSerialization where

instance NFData InputSerialization where

instance ToXML InputSerialization where
        toXML InputSerialization'{..}
          = mconcat
              ["JSON" @= _isJSON, "CSV" @= _isCSV,
               "Parquet" @= _isParquet,
               "CompressionType" @= _isCompressionType]

-- | A container for specifying S3 Intelligent-Tiering filters. The filters determine the subset of objects to which the rule applies.
--
--
--
-- /See:/ 'intelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
  { _itaoPrefix :: !(Maybe Text)
  , _itaoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntelligentTieringAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itaoPrefix' - An object key name prefix that identifies the subset of objects to which the configuration applies.
--
-- * 'itaoTags' - All of these tags must exist in the object's tag set in order for the configuration to apply.
intelligentTieringAndOperator
    :: IntelligentTieringAndOperator
intelligentTieringAndOperator =
  IntelligentTieringAndOperator' {_itaoPrefix = Nothing, _itaoTags = Nothing}


-- | An object key name prefix that identifies the subset of objects to which the configuration applies.
itaoPrefix :: Lens' IntelligentTieringAndOperator (Maybe Text)
itaoPrefix = lens _itaoPrefix (\ s a -> s{_itaoPrefix = a})

-- | All of these tags must exist in the object's tag set in order for the configuration to apply.
itaoTags :: Lens' IntelligentTieringAndOperator [Tag]
itaoTags = lens _itaoTags (\ s a -> s{_itaoTags = a}) . _Default . _Coerce

instance FromXML IntelligentTieringAndOperator where
        parseXML x
          = IntelligentTieringAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable IntelligentTieringAndOperator where

instance NFData IntelligentTieringAndOperator where

instance ToXML IntelligentTieringAndOperator where
        toXML IntelligentTieringAndOperator'{..}
          = mconcat
              ["Prefix" @= _itaoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _itaoTags)]

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3 bucket.
--
--
-- For information about the S3 Intelligent-Tiering storage class, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
--
-- /See:/ 'intelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { _itcFilter   :: !(Maybe IntelligentTieringFilter)
  , _itcId       :: !Text
  , _itcStatus   :: !IntelligentTieringStatus
  , _itcTierings :: ![Tiering]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcFilter' - Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
--
-- * 'itcId' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- * 'itcStatus' - Specifies the status of the configuration.
--
-- * 'itcTierings' - Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
intelligentTieringConfiguration
    :: Text -- ^ 'itcId'
    -> IntelligentTieringStatus -- ^ 'itcStatus'
    -> IntelligentTieringConfiguration
intelligentTieringConfiguration pId_ pStatus_ =
  IntelligentTieringConfiguration'
    { _itcFilter = Nothing
    , _itcId = pId_
    , _itcStatus = pStatus_
    , _itcTierings = mempty
    }


-- | Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
itcFilter :: Lens' IntelligentTieringConfiguration (Maybe IntelligentTieringFilter)
itcFilter = lens _itcFilter (\ s a -> s{_itcFilter = a})

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
itcId :: Lens' IntelligentTieringConfiguration Text
itcId = lens _itcId (\ s a -> s{_itcId = a})

-- | Specifies the status of the configuration.
itcStatus :: Lens' IntelligentTieringConfiguration IntelligentTieringStatus
itcStatus = lens _itcStatus (\ s a -> s{_itcStatus = a})

-- | Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
itcTierings :: Lens' IntelligentTieringConfiguration [Tiering]
itcTierings = lens _itcTierings (\ s a -> s{_itcTierings = a}) . _Coerce

instance FromXML IntelligentTieringConfiguration
         where
        parseXML x
          = IntelligentTieringConfiguration' <$>
              (x .@? "Filter") <*> (x .@ "Id") <*> (x .@ "Status")
                <*> (parseXMLList "Tiering" x)

instance Hashable IntelligentTieringConfiguration
         where

instance NFData IntelligentTieringConfiguration where

instance ToXML IntelligentTieringConfiguration where
        toXML IntelligentTieringConfiguration'{..}
          = mconcat
              ["Filter" @= _itcFilter, "Id" @= _itcId,
               "Status" @= _itcStatus,
               toXMLList "Tiering" _itcTierings]

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering configuration applies to.
--
--
--
-- /See:/ 'intelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { _itfTag    :: !(Maybe Tag)
  , _itfPrefix :: !(Maybe Text)
  , _itfAnd    :: !(Maybe IntelligentTieringAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntelligentTieringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itfTag' - Undocumented member.
--
-- * 'itfPrefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- * 'itfAnd' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
intelligentTieringFilter
    :: IntelligentTieringFilter
intelligentTieringFilter =
  IntelligentTieringFilter'
    {_itfTag = Nothing, _itfPrefix = Nothing, _itfAnd = Nothing}


-- | Undocumented member.
itfTag :: Lens' IntelligentTieringFilter (Maybe Tag)
itfTag = lens _itfTag (\ s a -> s{_itfTag = a})

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
itfPrefix :: Lens' IntelligentTieringFilter (Maybe Text)
itfPrefix = lens _itfPrefix (\ s a -> s{_itfPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
itfAnd :: Lens' IntelligentTieringFilter (Maybe IntelligentTieringAndOperator)
itfAnd = lens _itfAnd (\ s a -> s{_itfAnd = a})

instance FromXML IntelligentTieringFilter where
        parseXML x
          = IntelligentTieringFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable IntelligentTieringFilter where

instance NFData IntelligentTieringFilter where

instance ToXML IntelligentTieringFilter where
        toXML IntelligentTieringFilter'{..}
          = mconcat
              ["Tag" @= _itfTag, "Prefix" @= _itfPrefix,
               "And" @= _itfAnd]

-- | Specifies the inventory configuration for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETInventoryConfig.html GET Bucket inventory> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'inventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { _icOptionalFields         :: !(Maybe [InventoryOptionalField])
  , _icFilter                 :: !(Maybe InventoryFilter)
  , _icDestination            :: !InventoryDestination
  , _icIsEnabled              :: !Bool
  , _icId                     :: !Text
  , _icIncludedObjectVersions :: !InventoryIncludedObjectVersions
  , _icSchedule               :: !InventorySchedule
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icOptionalFields' - Contains the optional fields that are included in the inventory results.
--
-- * 'icFilter' - Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- * 'icDestination' - Contains information about where to publish the inventory results.
--
-- * 'icIsEnabled' - Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- * 'icId' - The ID used to identify the inventory configuration.
--
-- * 'icIncludedObjectVersions' - Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- * 'icSchedule' - Specifies the schedule for generating inventory results.
inventoryConfiguration
    :: InventoryDestination -- ^ 'icDestination'
    -> Bool -- ^ 'icIsEnabled'
    -> Text -- ^ 'icId'
    -> InventoryIncludedObjectVersions -- ^ 'icIncludedObjectVersions'
    -> InventorySchedule -- ^ 'icSchedule'
    -> InventoryConfiguration
inventoryConfiguration pDestination_ pIsEnabled_ pId_ pIncludedObjectVersions_ pSchedule_ =
  InventoryConfiguration'
    { _icOptionalFields = Nothing
    , _icFilter = Nothing
    , _icDestination = pDestination_
    , _icIsEnabled = pIsEnabled_
    , _icId = pId_
    , _icIncludedObjectVersions = pIncludedObjectVersions_
    , _icSchedule = pSchedule_
    }


-- | Contains the optional fields that are included in the inventory results.
icOptionalFields :: Lens' InventoryConfiguration [InventoryOptionalField]
icOptionalFields = lens _icOptionalFields (\ s a -> s{_icOptionalFields = a}) . _Default . _Coerce

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
icFilter :: Lens' InventoryConfiguration (Maybe InventoryFilter)
icFilter = lens _icFilter (\ s a -> s{_icFilter = a})

-- | Contains information about where to publish the inventory results.
icDestination :: Lens' InventoryConfiguration InventoryDestination
icDestination = lens _icDestination (\ s a -> s{_icDestination = a})

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
icIsEnabled :: Lens' InventoryConfiguration Bool
icIsEnabled = lens _icIsEnabled (\ s a -> s{_icIsEnabled = a})

-- | The ID used to identify the inventory configuration.
icId :: Lens' InventoryConfiguration Text
icId = lens _icId (\ s a -> s{_icId = a})

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
icIncludedObjectVersions :: Lens' InventoryConfiguration InventoryIncludedObjectVersions
icIncludedObjectVersions = lens _icIncludedObjectVersions (\ s a -> s{_icIncludedObjectVersions = a})

-- | Specifies the schedule for generating inventory results.
icSchedule :: Lens' InventoryConfiguration InventorySchedule
icSchedule = lens _icSchedule (\ s a -> s{_icSchedule = a})

instance FromXML InventoryConfiguration where
        parseXML x
          = InventoryConfiguration' <$>
              (x .@? "OptionalFields" .!@ mempty >>=
                 may (parseXMLList "Field"))
                <*> (x .@? "Filter")
                <*> (x .@ "Destination")
                <*> (x .@ "IsEnabled")
                <*> (x .@ "Id")
                <*> (x .@ "IncludedObjectVersions")
                <*> (x .@ "Schedule")

instance Hashable InventoryConfiguration where

instance NFData InventoryConfiguration where

instance ToXML InventoryConfiguration where
        toXML InventoryConfiguration'{..}
          = mconcat
              ["OptionalFields" @=
                 toXML (toXMLList "Field" <$> _icOptionalFields),
               "Filter" @= _icFilter,
               "Destination" @= _icDestination,
               "IsEnabled" @= _icIsEnabled, "Id" @= _icId,
               "IncludedObjectVersions" @=
                 _icIncludedObjectVersions,
               "Schedule" @= _icSchedule]

-- | Specifies the inventory configuration for an Amazon S3 bucket.
--
--
--
-- /See:/ 'inventoryDestination' smart constructor.
newtype InventoryDestination = InventoryDestination'
  { _idS3BucketDestination :: InventoryS3BucketDestination
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idS3BucketDestination' - Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
inventoryDestination
    :: InventoryS3BucketDestination -- ^ 'idS3BucketDestination'
    -> InventoryDestination
inventoryDestination pS3BucketDestination_ =
  InventoryDestination' {_idS3BucketDestination = pS3BucketDestination_}


-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
idS3BucketDestination :: Lens' InventoryDestination InventoryS3BucketDestination
idS3BucketDestination = lens _idS3BucketDestination (\ s a -> s{_idS3BucketDestination = a})

instance FromXML InventoryDestination where
        parseXML x
          = InventoryDestination' <$>
              (x .@ "S3BucketDestination")

instance Hashable InventoryDestination where

instance NFData InventoryDestination where

instance ToXML InventoryDestination where
        toXML InventoryDestination'{..}
          = mconcat
              ["S3BucketDestination" @= _idS3BucketDestination]

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
--
--
-- /See:/ 'inventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { _ieSSES3  :: !(Maybe SSES3)
  , _ieSSEKMS :: !(Maybe SSEKMS)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieSSES3' - Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- * 'ieSSEKMS' - Specifies the use of SSE-KMS to encrypt delivered inventory reports.
inventoryEncryption
    :: InventoryEncryption
inventoryEncryption =
  InventoryEncryption' {_ieSSES3 = Nothing, _ieSSEKMS = Nothing}


-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
ieSSES3 :: Lens' InventoryEncryption (Maybe SSES3)
ieSSES3 = lens _ieSSES3 (\ s a -> s{_ieSSES3 = a})

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
ieSSEKMS :: Lens' InventoryEncryption (Maybe SSEKMS)
ieSSEKMS = lens _ieSSEKMS (\ s a -> s{_ieSSEKMS = a})

instance FromXML InventoryEncryption where
        parseXML x
          = InventoryEncryption' <$>
              (x .@? "SSE-S3") <*> (x .@? "SSE-KMS")

instance Hashable InventoryEncryption where

instance NFData InventoryEncryption where

instance ToXML InventoryEncryption where
        toXML InventoryEncryption'{..}
          = mconcat
              ["SSE-S3" @= _ieSSES3, "SSE-KMS" @= _ieSSEKMS]

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
--
--
-- /See:/ 'inventoryFilter' smart constructor.
newtype InventoryFilter = InventoryFilter'
  { _ifPrefix :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifPrefix' - The prefix that an object must have to be included in the inventory results.
inventoryFilter
    :: Text -- ^ 'ifPrefix'
    -> InventoryFilter
inventoryFilter pPrefix_ = InventoryFilter' {_ifPrefix = pPrefix_}


-- | The prefix that an object must have to be included in the inventory results.
ifPrefix :: Lens' InventoryFilter Text
ifPrefix = lens _ifPrefix (\ s a -> s{_ifPrefix = a})

instance FromXML InventoryFilter where
        parseXML x = InventoryFilter' <$> (x .@ "Prefix")

instance Hashable InventoryFilter where

instance NFData InventoryFilter where

instance ToXML InventoryFilter where
        toXML InventoryFilter'{..}
          = mconcat ["Prefix" @= _ifPrefix]

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
--
--
-- /See:/ 'inventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { _isbdPrefix     :: !(Maybe Text)
  , _isbdAccountId  :: !(Maybe Text)
  , _isbdEncryption :: !(Maybe InventoryEncryption)
  , _isbdBucket     :: !BucketName
  , _isbdFormat     :: !InventoryFormat
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isbdPrefix' - The prefix that is prepended to all inventory results.
--
-- * 'isbdAccountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- * 'isbdEncryption' - Contains the type of server-side encryption used to encrypt the inventory results.
--
-- * 'isbdBucket' - The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
--
-- * 'isbdFormat' - Specifies the output format of the inventory results.
inventoryS3BucketDestination
    :: BucketName -- ^ 'isbdBucket'
    -> InventoryFormat -- ^ 'isbdFormat'
    -> InventoryS3BucketDestination
inventoryS3BucketDestination pBucket_ pFormat_ =
  InventoryS3BucketDestination'
    { _isbdPrefix = Nothing
    , _isbdAccountId = Nothing
    , _isbdEncryption = Nothing
    , _isbdBucket = pBucket_
    , _isbdFormat = pFormat_
    }


-- | The prefix that is prepended to all inventory results.
isbdPrefix :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdPrefix = lens _isbdPrefix (\ s a -> s{_isbdPrefix = a})

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
isbdAccountId :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdAccountId = lens _isbdAccountId (\ s a -> s{_isbdAccountId = a})

-- | Contains the type of server-side encryption used to encrypt the inventory results.
isbdEncryption :: Lens' InventoryS3BucketDestination (Maybe InventoryEncryption)
isbdEncryption = lens _isbdEncryption (\ s a -> s{_isbdEncryption = a})

-- | The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
isbdBucket :: Lens' InventoryS3BucketDestination BucketName
isbdBucket = lens _isbdBucket (\ s a -> s{_isbdBucket = a})

-- | Specifies the output format of the inventory results.
isbdFormat :: Lens' InventoryS3BucketDestination InventoryFormat
isbdFormat = lens _isbdFormat (\ s a -> s{_isbdFormat = a})

instance FromXML InventoryS3BucketDestination where
        parseXML x
          = InventoryS3BucketDestination' <$>
              (x .@? "Prefix") <*> (x .@? "AccountId") <*>
                (x .@? "Encryption")
                <*> (x .@ "Bucket")
                <*> (x .@ "Format")

instance Hashable InventoryS3BucketDestination where

instance NFData InventoryS3BucketDestination where

instance ToXML InventoryS3BucketDestination where
        toXML InventoryS3BucketDestination'{..}
          = mconcat
              ["Prefix" @= _isbdPrefix,
               "AccountId" @= _isbdAccountId,
               "Encryption" @= _isbdEncryption,
               "Bucket" @= _isbdBucket, "Format" @= _isbdFormat]

-- | Specifies the schedule for generating inventory results.
--
--
--
-- /See:/ 'inventorySchedule' smart constructor.
newtype InventorySchedule = InventorySchedule'
  { _isFrequency :: InventoryFrequency
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventorySchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isFrequency' - Specifies how frequently inventory results are produced.
inventorySchedule
    :: InventoryFrequency -- ^ 'isFrequency'
    -> InventorySchedule
inventorySchedule pFrequency_ = InventorySchedule' {_isFrequency = pFrequency_}


-- | Specifies how frequently inventory results are produced.
isFrequency :: Lens' InventorySchedule InventoryFrequency
isFrequency = lens _isFrequency (\ s a -> s{_isFrequency = a})

instance FromXML InventorySchedule where
        parseXML x
          = InventorySchedule' <$> (x .@ "Frequency")

instance Hashable InventorySchedule where

instance NFData InventorySchedule where

instance ToXML InventorySchedule where
        toXML InventorySchedule'{..}
          = mconcat ["Frequency" @= _isFrequency]

-- | Specifies JSON as object's input serialization format.
--
--
--
-- /See:/ 'jsonInput' smart constructor.
newtype JSONInput = JSONInput'
  { _jiType :: Maybe JSONType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jiType' - The type of JSON. Valid values: Document, Lines.
jsonInput
    :: JSONInput
jsonInput = JSONInput' {_jiType = Nothing}


-- | The type of JSON. Valid values: Document, Lines.
jiType :: Lens' JSONInput (Maybe JSONType)
jiType = lens _jiType (\ s a -> s{_jiType = a})

instance Hashable JSONInput where

instance NFData JSONInput where

instance ToXML JSONInput where
        toXML JSONInput'{..} = mconcat ["Type" @= _jiType]

-- | Specifies JSON as request's output serialization format.
--
--
--
-- /See:/ 'jsonOutput' smart constructor.
newtype JSONOutput = JSONOutput'
  { _joRecordDelimiter :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'joRecordDelimiter' - The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
jsonOutput
    :: JSONOutput
jsonOutput = JSONOutput' {_joRecordDelimiter = Nothing}


-- | The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
joRecordDelimiter :: Lens' JSONOutput (Maybe Text)
joRecordDelimiter = lens _joRecordDelimiter (\ s a -> s{_joRecordDelimiter = a})

instance Hashable JSONOutput where

instance NFData JSONOutput where

instance ToXML JSONOutput where
        toXML JSONOutput'{..}
          = mconcat ["RecordDelimiter" @= _joRecordDelimiter]

-- | A container for specifying the configuration for AWS Lambda notifications.
--
--
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { _lfcId                :: !(Maybe Text)
  , _lfcFilter            :: !(Maybe NotificationConfigurationFilter)
  , _lfcLambdaFunctionARN :: !Text
  , _lfcEvents            :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfcId' - Undocumented member.
--
-- * 'lfcFilter' - Undocumented member.
--
-- * 'lfcLambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
--
-- * 'lfcEvents' - The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
lambdaFunctionConfiguration
    :: Text -- ^ 'lfcLambdaFunctionARN'
    -> LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN_ =
  LambdaFunctionConfiguration'
    { _lfcId = Nothing
    , _lfcFilter = Nothing
    , _lfcLambdaFunctionARN = pLambdaFunctionARN_
    , _lfcEvents = mempty
    }


-- | Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\ s a -> s{_lfcId = a})

-- | Undocumented member.
lfcFilter :: Lens' LambdaFunctionConfiguration (Maybe NotificationConfigurationFilter)
lfcFilter = lens _lfcFilter (\ s a -> s{_lfcFilter = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
lfcLambdaFunctionARN :: Lens' LambdaFunctionConfiguration Text
lfcLambdaFunctionARN = lens _lfcLambdaFunctionARN (\ s a -> s{_lfcLambdaFunctionARN = a})

-- | The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
lfcEvents :: Lens' LambdaFunctionConfiguration [Event]
lfcEvents = lens _lfcEvents (\ s a -> s{_lfcEvents = a}) . _Coerce

instance FromXML LambdaFunctionConfiguration where
        parseXML x
          = LambdaFunctionConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*>
                (x .@ "CloudFunction")
                <*> (parseXMLList "Event" x)

instance Hashable LambdaFunctionConfiguration where

instance NFData LambdaFunctionConfiguration where

instance ToXML LambdaFunctionConfiguration where
        toXML LambdaFunctionConfiguration'{..}
          = mconcat
              ["Id" @= _lfcId, "Filter" @= _lfcFilter,
               "CloudFunction" @= _lfcLambdaFunctionARN,
               toXMLList "Event" _lfcEvents]

-- | Container for the expiration for the lifecycle of the object.
--
--
--
-- /See:/ 'lifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { _leDays                      :: !(Maybe Int)
  , _leDate                      :: !(Maybe ISO8601)
  , _leExpiredObjectDeleteMarker :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leDays' - Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
--
-- * 'leDate' - Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
--
-- * 'leExpiredObjectDeleteMarker' - Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
lifecycleExpiration
    :: LifecycleExpiration
lifecycleExpiration =
  LifecycleExpiration'
    { _leDays = Nothing
    , _leDate = Nothing
    , _leExpiredObjectDeleteMarker = Nothing
    }


-- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\ s a -> s{_leDays = a})

-- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\ s a -> s{_leDate = a}) . mapping _Time

-- | Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
leExpiredObjectDeleteMarker :: Lens' LifecycleExpiration (Maybe Bool)
leExpiredObjectDeleteMarker = lens _leExpiredObjectDeleteMarker (\ s a -> s{_leExpiredObjectDeleteMarker = a})

instance FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "ExpiredObjectDeleteMarker")

instance Hashable LifecycleExpiration where

instance NFData LifecycleExpiration where

instance ToXML LifecycleExpiration where
        toXML LifecycleExpiration'{..}
          = mconcat
              ["Days" @= _leDays, "Date" @= _leDate,
               "ExpiredObjectDeleteMarker" @=
                 _leExpiredObjectDeleteMarker]

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
--
--
-- /See:/ 'lifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { _lrTransitions                    :: !(Maybe [Transition])
  , _lrNoncurrentVersionExpiration    :: !(Maybe NoncurrentVersionExpiration)
  , _lrPrefix                         :: !(Maybe Text)
  , _lrNoncurrentVersionTransitions   :: !(Maybe [NoncurrentVersionTransition])
  , _lrExpiration                     :: !(Maybe LifecycleExpiration)
  , _lrId                             :: !(Maybe Text)
  , _lrFilter                         :: !(Maybe LifecycleRuleFilter)
  , _lrAbortIncompleteMultipartUpload :: !(Maybe AbortIncompleteMultipartUpload)
  , _lrStatus                         :: !ExpirationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrTransitions' - Specifies when an Amazon S3 object transitions to a specified storage class.
--
-- * 'lrNoncurrentVersionExpiration' - Undocumented member.
--
-- * 'lrPrefix' - Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
--
-- * 'lrNoncurrentVersionTransitions' - Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
--
-- * 'lrExpiration' - Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
--
-- * 'lrId' - Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- * 'lrFilter' - Undocumented member.
--
-- * 'lrAbortIncompleteMultipartUpload' - Undocumented member.
--
-- * 'lrStatus' - If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
lifecycleRule
    :: ExpirationStatus -- ^ 'lrStatus'
    -> LifecycleRule
lifecycleRule pStatus_ =
  LifecycleRule'
    { _lrTransitions = Nothing
    , _lrNoncurrentVersionExpiration = Nothing
    , _lrPrefix = Nothing
    , _lrNoncurrentVersionTransitions = Nothing
    , _lrExpiration = Nothing
    , _lrId = Nothing
    , _lrFilter = Nothing
    , _lrAbortIncompleteMultipartUpload = Nothing
    , _lrStatus = pStatus_
    }


-- | Specifies when an Amazon S3 object transitions to a specified storage class.
lrTransitions :: Lens' LifecycleRule [Transition]
lrTransitions = lens _lrTransitions (\ s a -> s{_lrTransitions = a}) . _Default . _Coerce

-- | Undocumented member.
lrNoncurrentVersionExpiration :: Lens' LifecycleRule (Maybe NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = lens _lrNoncurrentVersionExpiration (\ s a -> s{_lrNoncurrentVersionExpiration = a})

-- | Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
lrPrefix :: Lens' LifecycleRule (Maybe Text)
lrPrefix = lens _lrPrefix (\ s a -> s{_lrPrefix = a})

-- | Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
lrNoncurrentVersionTransitions :: Lens' LifecycleRule [NoncurrentVersionTransition]
lrNoncurrentVersionTransitions = lens _lrNoncurrentVersionTransitions (\ s a -> s{_lrNoncurrentVersionTransitions = a}) . _Default . _Coerce

-- | Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
lrExpiration :: Lens' LifecycleRule (Maybe LifecycleExpiration)
lrExpiration = lens _lrExpiration (\ s a -> s{_lrExpiration = a})

-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
lrId :: Lens' LifecycleRule (Maybe Text)
lrId = lens _lrId (\ s a -> s{_lrId = a})

-- | Undocumented member.
lrFilter :: Lens' LifecycleRule (Maybe LifecycleRuleFilter)
lrFilter = lens _lrFilter (\ s a -> s{_lrFilter = a})

-- | Undocumented member.
lrAbortIncompleteMultipartUpload :: Lens' LifecycleRule (Maybe AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = lens _lrAbortIncompleteMultipartUpload (\ s a -> s{_lrAbortIncompleteMultipartUpload = a})

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
lrStatus :: Lens' LifecycleRule ExpirationStatus
lrStatus = lens _lrStatus (\ s a -> s{_lrStatus = a})

instance FromXML LifecycleRule where
        parseXML x
          = LifecycleRule' <$>
              (may (parseXMLList "Transition") x) <*>
                (x .@? "NoncurrentVersionExpiration")
                <*> (x .@? "Prefix")
                <*>
                (may (parseXMLList "NoncurrentVersionTransition") x)
                <*> (x .@? "Expiration")
                <*> (x .@? "ID")
                <*> (x .@? "Filter")
                <*> (x .@? "AbortIncompleteMultipartUpload")
                <*> (x .@ "Status")

instance Hashable LifecycleRule where

instance NFData LifecycleRule where

instance ToXML LifecycleRule where
        toXML LifecycleRule'{..}
          = mconcat
              [toXML (toXMLList "Transition" <$> _lrTransitions),
               "NoncurrentVersionExpiration" @=
                 _lrNoncurrentVersionExpiration,
               "Prefix" @= _lrPrefix,
               toXML
                 (toXMLList "NoncurrentVersionTransition" <$>
                    _lrNoncurrentVersionTransitions),
               "Expiration" @= _lrExpiration, "ID" @= _lrId,
               "Filter" @= _lrFilter,
               "AbortIncompleteMultipartUpload" @=
                 _lrAbortIncompleteMultipartUpload,
               "Status" @= _lrStatus]

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
--
--
--
-- /See:/ 'lifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { _lraoPrefix :: !(Maybe Text)
  , _lraoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRuleAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lraoPrefix' - Prefix identifying one or more objects to which the rule applies.
--
-- * 'lraoTags' - All of these tags must exist in the object's tag set in order for the rule to apply.
lifecycleRuleAndOperator
    :: LifecycleRuleAndOperator
lifecycleRuleAndOperator =
  LifecycleRuleAndOperator' {_lraoPrefix = Nothing, _lraoTags = Nothing}


-- | Prefix identifying one or more objects to which the rule applies.
lraoPrefix :: Lens' LifecycleRuleAndOperator (Maybe Text)
lraoPrefix = lens _lraoPrefix (\ s a -> s{_lraoPrefix = a})

-- | All of these tags must exist in the object's tag set in order for the rule to apply.
lraoTags :: Lens' LifecycleRuleAndOperator [Tag]
lraoTags = lens _lraoTags (\ s a -> s{_lraoTags = a}) . _Default . _Coerce

instance FromXML LifecycleRuleAndOperator where
        parseXML x
          = LifecycleRuleAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable LifecycleRuleAndOperator where

instance NFData LifecycleRuleAndOperator where

instance ToXML LifecycleRuleAndOperator where
        toXML LifecycleRuleAndOperator'{..}
          = mconcat
              ["Prefix" @= _lraoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _lraoTags)]

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies to. A @Filter@ must have exactly one of @Prefix@ , @Tag@ , or @And@ specified.
--
--
--
-- /See:/ 'lifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { _lrfTag    :: !(Maybe Tag)
  , _lrfPrefix :: !(Maybe Text)
  , _lrfAnd    :: !(Maybe LifecycleRuleAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRuleFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfTag' - This tag must exist in the object's tag set in order for the rule to apply.
--
-- * 'lrfPrefix' - Prefix identifying one or more objects to which the rule applies.
--
-- * 'lrfAnd' - Undocumented member.
lifecycleRuleFilter
    :: LifecycleRuleFilter
lifecycleRuleFilter =
  LifecycleRuleFilter'
    {_lrfTag = Nothing, _lrfPrefix = Nothing, _lrfAnd = Nothing}


-- | This tag must exist in the object's tag set in order for the rule to apply.
lrfTag :: Lens' LifecycleRuleFilter (Maybe Tag)
lrfTag = lens _lrfTag (\ s a -> s{_lrfTag = a})

-- | Prefix identifying one or more objects to which the rule applies.
lrfPrefix :: Lens' LifecycleRuleFilter (Maybe Text)
lrfPrefix = lens _lrfPrefix (\ s a -> s{_lrfPrefix = a})

-- | Undocumented member.
lrfAnd :: Lens' LifecycleRuleFilter (Maybe LifecycleRuleAndOperator)
lrfAnd = lens _lrfAnd (\ s a -> s{_lrfAnd = a})

instance FromXML LifecycleRuleFilter where
        parseXML x
          = LifecycleRuleFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable LifecycleRuleFilter where

instance NFData LifecycleRuleFilter where

instance ToXML LifecycleRuleFilter where
        toXML LifecycleRuleFilter'{..}
          = mconcat
              ["Tag" @= _lrfTag, "Prefix" @= _lrfPrefix,
               "And" @= _lrfAnd]

-- | Describes where logs are stored and the prefix that Amazon S3 assigns to all log object keys for a bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlogging.html PUT Bucket logging> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'loggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
  { _leTargetGrants :: !(Maybe [TargetGrant])
  , _leTargetBucket :: !Text
  , _leTargetPrefix :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leTargetGrants' - Container for granting information.
--
-- * 'leTargetBucket' - Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
--
-- * 'leTargetPrefix' - A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
loggingEnabled
    :: Text -- ^ 'leTargetBucket'
    -> Text -- ^ 'leTargetPrefix'
    -> LoggingEnabled
loggingEnabled pTargetBucket_ pTargetPrefix_ =
  LoggingEnabled'
    { _leTargetGrants = Nothing
    , _leTargetBucket = pTargetBucket_
    , _leTargetPrefix = pTargetPrefix_
    }


-- | Container for granting information.
leTargetGrants :: Lens' LoggingEnabled [TargetGrant]
leTargetGrants = lens _leTargetGrants (\ s a -> s{_leTargetGrants = a}) . _Default . _Coerce

-- | Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
leTargetBucket :: Lens' LoggingEnabled Text
leTargetBucket = lens _leTargetBucket (\ s a -> s{_leTargetBucket = a})

-- | A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
leTargetPrefix :: Lens' LoggingEnabled Text
leTargetPrefix = lens _leTargetPrefix (\ s a -> s{_leTargetPrefix = a})

instance FromXML LoggingEnabled where
        parseXML x
          = LoggingEnabled' <$>
              (x .@? "TargetGrants" .!@ mempty >>=
                 may (parseXMLList "Grant"))
                <*> (x .@ "TargetBucket")
                <*> (x .@ "TargetPrefix")

instance Hashable LoggingEnabled where

instance NFData LoggingEnabled where

instance ToXML LoggingEnabled where
        toXML LoggingEnabled'{..}
          = mconcat
              ["TargetGrants" @=
                 toXML (toXMLList "Grant" <$> _leTargetGrants),
               "TargetBucket" @= _leTargetBucket,
               "TargetPrefix" @= _leTargetPrefix]

-- | A metadata key-value pair to store with an object.
--
--
--
-- /See:/ 'metadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { _meValue :: !(Maybe Text)
  , _meName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetadataEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meValue' - Value of the Object.
--
-- * 'meName' - Name of the Object.
metadataEntry
    :: MetadataEntry
metadataEntry = MetadataEntry' {_meValue = Nothing, _meName = Nothing}


-- | Value of the Object.
meValue :: Lens' MetadataEntry (Maybe Text)
meValue = lens _meValue (\ s a -> s{_meValue = a})

-- | Name of the Object.
meName :: Lens' MetadataEntry (Maybe Text)
meName = lens _meName (\ s a -> s{_meName = a})

instance Hashable MetadataEntry where

instance NFData MetadataEntry where

instance ToXML MetadataEntry where
        toXML MetadataEntry'{..}
          = mconcat ["Value" @= _meValue, "Name" @= _meName]

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
--
--
-- /See:/ 'metrics' smart constructor.
data Metrics = Metrics'
  { _mEventThreshold :: !(Maybe ReplicationTimeValue)
  , _mStatus         :: !MetricsStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Metrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mEventThreshold' - A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
--
-- * 'mStatus' - Specifies whether the replication metrics are enabled.
metrics
    :: MetricsStatus -- ^ 'mStatus'
    -> Metrics
metrics pStatus_ = Metrics' {_mEventThreshold = Nothing, _mStatus = pStatus_}


-- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
mEventThreshold :: Lens' Metrics (Maybe ReplicationTimeValue)
mEventThreshold = lens _mEventThreshold (\ s a -> s{_mEventThreshold = a})

-- | Specifies whether the replication metrics are enabled.
mStatus :: Lens' Metrics MetricsStatus
mStatus = lens _mStatus (\ s a -> s{_mStatus = a})

instance FromXML Metrics where
        parseXML x
          = Metrics' <$>
              (x .@? "EventThreshold") <*> (x .@ "Status")

instance Hashable Metrics where

instance NFData Metrics where

instance ToXML Metrics where
        toXML Metrics'{..}
          = mconcat
              ["EventThreshold" @= _mEventThreshold,
               "Status" @= _mStatus]

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
--
--
-- /See:/ 'metricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { _maoPrefix :: !(Maybe Text)
  , _maoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maoPrefix' - The prefix used when evaluating an AND predicate.
--
-- * 'maoTags' - The list of tags used when evaluating an AND predicate.
metricsAndOperator
    :: MetricsAndOperator
metricsAndOperator =
  MetricsAndOperator' {_maoPrefix = Nothing, _maoTags = Nothing}


-- | The prefix used when evaluating an AND predicate.
maoPrefix :: Lens' MetricsAndOperator (Maybe Text)
maoPrefix = lens _maoPrefix (\ s a -> s{_maoPrefix = a})

-- | The list of tags used when evaluating an AND predicate.
maoTags :: Lens' MetricsAndOperator [Tag]
maoTags = lens _maoTags (\ s a -> s{_maoTags = a}) . _Default . _Coerce

instance FromXML MetricsAndOperator where
        parseXML x
          = MetricsAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable MetricsAndOperator where

instance NFData MetricsAndOperator where

instance ToXML MetricsAndOperator where
        toXML MetricsAndOperator'{..}
          = mconcat
              ["Prefix" @= _maoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _maoTags)]

-- | Specifies a metrics configuration for the CloudWatch request metrics (specified by the metrics configuration ID) from an Amazon S3 bucket. If you're updating an existing metrics configuration, note that this is a full replacement of the existing metrics configuration. If you don't include the elements you want to keep, they are erased. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTMetricConfiguration.html PUT Bucket metrics> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'metricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { _mcFilter :: !(Maybe MetricsFilter)
  , _mcId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcFilter' - Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- * 'mcId' - The ID used to identify the metrics configuration.
metricsConfiguration
    :: Text -- ^ 'mcId'
    -> MetricsConfiguration
metricsConfiguration pId_ =
  MetricsConfiguration' {_mcFilter = Nothing, _mcId = pId_}


-- | Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
mcFilter :: Lens' MetricsConfiguration (Maybe MetricsFilter)
mcFilter = lens _mcFilter (\ s a -> s{_mcFilter = a})

-- | The ID used to identify the metrics configuration.
mcId :: Lens' MetricsConfiguration Text
mcId = lens _mcId (\ s a -> s{_mcId = a})

instance FromXML MetricsConfiguration where
        parseXML x
          = MetricsConfiguration' <$>
              (x .@? "Filter") <*> (x .@ "Id")

instance Hashable MetricsConfiguration where

instance NFData MetricsConfiguration where

instance ToXML MetricsConfiguration where
        toXML MetricsConfiguration'{..}
          = mconcat ["Filter" @= _mcFilter, "Id" @= _mcId]

-- | Specifies a metrics configuration filter. The metrics configuration only includes objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
--
--
-- /See:/ 'metricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { _mfTag    :: !(Maybe Tag)
  , _mfPrefix :: !(Maybe Text)
  , _mfAnd    :: !(Maybe MetricsAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfTag' - The tag used when evaluating a metrics filter.
--
-- * 'mfPrefix' - The prefix used when evaluating a metrics filter.
--
-- * 'mfAnd' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
metricsFilter
    :: MetricsFilter
metricsFilter =
  MetricsFilter' {_mfTag = Nothing, _mfPrefix = Nothing, _mfAnd = Nothing}


-- | The tag used when evaluating a metrics filter.
mfTag :: Lens' MetricsFilter (Maybe Tag)
mfTag = lens _mfTag (\ s a -> s{_mfTag = a})

-- | The prefix used when evaluating a metrics filter.
mfPrefix :: Lens' MetricsFilter (Maybe Text)
mfPrefix = lens _mfPrefix (\ s a -> s{_mfPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
mfAnd :: Lens' MetricsFilter (Maybe MetricsAndOperator)
mfAnd = lens _mfAnd (\ s a -> s{_mfAnd = a})

instance FromXML MetricsFilter where
        parseXML x
          = MetricsFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable MetricsFilter where

instance NFData MetricsFilter where

instance ToXML MetricsFilter where
        toXML MetricsFilter'{..}
          = mconcat
              ["Tag" @= _mfTag, "Prefix" @= _mfPrefix,
               "And" @= _mfAnd]

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
--
--
-- /See:/ 'multipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { _muInitiated    :: !(Maybe ISO8601)
  , _muInitiator    :: !(Maybe Initiator)
  , _muOwner        :: !(Maybe Owner)
  , _muKey          :: !(Maybe ObjectKey)
  , _muStorageClass :: !(Maybe StorageClass)
  , _muUploadId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muInitiated' - Date and time at which the multipart upload was initiated.
--
-- * 'muInitiator' - Identifies who initiated the multipart upload.
--
-- * 'muOwner' - Specifies the owner of the object that is part of the multipart upload.
--
-- * 'muKey' - Key of the object for which the multipart upload was initiated.
--
-- * 'muStorageClass' - The class of storage used to store the object.
--
-- * 'muUploadId' - Upload ID that identifies the multipart upload.
multipartUpload
    :: MultipartUpload
multipartUpload =
  MultipartUpload'
    { _muInitiated = Nothing
    , _muInitiator = Nothing
    , _muOwner = Nothing
    , _muKey = Nothing
    , _muStorageClass = Nothing
    , _muUploadId = Nothing
    }


-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\ s a -> s{_muInitiated = a}) . mapping _Time

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\ s a -> s{_muInitiator = a})

-- | Specifies the owner of the object that is part of the multipart upload.
muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\ s a -> s{_muOwner = a})

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\ s a -> s{_muKey = a})

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\ s a -> s{_muStorageClass = a})

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\ s a -> s{_muUploadId = a})

instance FromXML MultipartUpload where
        parseXML x
          = MultipartUpload' <$>
              (x .@? "Initiated") <*> (x .@? "Initiator") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "UploadId")

instance Hashable MultipartUpload where

instance NFData MultipartUpload where

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
--
--
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { _nveNoncurrentDays :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoncurrentVersionExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nveNoncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
noncurrentVersionExpiration
    :: Int -- ^ 'nveNoncurrentDays'
    -> NoncurrentVersionExpiration
noncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration' {_nveNoncurrentDays = pNoncurrentDays_}


-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays = lens _nveNoncurrentDays (\ s a -> s{_nveNoncurrentDays = a})

instance FromXML NoncurrentVersionExpiration where
        parseXML x
          = NoncurrentVersionExpiration' <$>
              (x .@ "NoncurrentDays")

instance Hashable NoncurrentVersionExpiration where

instance NFData NoncurrentVersionExpiration where

instance ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration'{..}
          = mconcat ["NoncurrentDays" @= _nveNoncurrentDays]

-- | Container for the transition rule that describes when noncurrent objects transition to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class at a specific period in the object's lifetime.
--
--
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { _nvtNoncurrentDays :: !Int
  , _nvtStorageClass   :: !TransitionStorageClass
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoncurrentVersionTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nvtNoncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'nvtStorageClass' - The class of storage used to store the object.
noncurrentVersionTransition
    :: Int -- ^ 'nvtNoncurrentDays'
    -> TransitionStorageClass -- ^ 'nvtStorageClass'
    -> NoncurrentVersionTransition
noncurrentVersionTransition pNoncurrentDays_ pStorageClass_ =
  NoncurrentVersionTransition'
    {_nvtNoncurrentDays = pNoncurrentDays_, _nvtStorageClass = pStorageClass_}


-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays = lens _nvtNoncurrentDays (\ s a -> s{_nvtNoncurrentDays = a})

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = lens _nvtStorageClass (\ s a -> s{_nvtStorageClass = a})

instance FromXML NoncurrentVersionTransition where
        parseXML x
          = NoncurrentVersionTransition' <$>
              (x .@ "NoncurrentDays") <*> (x .@ "StorageClass")

instance Hashable NoncurrentVersionTransition where

instance NFData NoncurrentVersionTransition where

instance ToXML NoncurrentVersionTransition where
        toXML NoncurrentVersionTransition'{..}
          = mconcat
              ["NoncurrentDays" @= _nvtNoncurrentDays,
               "StorageClass" @= _nvtStorageClass]

-- | A container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off for the bucket.
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncQueueConfigurations          :: !(Maybe [QueueConfiguration])
  , _ncTopicConfigurations          :: !(Maybe [TopicConfiguration])
  , _ncLambdaFunctionConfigurations :: !(Maybe [LambdaFunctionConfiguration])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncQueueConfigurations' - The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
--
-- * 'ncTopicConfigurations' - The topic to which notifications are sent and the events for which notifications are generated.
--
-- * 'ncLambdaFunctionConfigurations' - Describes the AWS Lambda functions to invoke and the events for which to invoke them.
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration'
    { _ncQueueConfigurations = Nothing
    , _ncTopicConfigurations = Nothing
    , _ncLambdaFunctionConfigurations = Nothing
    }


-- | The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
ncQueueConfigurations :: Lens' NotificationConfiguration [QueueConfiguration]
ncQueueConfigurations = lens _ncQueueConfigurations (\ s a -> s{_ncQueueConfigurations = a}) . _Default . _Coerce

-- | The topic to which notifications are sent and the events for which notifications are generated.
ncTopicConfigurations :: Lens' NotificationConfiguration [TopicConfiguration]
ncTopicConfigurations = lens _ncTopicConfigurations (\ s a -> s{_ncTopicConfigurations = a}) . _Default . _Coerce

-- | Describes the AWS Lambda functions to invoke and the events for which to invoke them.
ncLambdaFunctionConfigurations :: Lens' NotificationConfiguration [LambdaFunctionConfiguration]
ncLambdaFunctionConfigurations = lens _ncLambdaFunctionConfigurations (\ s a -> s{_ncLambdaFunctionConfigurations = a}) . _Default . _Coerce

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (may (parseXMLList "QueueConfiguration") x) <*>
                (may (parseXMLList "TopicConfiguration") x)
                <*>
                (may (parseXMLList "CloudFunctionConfiguration") x)

instance Hashable NotificationConfiguration where

instance NFData NotificationConfiguration where

instance ToXML NotificationConfiguration where
        toXML NotificationConfiguration'{..}
          = mconcat
              [toXML
                 (toXMLList "QueueConfiguration" <$>
                    _ncQueueConfigurations),
               toXML
                 (toXMLList "TopicConfiguration" <$>
                    _ncTopicConfigurations),
               toXML
                 (toXMLList "CloudFunctionConfiguration" <$>
                    _ncLambdaFunctionConfigurations)]

-- | Specifies object key name filtering rules. For information about key name filtering, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'notificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
  { _ncfKey :: Maybe S3KeyFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfigurationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncfKey' - Undocumented member.
notificationConfigurationFilter
    :: NotificationConfigurationFilter
notificationConfigurationFilter =
  NotificationConfigurationFilter' {_ncfKey = Nothing}


-- | Undocumented member.
ncfKey :: Lens' NotificationConfigurationFilter (Maybe S3KeyFilter)
ncfKey = lens _ncfKey (\ s a -> s{_ncfKey = a})

instance FromXML NotificationConfigurationFilter
         where
        parseXML x
          = NotificationConfigurationFilter' <$>
              (x .@? "S3Key")

instance Hashable NotificationConfigurationFilter
         where

instance NFData NotificationConfigurationFilter where

instance ToXML NotificationConfigurationFilter where
        toXML NotificationConfigurationFilter'{..}
          = mconcat ["S3Key" @= _ncfKey]

-- | An object consists of data and its descriptive metadata.
--
--
--
-- /See:/ 'object'' smart constructor.
data Object = Object'
  { _oOwner        :: !(Maybe Owner)
  , _oETag         :: !ETag
  , _oSize         :: !Int
  , _oKey          :: !ObjectKey
  , _oStorageClass :: !ObjectStorageClass
  , _oLastModified :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOwner' - The owner of the object
--
-- * 'oETag' - The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
--
-- * 'oSize' - Size in bytes of the object
--
-- * 'oKey' - The name that you assign to an object. You use the object key to retrieve the object.
--
-- * 'oStorageClass' - The class of storage used to store the object.
--
-- * 'oLastModified' - The date the Object was Last Modified
object'
    :: ETag -- ^ 'oETag'
    -> Int -- ^ 'oSize'
    -> ObjectKey -- ^ 'oKey'
    -> ObjectStorageClass -- ^ 'oStorageClass'
    -> UTCTime -- ^ 'oLastModified'
    -> Object
object' pETag_ pSize_ pKey_ pStorageClass_ pLastModified_ =
  Object'
    { _oOwner = Nothing
    , _oETag = pETag_
    , _oSize = pSize_
    , _oKey = pKey_
    , _oStorageClass = pStorageClass_
    , _oLastModified = _Time # pLastModified_
    }


-- | The owner of the object
oOwner :: Lens' Object (Maybe Owner)
oOwner = lens _oOwner (\ s a -> s{_oOwner = a})

-- | The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
oETag :: Lens' Object ETag
oETag = lens _oETag (\ s a -> s{_oETag = a})

-- | Size in bytes of the object
oSize :: Lens' Object Int
oSize = lens _oSize (\ s a -> s{_oSize = a})

-- | The name that you assign to an object. You use the object key to retrieve the object.
oKey :: Lens' Object ObjectKey
oKey = lens _oKey (\ s a -> s{_oKey = a})

-- | The class of storage used to store the object.
oStorageClass :: Lens' Object ObjectStorageClass
oStorageClass = lens _oStorageClass (\ s a -> s{_oStorageClass = a})

-- | The date the Object was Last Modified
oLastModified :: Lens' Object UTCTime
oLastModified = lens _oLastModified (\ s a -> s{_oLastModified = a}) . _Time

instance FromXML Object where
        parseXML x
          = Object' <$>
              (x .@? "Owner") <*> (x .@ "ETag") <*> (x .@ "Size")
                <*> (x .@ "Key")
                <*> (x .@ "StorageClass")
                <*> (x .@ "LastModified")

instance Hashable Object where

instance NFData Object where

-- | Object Identifier is unique value to identify objects.
--
--
--
-- /See:/ 'objectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { _oiVersionId :: !(Maybe ObjectVersionId)
  , _oiKey       :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiVersionId' - VersionId for the specific version of the object to delete.
--
-- * 'oiKey' - Key name of the object to delete.
objectIdentifier
    :: ObjectKey -- ^ 'oiKey'
    -> ObjectIdentifier
objectIdentifier pKey_ =
  ObjectIdentifier' {_oiVersionId = Nothing, _oiKey = pKey_}


-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\ s a -> s{_oiVersionId = a})

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\ s a -> s{_oiKey = a})

instance Hashable ObjectIdentifier where

instance NFData ObjectIdentifier where

instance ToXML ObjectIdentifier where
        toXML ObjectIdentifier'{..}
          = mconcat
              ["VersionId" @= _oiVersionId, "Key" @= _oiKey]

-- | The container element for Object Lock configuration parameters.
--
--
--
-- /See:/ 'objectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { _olcObjectLockEnabled :: !(Maybe ObjectLockEnabled)
  , _olcRule              :: !(Maybe ObjectLockRule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectLockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olcObjectLockEnabled' - Indicates whether this bucket has an Object Lock configuration enabled.
--
-- * 'olcRule' - The Object Lock rule in place for the specified object.
objectLockConfiguration
    :: ObjectLockConfiguration
objectLockConfiguration =
  ObjectLockConfiguration' {_olcObjectLockEnabled = Nothing, _olcRule = Nothing}


-- | Indicates whether this bucket has an Object Lock configuration enabled.
olcObjectLockEnabled :: Lens' ObjectLockConfiguration (Maybe ObjectLockEnabled)
olcObjectLockEnabled = lens _olcObjectLockEnabled (\ s a -> s{_olcObjectLockEnabled = a})

-- | The Object Lock rule in place for the specified object.
olcRule :: Lens' ObjectLockConfiguration (Maybe ObjectLockRule)
olcRule = lens _olcRule (\ s a -> s{_olcRule = a})

instance FromXML ObjectLockConfiguration where
        parseXML x
          = ObjectLockConfiguration' <$>
              (x .@? "ObjectLockEnabled") <*> (x .@? "Rule")

instance Hashable ObjectLockConfiguration where

instance NFData ObjectLockConfiguration where

instance ToXML ObjectLockConfiguration where
        toXML ObjectLockConfiguration'{..}
          = mconcat
              ["ObjectLockEnabled" @= _olcObjectLockEnabled,
               "Rule" @= _olcRule]

-- | A Legal Hold configuration for an object.
--
--
--
-- /See:/ 'objectLockLegalHold' smart constructor.
newtype ObjectLockLegalHold = ObjectLockLegalHold'
  { _ollhStatus :: Maybe ObjectLockLegalHoldStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectLockLegalHold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ollhStatus' - Indicates whether the specified object has a Legal Hold in place.
objectLockLegalHold
    :: ObjectLockLegalHold
objectLockLegalHold = ObjectLockLegalHold' {_ollhStatus = Nothing}


-- | Indicates whether the specified object has a Legal Hold in place.
ollhStatus :: Lens' ObjectLockLegalHold (Maybe ObjectLockLegalHoldStatus)
ollhStatus = lens _ollhStatus (\ s a -> s{_ollhStatus = a})

instance FromXML ObjectLockLegalHold where
        parseXML x
          = ObjectLockLegalHold' <$> (x .@? "Status")

instance Hashable ObjectLockLegalHold where

instance NFData ObjectLockLegalHold where

instance ToXML ObjectLockLegalHold where
        toXML ObjectLockLegalHold'{..}
          = mconcat ["Status" @= _ollhStatus]

-- | A Retention configuration for an object.
--
--
--
-- /See:/ 'objectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { _olrMode            :: !(Maybe ObjectLockRetentionMode)
  , _olrRetainUntilDate :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectLockRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrMode' - Indicates the Retention mode for the specified object.
--
-- * 'olrRetainUntilDate' - The date on which this Object Lock Retention will expire.
objectLockRetention
    :: ObjectLockRetention
objectLockRetention =
  ObjectLockRetention' {_olrMode = Nothing, _olrRetainUntilDate = Nothing}


-- | Indicates the Retention mode for the specified object.
olrMode :: Lens' ObjectLockRetention (Maybe ObjectLockRetentionMode)
olrMode = lens _olrMode (\ s a -> s{_olrMode = a})

-- | The date on which this Object Lock Retention will expire.
olrRetainUntilDate :: Lens' ObjectLockRetention (Maybe UTCTime)
olrRetainUntilDate = lens _olrRetainUntilDate (\ s a -> s{_olrRetainUntilDate = a}) . mapping _Time

instance FromXML ObjectLockRetention where
        parseXML x
          = ObjectLockRetention' <$>
              (x .@? "Mode") <*> (x .@? "RetainUntilDate")

instance Hashable ObjectLockRetention where

instance NFData ObjectLockRetention where

instance ToXML ObjectLockRetention where
        toXML ObjectLockRetention'{..}
          = mconcat
              ["Mode" @= _olrMode,
               "RetainUntilDate" @= _olrRetainUntilDate]

-- | The container element for an Object Lock rule.
--
--
--
-- /See:/ 'objectLockRule' smart constructor.
newtype ObjectLockRule = ObjectLockRule'
  { _olrDefaultRetention :: Maybe DefaultRetention
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectLockRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrDefaultRetention' - The default retention period that you want to apply to new objects placed in the specified bucket.
objectLockRule
    :: ObjectLockRule
objectLockRule = ObjectLockRule' {_olrDefaultRetention = Nothing}


-- | The default retention period that you want to apply to new objects placed in the specified bucket.
olrDefaultRetention :: Lens' ObjectLockRule (Maybe DefaultRetention)
olrDefaultRetention = lens _olrDefaultRetention (\ s a -> s{_olrDefaultRetention = a})

instance FromXML ObjectLockRule where
        parseXML x
          = ObjectLockRule' <$> (x .@? "DefaultRetention")

instance Hashable ObjectLockRule where

instance NFData ObjectLockRule where

instance ToXML ObjectLockRule where
        toXML ObjectLockRule'{..}
          = mconcat
              ["DefaultRetention" @= _olrDefaultRetention]

-- | The version of an object.
--
--
--
-- /See:/ 'objectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { _ovETag         :: !(Maybe ETag)
  , _ovVersionId    :: !(Maybe ObjectVersionId)
  , _ovSize         :: !(Maybe Int)
  , _ovIsLatest     :: !(Maybe Bool)
  , _ovOwner        :: !(Maybe Owner)
  , _ovKey          :: !(Maybe ObjectKey)
  , _ovStorageClass :: !(Maybe ObjectVersionStorageClass)
  , _ovLastModified :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovETag' - The entity tag is an MD5 hash of that version of the object.
--
-- * 'ovVersionId' - Version ID of an object.
--
-- * 'ovSize' - Size in bytes of the object.
--
-- * 'ovIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'ovOwner' - Specifies the owner of the object.
--
-- * 'ovKey' - The object key.
--
-- * 'ovStorageClass' - The class of storage used to store the object.
--
-- * 'ovLastModified' - Date and time the object was last modified.
objectVersion
    :: ObjectVersion
objectVersion =
  ObjectVersion'
    { _ovETag = Nothing
    , _ovVersionId = Nothing
    , _ovSize = Nothing
    , _ovIsLatest = Nothing
    , _ovOwner = Nothing
    , _ovKey = Nothing
    , _ovStorageClass = Nothing
    , _ovLastModified = Nothing
    }


-- | The entity tag is an MD5 hash of that version of the object.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\ s a -> s{_ovETag = a})

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\ s a -> s{_ovVersionId = a})

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\ s a -> s{_ovSize = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\ s a -> s{_ovIsLatest = a})

-- | Specifies the owner of the object.
ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\ s a -> s{_ovOwner = a})

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\ s a -> s{_ovKey = a})

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\ s a -> s{_ovStorageClass = a})

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\ s a -> s{_ovLastModified = a}) . mapping _Time

instance FromXML ObjectVersion where
        parseXML x
          = ObjectVersion' <$>
              (x .@? "ETag") <*> (x .@? "VersionId") <*>
                (x .@? "Size")
                <*> (x .@? "IsLatest")
                <*> (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "LastModified")

instance Hashable ObjectVersion where

instance NFData ObjectVersion where

-- | Describes the location where the restore job's output is stored.
--
--
--
-- /See:/ 'outputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { _olS3 :: Maybe S3Location
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olS3' - Describes an S3 location that will receive the results of the restore request.
outputLocation
    :: OutputLocation
outputLocation = OutputLocation' {_olS3 = Nothing}


-- | Describes an S3 location that will receive the results of the restore request.
olS3 :: Lens' OutputLocation (Maybe S3Location)
olS3 = lens _olS3 (\ s a -> s{_olS3 = a})

instance Hashable OutputLocation where

instance NFData OutputLocation where

instance ToXML OutputLocation where
        toXML OutputLocation'{..} = mconcat ["S3" @= _olS3]

-- | Describes how results of the Select job are serialized.
--
--
--
-- /See:/ 'outputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { _osJSON :: !(Maybe JSONOutput)
  , _osCSV  :: !(Maybe CSVOutput)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osJSON' - Specifies JSON as request's output serialization format.
--
-- * 'osCSV' - Describes the serialization of CSV-encoded Select results.
outputSerialization
    :: OutputSerialization
outputSerialization = OutputSerialization' {_osJSON = Nothing, _osCSV = Nothing}


-- | Specifies JSON as request's output serialization format.
osJSON :: Lens' OutputSerialization (Maybe JSONOutput)
osJSON = lens _osJSON (\ s a -> s{_osJSON = a})

-- | Describes the serialization of CSV-encoded Select results.
osCSV :: Lens' OutputSerialization (Maybe CSVOutput)
osCSV = lens _osCSV (\ s a -> s{_osCSV = a})

instance Hashable OutputSerialization where

instance NFData OutputSerialization where

instance ToXML OutputSerialization where
        toXML OutputSerialization'{..}
          = mconcat ["JSON" @= _osJSON, "CSV" @= _osCSV]

-- | Container for the owner's display name and ID.
--
--
--
-- /See:/ 'owner' smart constructor.
data Owner = Owner'
  { _oDisplayName :: !(Maybe Text)
  , _oId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDisplayName' - Container for the display name of the owner.
--
-- * 'oId' - Container for the ID of the owner.
owner
    :: Owner
owner = Owner' {_oDisplayName = Nothing, _oId = Nothing}


-- | Container for the display name of the owner.
oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\ s a -> s{_oDisplayName = a})

-- | Container for the ID of the owner.
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

instance FromXML Owner where
        parseXML x
          = Owner' <$> (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Owner where

instance NFData Owner where

instance ToXML Owner where
        toXML Owner'{..}
          = mconcat
              ["DisplayName" @= _oDisplayName, "ID" @= _oId]

-- | The container element for a bucket's ownership controls.
--
--
--
-- /See:/ 'ownershipControls' smart constructor.
newtype OwnershipControls = OwnershipControls'
  { _ocRules :: [OwnershipControlsRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OwnershipControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocRules' - The container element for an ownership control rule.
ownershipControls
    :: OwnershipControls
ownershipControls = OwnershipControls' {_ocRules = mempty}


-- | The container element for an ownership control rule.
ocRules :: Lens' OwnershipControls [OwnershipControlsRule]
ocRules = lens _ocRules (\ s a -> s{_ocRules = a}) . _Coerce

instance FromXML OwnershipControls where
        parseXML x
          = OwnershipControls' <$> (parseXMLList "Rule" x)

instance Hashable OwnershipControls where

instance NFData OwnershipControls where

instance ToXML OwnershipControls where
        toXML OwnershipControls'{..}
          = mconcat [toXMLList "Rule" _ocRules]

-- | The container element for an ownership control rule.
--
--
--
-- /See:/ 'ownershipControlsRule' smart constructor.
newtype OwnershipControlsRule = OwnershipControlsRule'
  { _ocrObjectOwnership :: ObjectOwnership
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OwnershipControlsRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrObjectOwnership' - Undocumented member.
ownershipControlsRule
    :: ObjectOwnership -- ^ 'ocrObjectOwnership'
    -> OwnershipControlsRule
ownershipControlsRule pObjectOwnership_ =
  OwnershipControlsRule' {_ocrObjectOwnership = pObjectOwnership_}


-- | Undocumented member.
ocrObjectOwnership :: Lens' OwnershipControlsRule ObjectOwnership
ocrObjectOwnership = lens _ocrObjectOwnership (\ s a -> s{_ocrObjectOwnership = a})

instance FromXML OwnershipControlsRule where
        parseXML x
          = OwnershipControlsRule' <$> (x .@ "ObjectOwnership")

instance Hashable OwnershipControlsRule where

instance NFData OwnershipControlsRule where

instance ToXML OwnershipControlsRule where
        toXML OwnershipControlsRule'{..}
          = mconcat ["ObjectOwnership" @= _ocrObjectOwnership]

-- | Container for Parquet.
--
--
--
-- /See:/ 'parquetInput' smart constructor.
data ParquetInput =
  ParquetInput'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParquetInput' with the minimum fields required to make a request.
--
parquetInput
    :: ParquetInput
parquetInput = ParquetInput'


instance Hashable ParquetInput where

instance NFData ParquetInput where

instance ToXML ParquetInput where
        toXML = const mempty

-- | Container for elements related to a part.
--
--
--
-- /See:/ 'part' smart constructor.
data Part = Part'
  { _pETag         :: !(Maybe ETag)
  , _pSize         :: !(Maybe Int)
  , _pPartNumber   :: !(Maybe Int)
  , _pLastModified :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Part' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pETag' - Entity tag returned when the part was uploaded.
--
-- * 'pSize' - Size in bytes of the uploaded part data.
--
-- * 'pPartNumber' - Part number identifying the part. This is a positive integer between 1 and 10,000.
--
-- * 'pLastModified' - Date and time at which the part was uploaded.
part
    :: Part
part =
  Part'
    { _pETag = Nothing
    , _pSize = Nothing
    , _pPartNumber = Nothing
    , _pLastModified = Nothing
    }


-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe ETag)
pETag = lens _pETag (\ s a -> s{_pETag = a})

-- | Size in bytes of the uploaded part data.
pSize :: Lens' Part (Maybe Int)
pSize = lens _pSize (\ s a -> s{_pSize = a})

-- | Part number identifying the part. This is a positive integer between 1 and 10,000.
pPartNumber :: Lens' Part (Maybe Int)
pPartNumber = lens _pPartNumber (\ s a -> s{_pPartNumber = a})

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe UTCTime)
pLastModified = lens _pLastModified (\ s a -> s{_pLastModified = a}) . mapping _Time

instance FromXML Part where
        parseXML x
          = Part' <$>
              (x .@? "ETag") <*> (x .@? "Size") <*>
                (x .@? "PartNumber")
                <*> (x .@? "LastModified")

instance Hashable Part where

instance NFData Part where

-- | The container element for a bucket's policy status.
--
--
--
-- /See:/ 'policyStatus' smart constructor.
newtype PolicyStatus = PolicyStatus'
  { _psIsPublic :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psIsPublic' - The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
policyStatus
    :: PolicyStatus
policyStatus = PolicyStatus' {_psIsPublic = Nothing}


-- | The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
psIsPublic :: Lens' PolicyStatus (Maybe Bool)
psIsPublic = lens _psIsPublic (\ s a -> s{_psIsPublic = a})

instance FromXML PolicyStatus where
        parseXML x = PolicyStatus' <$> (x .@? "IsPublic")

instance Hashable PolicyStatus where

instance NFData PolicyStatus where

-- | This data type contains information about progress of an operation.
--
--
--
-- /See:/ 'progress' smart constructor.
data Progress = Progress'
  { _pBytesReturned  :: !(Maybe Integer)
  , _pBytesScanned   :: !(Maybe Integer)
  , _pBytesProcessed :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Progress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pBytesReturned' - The current number of bytes of records payload data returned.
--
-- * 'pBytesScanned' - The current number of object bytes scanned.
--
-- * 'pBytesProcessed' - The current number of uncompressed object bytes processed.
progress
    :: Progress
progress =
  Progress'
    { _pBytesReturned = Nothing
    , _pBytesScanned = Nothing
    , _pBytesProcessed = Nothing
    }


-- | The current number of bytes of records payload data returned.
pBytesReturned :: Lens' Progress (Maybe Integer)
pBytesReturned = lens _pBytesReturned (\ s a -> s{_pBytesReturned = a})

-- | The current number of object bytes scanned.
pBytesScanned :: Lens' Progress (Maybe Integer)
pBytesScanned = lens _pBytesScanned (\ s a -> s{_pBytesScanned = a})

-- | The current number of uncompressed object bytes processed.
pBytesProcessed :: Lens' Progress (Maybe Integer)
pBytesProcessed = lens _pBytesProcessed (\ s a -> s{_pBytesProcessed = a})

instance FromXML Progress where
        parseXML x
          = Progress' <$>
              (x .@? "BytesReturned") <*> (x .@? "BytesScanned")
                <*> (x .@? "BytesProcessed")

instance Hashable Progress where

instance NFData Progress where

-- | This data type contains information about the progress event of an operation.
--
--
--
-- /See:/ 'progressEvent' smart constructor.
newtype ProgressEvent = ProgressEvent'
  { _peDetails :: Maybe Progress
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProgressEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peDetails' - The Progress event details.
progressEvent
    :: ProgressEvent
progressEvent = ProgressEvent' {_peDetails = Nothing}


-- | The Progress event details.
peDetails :: Lens' ProgressEvent (Maybe Progress)
peDetails = lens _peDetails (\ s a -> s{_peDetails = a})

instance FromXML ProgressEvent where
        parseXML x = ProgressEvent' <$> (x .@? "Details")

instance Hashable ProgressEvent where

instance NFData ProgressEvent where

-- | The PublicAccessBlock configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'publicAccessBlockConfiguration' smart constructor.
data PublicAccessBlockConfiguration = PublicAccessBlockConfiguration'
  { _pabcIgnorePublicACLs      :: !(Maybe Bool)
  , _pabcBlockPublicACLs       :: !(Maybe Bool)
  , _pabcRestrictPublicBuckets :: !(Maybe Bool)
  , _pabcBlockPublicPolicy     :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicAccessBlockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pabcIgnorePublicACLs' - Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket. Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
--
-- * 'pabcBlockPublicACLs' - Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.     * PUT Object calls fail if the request includes a public ACL.     * PUT Bucket calls fail if the request includes a public ACL. Enabling this setting doesn't affect existing policies or ACLs.
--
-- * 'pabcRestrictPublicBuckets' - Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy. Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
--
-- * 'pabcBlockPublicPolicy' - Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.  Enabling this setting doesn't affect existing bucket policies.
publicAccessBlockConfiguration
    :: PublicAccessBlockConfiguration
publicAccessBlockConfiguration =
  PublicAccessBlockConfiguration'
    { _pabcIgnorePublicACLs = Nothing
    , _pabcBlockPublicACLs = Nothing
    , _pabcRestrictPublicBuckets = Nothing
    , _pabcBlockPublicPolicy = Nothing
    }


-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket and objects in this bucket. Setting this element to @TRUE@ causes Amazon S3 to ignore all public ACLs on this bucket and objects in this bucket. Enabling this setting doesn't affect the persistence of any existing ACLs and doesn't prevent new public ACLs from being set.
pabcIgnorePublicACLs :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcIgnorePublicACLs = lens _pabcIgnorePublicACLs (\ s a -> s{_pabcIgnorePublicACLs = a})

-- | Specifies whether Amazon S3 should block public access control lists (ACLs) for this bucket and objects in this bucket. Setting this element to @TRUE@ causes the following behavior:     * PUT Bucket acl and PUT Object acl calls fail if the specified ACL is public.     * PUT Object calls fail if the request includes a public ACL.     * PUT Bucket calls fail if the request includes a public ACL. Enabling this setting doesn't affect existing policies or ACLs.
pabcBlockPublicACLs :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcBlockPublicACLs = lens _pabcBlockPublicACLs (\ s a -> s{_pabcBlockPublicACLs = a})

-- | Specifies whether Amazon S3 should restrict public bucket policies for this bucket. Setting this element to @TRUE@ restricts access to this bucket to only AWS service principals and authorized users within this account if the bucket has a public policy. Enabling this setting doesn't affect previously stored bucket policies, except that public and cross-account access within any public bucket policy, including non-public delegation to specific accounts, is blocked.
pabcRestrictPublicBuckets :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcRestrictPublicBuckets = lens _pabcRestrictPublicBuckets (\ s a -> s{_pabcRestrictPublicBuckets = a})

-- | Specifies whether Amazon S3 should block public bucket policies for this bucket. Setting this element to @TRUE@ causes Amazon S3 to reject calls to PUT Bucket policy if the specified bucket policy allows public access.  Enabling this setting doesn't affect existing bucket policies.
pabcBlockPublicPolicy :: Lens' PublicAccessBlockConfiguration (Maybe Bool)
pabcBlockPublicPolicy = lens _pabcBlockPublicPolicy (\ s a -> s{_pabcBlockPublicPolicy = a})

instance FromXML PublicAccessBlockConfiguration where
        parseXML x
          = PublicAccessBlockConfiguration' <$>
              (x .@? "IgnorePublicAcls") <*>
                (x .@? "BlockPublicAcls")
                <*> (x .@? "RestrictPublicBuckets")
                <*> (x .@? "BlockPublicPolicy")

instance Hashable PublicAccessBlockConfiguration
         where

instance NFData PublicAccessBlockConfiguration where

instance ToXML PublicAccessBlockConfiguration where
        toXML PublicAccessBlockConfiguration'{..}
          = mconcat
              ["IgnorePublicAcls" @= _pabcIgnorePublicACLs,
               "BlockPublicAcls" @= _pabcBlockPublicACLs,
               "RestrictPublicBuckets" @=
                 _pabcRestrictPublicBuckets,
               "BlockPublicPolicy" @= _pabcBlockPublicPolicy]

-- | Specifies the configuration for publishing messages to an Amazon Simple Queue Service (Amazon SQS) queue when Amazon S3 detects specified events.
--
--
--
-- /See:/ 'queueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { _qcId       :: !(Maybe Text)
  , _qcFilter   :: !(Maybe NotificationConfigurationFilter)
  , _qcQueueARN :: !Text
  , _qcEvents   :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueueConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qcId' - Undocumented member.
--
-- * 'qcFilter' - Undocumented member.
--
-- * 'qcQueueARN' - The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- * 'qcEvents' - A collection of bucket events for which to send notifications
queueConfiguration
    :: Text -- ^ 'qcQueueARN'
    -> QueueConfiguration
queueConfiguration pQueueARN_ =
  QueueConfiguration'
    { _qcId = Nothing
    , _qcFilter = Nothing
    , _qcQueueARN = pQueueARN_
    , _qcEvents = mempty
    }


-- | Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\ s a -> s{_qcId = a})

-- | Undocumented member.
qcFilter :: Lens' QueueConfiguration (Maybe NotificationConfigurationFilter)
qcFilter = lens _qcFilter (\ s a -> s{_qcFilter = a})

-- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
qcQueueARN :: Lens' QueueConfiguration Text
qcQueueARN = lens _qcQueueARN (\ s a -> s{_qcQueueARN = a})

-- | A collection of bucket events for which to send notifications
qcEvents :: Lens' QueueConfiguration [Event]
qcEvents = lens _qcEvents (\ s a -> s{_qcEvents = a}) . _Coerce

instance FromXML QueueConfiguration where
        parseXML x
          = QueueConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Queue")
                <*> (parseXMLList "Event" x)

instance Hashable QueueConfiguration where

instance NFData QueueConfiguration where

instance ToXML QueueConfiguration where
        toXML QueueConfiguration'{..}
          = mconcat
              ["Id" @= _qcId, "Filter" @= _qcFilter,
               "Queue" @= _qcQueueARN, toXMLList "Event" _qcEvents]

-- | The container for the records event.
--
--
--
-- /See:/ 'recordsEvent' smart constructor.
newtype RecordsEvent = RecordsEvent'
  { _rePayload :: Maybe Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rePayload' - The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
recordsEvent
    :: RecordsEvent
recordsEvent = RecordsEvent' {_rePayload = Nothing}


-- | The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rePayload :: Lens' RecordsEvent (Maybe ByteString)
rePayload = lens _rePayload (\ s a -> s{_rePayload = a}) . mapping _Base64

instance FromXML RecordsEvent where
        parseXML x = RecordsEvent' <$> (x .@? "Payload")

instance Hashable RecordsEvent where

instance NFData RecordsEvent where

-- | Specifies how requests are redirected. In the event of an error, you can specify a different error code to return.
--
--
--
-- /See:/ 'redirect' smart constructor.
data Redirect = Redirect'
  { _rHostName             :: !(Maybe Text)
  , _rProtocol             :: !(Maybe Protocol)
  , _rHTTPRedirectCode     :: !(Maybe Text)
  , _rReplaceKeyWith       :: !(Maybe Text)
  , _rReplaceKeyPrefixWith :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Redirect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHostName' - The host name to use in the redirect request.
--
-- * 'rProtocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rHTTPRedirectCode' - The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- * 'rReplaceKeyWith' - The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- * 'rReplaceKeyPrefixWith' - The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
redirect
    :: Redirect
redirect =
  Redirect'
    { _rHostName = Nothing
    , _rProtocol = Nothing
    , _rHTTPRedirectCode = Nothing
    , _rReplaceKeyWith = Nothing
    , _rReplaceKeyPrefixWith = Nothing
    }


-- | The host name to use in the redirect request.
rHostName :: Lens' Redirect (Maybe Text)
rHostName = lens _rHostName (\ s a -> s{_rHostName = a})

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
rProtocol :: Lens' Redirect (Maybe Protocol)
rProtocol = lens _rProtocol (\ s a -> s{_rProtocol = a})

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
rHTTPRedirectCode :: Lens' Redirect (Maybe Text)
rHTTPRedirectCode = lens _rHTTPRedirectCode (\ s a -> s{_rHTTPRedirectCode = a})

-- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
rReplaceKeyWith :: Lens' Redirect (Maybe Text)
rReplaceKeyWith = lens _rReplaceKeyWith (\ s a -> s{_rReplaceKeyWith = a})

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
rReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
rReplaceKeyPrefixWith = lens _rReplaceKeyPrefixWith (\ s a -> s{_rReplaceKeyPrefixWith = a})

instance FromXML Redirect where
        parseXML x
          = Redirect' <$>
              (x .@? "HostName") <*> (x .@? "Protocol") <*>
                (x .@? "HttpRedirectCode")
                <*> (x .@? "ReplaceKeyWith")
                <*> (x .@? "ReplaceKeyPrefixWith")

instance Hashable Redirect where

instance NFData Redirect where

instance ToXML Redirect where
        toXML Redirect'{..}
          = mconcat
              ["HostName" @= _rHostName, "Protocol" @= _rProtocol,
               "HttpRedirectCode" @= _rHTTPRedirectCode,
               "ReplaceKeyWith" @= _rReplaceKeyWith,
               "ReplaceKeyPrefixWith" @= _rReplaceKeyPrefixWith]

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
--
--
-- /See:/ 'redirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { _rartProtocol :: !(Maybe Protocol)
  , _rartHostName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedirectAllRequestsTo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rartProtocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rartHostName' - Name of the host where requests are redirected.
redirectAllRequestsTo
    :: Text -- ^ 'rartHostName'
    -> RedirectAllRequestsTo
redirectAllRequestsTo pHostName_ =
  RedirectAllRequestsTo' {_rartProtocol = Nothing, _rartHostName = pHostName_}


-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\ s a -> s{_rartProtocol = a})

-- | Name of the host where requests are redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\ s a -> s{_rartHostName = a})

instance FromXML RedirectAllRequestsTo where
        parseXML x
          = RedirectAllRequestsTo' <$>
              (x .@? "Protocol") <*> (x .@ "HostName")

instance Hashable RedirectAllRequestsTo where

instance NFData RedirectAllRequestsTo where

instance ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo'{..}
          = mconcat
              ["Protocol" @= _rartProtocol,
               "HostName" @= _rartHostName]

-- | A filter that you can specify for selection for modifications on replicas. Amazon S3 doesn't replicate replica modifications by default. In the latest version of replication configuration (when @Filter@ is specified), you can specify this element and set the status to @Enabled@ to replicate modifications on replicas.
--
--
--
-- /See:/ 'replicaModifications' smart constructor.
newtype ReplicaModifications = ReplicaModifications'
  { _rmStatus :: ReplicaModificationsStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmStatus' - Specifies whether Amazon S3 replicates modifications on replicas.
replicaModifications
    :: ReplicaModificationsStatus -- ^ 'rmStatus'
    -> ReplicaModifications
replicaModifications pStatus_ = ReplicaModifications' {_rmStatus = pStatus_}


-- | Specifies whether Amazon S3 replicates modifications on replicas.
rmStatus :: Lens' ReplicaModifications ReplicaModificationsStatus
rmStatus = lens _rmStatus (\ s a -> s{_rmStatus = a})

instance FromXML ReplicaModifications where
        parseXML x
          = ReplicaModifications' <$> (x .@ "Status")

instance Hashable ReplicaModifications where

instance NFData ReplicaModifications where

instance ToXML ReplicaModifications where
        toXML ReplicaModifications'{..}
          = mconcat ["Status" @= _rmStatus]

-- | A container for replication rules. You can add up to 1,000 rules. The maximum size of a replication configuration is 2 MB.
--
--
--
-- /See:/ 'replicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { _rcRole  :: !Text
  , _rcRules :: ![ReplicationRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'rcRules' - A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules.
replicationConfiguration
    :: Text -- ^ 'rcRole'
    -> ReplicationConfiguration
replicationConfiguration pRole_ =
  ReplicationConfiguration' {_rcRole = pRole_, _rcRules = mempty}


-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
rcRole :: Lens' ReplicationConfiguration Text
rcRole = lens _rcRole (\ s a -> s{_rcRole = a})

-- | A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules.
rcRules :: Lens' ReplicationConfiguration [ReplicationRule]
rcRules = lens _rcRules (\ s a -> s{_rcRules = a}) . _Coerce

instance FromXML ReplicationConfiguration where
        parseXML x
          = ReplicationConfiguration' <$>
              (x .@ "Role") <*> (parseXMLList "Rule" x)

instance Hashable ReplicationConfiguration where

instance NFData ReplicationConfiguration where

instance ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration'{..}
          = mconcat
              ["Role" @= _rcRole, toXMLList "Rule" _rcRules]

-- | Specifies which Amazon S3 objects to replicate and where to store the replicas.
--
--
--
-- /See:/ 'replicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { _rrDeleteMarkerReplication   :: !(Maybe DeleteMarkerReplication)
  , _rrPriority                  :: !(Maybe Int)
  , _rrPrefix                    :: !(Maybe Text)
  , _rrExistingObjectReplication :: !(Maybe ExistingObjectReplication)
  , _rrId                        :: !(Maybe Text)
  , _rrFilter                    :: !(Maybe ReplicationRuleFilter)
  , _rrSourceSelectionCriteria   :: !(Maybe SourceSelectionCriteria)
  , _rrStatus                    :: !ReplicationRuleStatus
  , _rrDestination               :: !Destination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDeleteMarkerReplication' - Undocumented member.
--
-- * 'rrPriority' - The priority indicates which rule has precedence whenever two or more replication rules conflict. Amazon S3 will attempt to replicate objects according to all replication rules. However, if there are two or more rules with the same destination bucket, then objects will be replicated according to the rule with the highest priority. The higher the number, the higher the priority.  For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'rrPrefix' - An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
--
-- * 'rrExistingObjectReplication' -
--
-- * 'rrId' - A unique identifier for the rule. The maximum value is 255 characters.
--
-- * 'rrFilter' - Undocumented member.
--
-- * 'rrSourceSelectionCriteria' - A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- * 'rrStatus' - Specifies whether the rule is enabled.
--
-- * 'rrDestination' - A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
replicationRule
    :: ReplicationRuleStatus -- ^ 'rrStatus'
    -> Destination -- ^ 'rrDestination'
    -> ReplicationRule
replicationRule pStatus_ pDestination_ =
  ReplicationRule'
    { _rrDeleteMarkerReplication = Nothing
    , _rrPriority = Nothing
    , _rrPrefix = Nothing
    , _rrExistingObjectReplication = Nothing
    , _rrId = Nothing
    , _rrFilter = Nothing
    , _rrSourceSelectionCriteria = Nothing
    , _rrStatus = pStatus_
    , _rrDestination = pDestination_
    }


-- | Undocumented member.
rrDeleteMarkerReplication :: Lens' ReplicationRule (Maybe DeleteMarkerReplication)
rrDeleteMarkerReplication = lens _rrDeleteMarkerReplication (\ s a -> s{_rrDeleteMarkerReplication = a})

-- | The priority indicates which rule has precedence whenever two or more replication rules conflict. Amazon S3 will attempt to replicate objects according to all replication rules. However, if there are two or more rules with the same destination bucket, then objects will be replicated according to the rule with the highest priority. The higher the number, the higher the priority.  For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
rrPriority :: Lens' ReplicationRule (Maybe Int)
rrPriority = lens _rrPriority (\ s a -> s{_rrPriority = a})

-- | An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
rrPrefix :: Lens' ReplicationRule (Maybe Text)
rrPrefix = lens _rrPrefix (\ s a -> s{_rrPrefix = a})

-- |
rrExistingObjectReplication :: Lens' ReplicationRule (Maybe ExistingObjectReplication)
rrExistingObjectReplication = lens _rrExistingObjectReplication (\ s a -> s{_rrExistingObjectReplication = a})

-- | A unique identifier for the rule. The maximum value is 255 characters.
rrId :: Lens' ReplicationRule (Maybe Text)
rrId = lens _rrId (\ s a -> s{_rrId = a})

-- | Undocumented member.
rrFilter :: Lens' ReplicationRule (Maybe ReplicationRuleFilter)
rrFilter = lens _rrFilter (\ s a -> s{_rrFilter = a})

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
rrSourceSelectionCriteria :: Lens' ReplicationRule (Maybe SourceSelectionCriteria)
rrSourceSelectionCriteria = lens _rrSourceSelectionCriteria (\ s a -> s{_rrSourceSelectionCriteria = a})

-- | Specifies whether the rule is enabled.
rrStatus :: Lens' ReplicationRule ReplicationRuleStatus
rrStatus = lens _rrStatus (\ s a -> s{_rrStatus = a})

-- | A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
rrDestination :: Lens' ReplicationRule Destination
rrDestination = lens _rrDestination (\ s a -> s{_rrDestination = a})

instance FromXML ReplicationRule where
        parseXML x
          = ReplicationRule' <$>
              (x .@? "DeleteMarkerReplication") <*>
                (x .@? "Priority")
                <*> (x .@? "Prefix")
                <*> (x .@? "ExistingObjectReplication")
                <*> (x .@? "ID")
                <*> (x .@? "Filter")
                <*> (x .@? "SourceSelectionCriteria")
                <*> (x .@ "Status")
                <*> (x .@ "Destination")

instance Hashable ReplicationRule where

instance NFData ReplicationRule where

instance ToXML ReplicationRule where
        toXML ReplicationRule'{..}
          = mconcat
              ["DeleteMarkerReplication" @=
                 _rrDeleteMarkerReplication,
               "Priority" @= _rrPriority, "Prefix" @= _rrPrefix,
               "ExistingObjectReplication" @=
                 _rrExistingObjectReplication,
               "ID" @= _rrId, "Filter" @= _rrFilter,
               "SourceSelectionCriteria" @=
                 _rrSourceSelectionCriteria,
               "Status" @= _rrStatus,
               "Destination" @= _rrDestination]

-- | A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter.
--
--
-- For example:
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag
--
--
--
--
-- /See:/ 'replicationRuleAndOperator' smart constructor.
data ReplicationRuleAndOperator = ReplicationRuleAndOperator'
  { _rraoPrefix :: !(Maybe Text)
  , _rraoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRuleAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rraoPrefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- * 'rraoTags' - An array of tags containing key and value pairs.
replicationRuleAndOperator
    :: ReplicationRuleAndOperator
replicationRuleAndOperator =
  ReplicationRuleAndOperator' {_rraoPrefix = Nothing, _rraoTags = Nothing}


-- | An object key name prefix that identifies the subset of objects to which the rule applies.
rraoPrefix :: Lens' ReplicationRuleAndOperator (Maybe Text)
rraoPrefix = lens _rraoPrefix (\ s a -> s{_rraoPrefix = a})

-- | An array of tags containing key and value pairs.
rraoTags :: Lens' ReplicationRuleAndOperator [Tag]
rraoTags = lens _rraoTags (\ s a -> s{_rraoTags = a}) . _Default . _Coerce

instance FromXML ReplicationRuleAndOperator where
        parseXML x
          = ReplicationRuleAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable ReplicationRuleAndOperator where

instance NFData ReplicationRuleAndOperator where

instance ToXML ReplicationRuleAndOperator where
        toXML ReplicationRuleAndOperator'{..}
          = mconcat
              ["Prefix" @= _rraoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _rraoTags)]

-- | A filter that identifies the subset of objects to which the replication rule applies. A @Filter@ must specify exactly one @Prefix@ , @Tag@ , or an @And@ child element.
--
--
--
-- /See:/ 'replicationRuleFilter' smart constructor.
data ReplicationRuleFilter = ReplicationRuleFilter'
  { _rrfTag    :: !(Maybe Tag)
  , _rrfPrefix :: !(Maybe Text)
  , _rrfAnd    :: !(Maybe ReplicationRuleAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRuleFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrfTag' - A container for specifying a tag key and value.  The rule applies only to objects that have the tag in their tag set.
--
-- * 'rrfPrefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- * 'rrfAnd' - A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter. For example:      * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag.
replicationRuleFilter
    :: ReplicationRuleFilter
replicationRuleFilter =
  ReplicationRuleFilter'
    {_rrfTag = Nothing, _rrfPrefix = Nothing, _rrfAnd = Nothing}


-- | A container for specifying a tag key and value.  The rule applies only to objects that have the tag in their tag set.
rrfTag :: Lens' ReplicationRuleFilter (Maybe Tag)
rrfTag = lens _rrfTag (\ s a -> s{_rrfTag = a})

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
rrfPrefix :: Lens' ReplicationRuleFilter (Maybe Text)
rrfPrefix = lens _rrfPrefix (\ s a -> s{_rrfPrefix = a})

-- | A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter. For example:      * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag.
rrfAnd :: Lens' ReplicationRuleFilter (Maybe ReplicationRuleAndOperator)
rrfAnd = lens _rrfAnd (\ s a -> s{_rrfAnd = a})

instance FromXML ReplicationRuleFilter where
        parseXML x
          = ReplicationRuleFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable ReplicationRuleFilter where

instance NFData ReplicationRuleFilter where

instance ToXML ReplicationRuleFilter where
        toXML ReplicationRuleFilter'{..}
          = mconcat
              ["Tag" @= _rrfTag, "Prefix" @= _rrfPrefix,
               "And" @= _rrfAnd]

-- | A container specifying S3 Replication Time Control (S3 RTC) related information, including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
--
--
-- /See:/ 'replicationTime' smart constructor.
data ReplicationTime = ReplicationTime'
  { _rtStatus :: !ReplicationTimeStatus
  , _rtTime   :: !ReplicationTimeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtStatus' - Specifies whether the replication time is enabled.
--
-- * 'rtTime' - A container specifying the time by which replication should be complete for all objects and operations on objects.
replicationTime
    :: ReplicationTimeStatus -- ^ 'rtStatus'
    -> ReplicationTimeValue -- ^ 'rtTime'
    -> ReplicationTime
replicationTime pStatus_ pTime_ =
  ReplicationTime' {_rtStatus = pStatus_, _rtTime = pTime_}


-- | Specifies whether the replication time is enabled.
rtStatus :: Lens' ReplicationTime ReplicationTimeStatus
rtStatus = lens _rtStatus (\ s a -> s{_rtStatus = a})

-- | A container specifying the time by which replication should be complete for all objects and operations on objects.
rtTime :: Lens' ReplicationTime ReplicationTimeValue
rtTime = lens _rtTime (\ s a -> s{_rtTime = a})

instance FromXML ReplicationTime where
        parseXML x
          = ReplicationTime' <$>
              (x .@ "Status") <*> (x .@ "Time")

instance Hashable ReplicationTime where

instance NFData ReplicationTime where

instance ToXML ReplicationTime where
        toXML ReplicationTime'{..}
          = mconcat ["Status" @= _rtStatus, "Time" @= _rtTime]

-- | A container specifying the time value for S3 Replication Time Control (S3 RTC) and replication metrics @EventThreshold@ .
--
--
--
-- /See:/ 'replicationTimeValue' smart constructor.
newtype ReplicationTimeValue = ReplicationTimeValue'
  { _rtvMinutes :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationTimeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtvMinutes' - Contains an integer specifying time in minutes.  Valid values: 15 minutes.
replicationTimeValue
    :: ReplicationTimeValue
replicationTimeValue = ReplicationTimeValue' {_rtvMinutes = Nothing}


-- | Contains an integer specifying time in minutes.  Valid values: 15 minutes.
rtvMinutes :: Lens' ReplicationTimeValue (Maybe Int)
rtvMinutes = lens _rtvMinutes (\ s a -> s{_rtvMinutes = a})

instance FromXML ReplicationTimeValue where
        parseXML x
          = ReplicationTimeValue' <$> (x .@? "Minutes")

instance Hashable ReplicationTimeValue where

instance NFData ReplicationTimeValue where

instance ToXML ReplicationTimeValue where
        toXML ReplicationTimeValue'{..}
          = mconcat ["Minutes" @= _rtvMinutes]

-- | Container for Payer.
--
--
--
-- /See:/ 'requestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
  { _rpcPayer :: Payer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestPaymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcPayer' - Specifies who pays for the download and request fees.
requestPaymentConfiguration
    :: Payer -- ^ 'rpcPayer'
    -> RequestPaymentConfiguration
requestPaymentConfiguration pPayer_ =
  RequestPaymentConfiguration' {_rpcPayer = pPayer_}


-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\ s a -> s{_rpcPayer = a})

instance Hashable RequestPaymentConfiguration where

instance NFData RequestPaymentConfiguration where

instance ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration'{..}
          = mconcat ["Payer" @= _rpcPayer]

-- | Container for specifying if periodic @QueryProgress@ messages should be sent.
--
--
--
-- /See:/ 'requestProgress' smart constructor.
newtype RequestProgress = RequestProgress'
  { _rpEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestProgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpEnabled' - Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
requestProgress
    :: RequestProgress
requestProgress = RequestProgress' {_rpEnabled = Nothing}


-- | Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
rpEnabled :: Lens' RequestProgress (Maybe Bool)
rpEnabled = lens _rpEnabled (\ s a -> s{_rpEnabled = a})

instance Hashable RequestProgress where

instance NFData RequestProgress where

instance ToXML RequestProgress where
        toXML RequestProgress'{..}
          = mconcat ["Enabled" @= _rpEnabled]

-- | Container for restore job parameters.
--
--
--
-- /See:/ 'restoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { _rrDays                 :: !(Maybe Int)
  , _rrSelectParameters     :: !(Maybe SelectParameters)
  , _rrOutputLocation       :: !(Maybe OutputLocation)
  , _rrTier                 :: !(Maybe Tier)
  , _rrGlacierJobParameters :: !(Maybe GlacierJobParameters)
  , _rrType                 :: !(Maybe RestoreRequestType)
  , _rrDescription          :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDays' - Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ . The Days element is required for regular restores, and must not be provided for select requests.
--
-- * 'rrSelectParameters' - Describes the parameters for Select job types.
--
-- * 'rrOutputLocation' - Describes the location where the restore job's output is stored.
--
-- * 'rrTier' - Retrieval tier at which the restore will be processed.
--
-- * 'rrGlacierJobParameters' - S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
--
-- * 'rrType' - Type of restore request.
--
-- * 'rrDescription' - The optional description for the job.
restoreRequest
    :: RestoreRequest
restoreRequest =
  RestoreRequest'
    { _rrDays = Nothing
    , _rrSelectParameters = Nothing
    , _rrOutputLocation = Nothing
    , _rrTier = Nothing
    , _rrGlacierJobParameters = Nothing
    , _rrType = Nothing
    , _rrDescription = Nothing
    }


-- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ . The Days element is required for regular restores, and must not be provided for select requests.
rrDays :: Lens' RestoreRequest (Maybe Int)
rrDays = lens _rrDays (\ s a -> s{_rrDays = a})

-- | Describes the parameters for Select job types.
rrSelectParameters :: Lens' RestoreRequest (Maybe SelectParameters)
rrSelectParameters = lens _rrSelectParameters (\ s a -> s{_rrSelectParameters = a})

-- | Describes the location where the restore job's output is stored.
rrOutputLocation :: Lens' RestoreRequest (Maybe OutputLocation)
rrOutputLocation = lens _rrOutputLocation (\ s a -> s{_rrOutputLocation = a})

-- | Retrieval tier at which the restore will be processed.
rrTier :: Lens' RestoreRequest (Maybe Tier)
rrTier = lens _rrTier (\ s a -> s{_rrTier = a})

-- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
rrGlacierJobParameters :: Lens' RestoreRequest (Maybe GlacierJobParameters)
rrGlacierJobParameters = lens _rrGlacierJobParameters (\ s a -> s{_rrGlacierJobParameters = a})

-- | Type of restore request.
rrType :: Lens' RestoreRequest (Maybe RestoreRequestType)
rrType = lens _rrType (\ s a -> s{_rrType = a})

-- | The optional description for the job.
rrDescription :: Lens' RestoreRequest (Maybe Text)
rrDescription = lens _rrDescription (\ s a -> s{_rrDescription = a})

instance Hashable RestoreRequest where

instance NFData RestoreRequest where

instance ToXML RestoreRequest where
        toXML RestoreRequest'{..}
          = mconcat
              ["Days" @= _rrDays,
               "SelectParameters" @= _rrSelectParameters,
               "OutputLocation" @= _rrOutputLocation,
               "Tier" @= _rrTier,
               "GlacierJobParameters" @= _rrGlacierJobParameters,
               "Type" @= _rrType, "Description" @= _rrDescription]

-- | Specifies the redirect behavior and when a redirect is applied. For more information about routing rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'routingRule' smart constructor.
data RoutingRule = RoutingRule'
  { _rrCondition :: !(Maybe Condition)
  , _rrRedirect  :: !Redirect
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoutingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrCondition' - A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- * 'rrRedirect' - Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
routingRule
    :: Redirect -- ^ 'rrRedirect'
    -> RoutingRule
routingRule pRedirect_ =
  RoutingRule' {_rrCondition = Nothing, _rrRedirect = pRedirect_}


-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\ s a -> s{_rrCondition = a})

-- | Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\ s a -> s{_rrRedirect = a})

instance FromXML RoutingRule where
        parseXML x
          = RoutingRule' <$>
              (x .@? "Condition") <*> (x .@ "Redirect")

instance Hashable RoutingRule where

instance NFData RoutingRule where

instance ToXML RoutingRule where
        toXML RoutingRule'{..}
          = mconcat
              ["Condition" @= _rrCondition,
               "Redirect" @= _rrRedirect]

-- | A container for object key name prefix and suffix filtering rules.
--
--
--
-- /See:/ 's3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
  { _skfFilterRules :: Maybe [FilterRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3KeyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skfFilterRules' - Undocumented member.
s3KeyFilter
    :: S3KeyFilter
s3KeyFilter = S3KeyFilter' {_skfFilterRules = Nothing}


-- | Undocumented member.
skfFilterRules :: Lens' S3KeyFilter [FilterRule]
skfFilterRules = lens _skfFilterRules (\ s a -> s{_skfFilterRules = a}) . _Default . _Coerce

instance FromXML S3KeyFilter where
        parseXML x
          = S3KeyFilter' <$>
              (may (parseXMLList "FilterRule") x)

instance Hashable S3KeyFilter where

instance NFData S3KeyFilter where

instance ToXML S3KeyFilter where
        toXML S3KeyFilter'{..}
          = mconcat
              [toXML (toXMLList "FilterRule" <$> _skfFilterRules)]

-- | Describes an Amazon S3 location that will receive the results of the restore request.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slCannedACL         :: !(Maybe ObjectCannedACL)
  , _slAccessControlList :: !(Maybe [Grant])
  , _slUserMetadata      :: !(Maybe [MetadataEntry])
  , _slEncryption        :: !(Maybe Encryption)
  , _slStorageClass      :: !(Maybe StorageClass)
  , _slTagging           :: !(Maybe Tagging)
  , _slBucketName        :: !BucketName
  , _slPrefix            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCannedACL' - The canned ACL to apply to the restore results.
--
-- * 'slAccessControlList' - A list of grants that control access to the staged results.
--
-- * 'slUserMetadata' - A list of metadata to store with the restore results in S3.
--
-- * 'slEncryption' - Undocumented member.
--
-- * 'slStorageClass' - The class of storage used to store the restore results.
--
-- * 'slTagging' - The tag-set that is applied to the restore results.
--
-- * 'slBucketName' - The name of the bucket where the restore results will be placed.
--
-- * 'slPrefix' - The prefix that is prepended to the restore results for this request.
s3Location
    :: BucketName -- ^ 'slBucketName'
    -> Text -- ^ 'slPrefix'
    -> S3Location
s3Location pBucketName_ pPrefix_ =
  S3Location'
    { _slCannedACL = Nothing
    , _slAccessControlList = Nothing
    , _slUserMetadata = Nothing
    , _slEncryption = Nothing
    , _slStorageClass = Nothing
    , _slTagging = Nothing
    , _slBucketName = pBucketName_
    , _slPrefix = pPrefix_
    }


-- | The canned ACL to apply to the restore results.
slCannedACL :: Lens' S3Location (Maybe ObjectCannedACL)
slCannedACL = lens _slCannedACL (\ s a -> s{_slCannedACL = a})

-- | A list of grants that control access to the staged results.
slAccessControlList :: Lens' S3Location [Grant]
slAccessControlList = lens _slAccessControlList (\ s a -> s{_slAccessControlList = a}) . _Default . _Coerce

-- | A list of metadata to store with the restore results in S3.
slUserMetadata :: Lens' S3Location [MetadataEntry]
slUserMetadata = lens _slUserMetadata (\ s a -> s{_slUserMetadata = a}) . _Default . _Coerce

-- | Undocumented member.
slEncryption :: Lens' S3Location (Maybe Encryption)
slEncryption = lens _slEncryption (\ s a -> s{_slEncryption = a})

-- | The class of storage used to store the restore results.
slStorageClass :: Lens' S3Location (Maybe StorageClass)
slStorageClass = lens _slStorageClass (\ s a -> s{_slStorageClass = a})

-- | The tag-set that is applied to the restore results.
slTagging :: Lens' S3Location (Maybe Tagging)
slTagging = lens _slTagging (\ s a -> s{_slTagging = a})

-- | The name of the bucket where the restore results will be placed.
slBucketName :: Lens' S3Location BucketName
slBucketName = lens _slBucketName (\ s a -> s{_slBucketName = a})

-- | The prefix that is prepended to the restore results for this request.
slPrefix :: Lens' S3Location Text
slPrefix = lens _slPrefix (\ s a -> s{_slPrefix = a})

instance Hashable S3Location where

instance NFData S3Location where

instance ToXML S3Location where
        toXML S3Location'{..}
          = mconcat
              ["CannedACL" @= _slCannedACL,
               "AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _slAccessControlList),
               "UserMetadata" @=
                 toXML
                   (toXMLList "MetadataEntry" <$> _slUserMetadata),
               "Encryption" @= _slEncryption,
               "StorageClass" @= _slStorageClass,
               "Tagging" @= _slTagging,
               "BucketName" @= _slBucketName, "Prefix" @= _slPrefix]

-- | Container for all error elements.
--
--
--
-- /See:/ 's3ServiceError' smart constructor.
data S3ServiceError = S3ServiceError'
  { _sseVersionId :: !(Maybe ObjectVersionId)
  , _sseKey       :: !(Maybe ObjectKey)
  , _sseCode      :: !(Maybe Text)
  , _sseMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ServiceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseVersionId' - The version ID of the error.
--
-- * 'sseKey' - The error key.
--
-- * 'sseCode' - The error code is a string that uniquely identifies an error condition. It is meant to be read and understood by programs that detect and handle errors by type.  __Amazon S3 error codes__      *     * /Code:/ AccessDenied      * /Description:/ Access Denied     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AccountProblem     * /Description:/ There is a problem with your AWS account that prevents the operation from completing successfully. Contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AllAccessDisabled     * /Description:/ All access to this Amazon S3 resource has been disabled. Contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AmbiguousGrantByEmailAddress     * /Description:/ The email address you provided is associated with more than one account.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AuthorizationHeaderMalformed     * /Description:/ The authorization header you provided is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /HTTP Status Code:/ N/A     *     * /Code:/ BadDigest     * /Description:/ The Content-MD5 you specified did not match what we received.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketAlreadyExists     * /Description:/ The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketAlreadyOwnedByYou     * /Description:/ The bucket you tried to create already exists, and you own it. Amazon S3 returns this error in all AWS Regions except in the North Virginia Region. For legacy compatibility, if you re-create an existing bucket that you already own in the North Virginia Region, Amazon S3 returns 200 OK and resets the bucket access control lists (ACLs).     * /Code:/ 409 Conflict (in all Regions except the North Virginia Region)      * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketNotEmpty     * /Description:/ The bucket you tried to delete is not empty.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ CredentialsNotSupported     * /Description:/ This request does not support credentials.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ CrossLocationLoggingProhibited     * /Description:/ Cross-location logging not allowed. Buckets in one geographic location cannot log information to a bucket in another location.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ EntityTooSmall     * /Description:/ Your proposed upload is smaller than the minimum allowed object size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ EntityTooLarge     * /Description:/ Your proposed upload exceeds the maximum allowed object size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ ExpiredToken     * /Description:/ The provided token has expired.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IllegalVersioningConfigurationException      * /Description:/ Indicates that the versioning configuration specified in the request is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IncompleteBody     * /Description:/ You did not provide the number of bytes specified by the Content-Length HTTP header     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IncorrectNumberOfFilesInPostRequest     * /Description:/ POST requires exactly one file upload per request.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InlineDataTooLarge     * /Description:/ Inline data exceeds the maximum allowed size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InternalError     * /Description:/ We encountered an internal error. Please try again.     * /HTTP Status Code:/ 500 Internal Server Error     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ InvalidAccessKeyId     * /Description:/ The AWS access key ID you provided does not exist in our records.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidAddressingHeader     * /Description:/ You must specify the Anonymous role.     * /HTTP Status Code:/ N/A     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidArgument     * /Description:/ Invalid Argument     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidBucketName     * /Description:/ The specified bucket is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidBucketState     * /Description:/ The request is not valid with the current state of the bucket.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidDigest     * /Description:/ The Content-MD5 you specified is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidEncryptionAlgorithmError     * /Description:/ The encryption request you specified is not valid. The valid value is AES256.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidLocationConstraint     * /Description:/ The specified location constraint is not valid. For more information about Regions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro How to Select a Region for Your Buckets> .      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidObjectState     * /Description:/ The operation is not valid for the current state of the object.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPart     * /Description:/ One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPartOrder     * /Description:/ The list of parts was not in ascending order. Parts list must be specified in order by part number.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPayer     * /Description:/ All access to this object has been disabled. Please contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPolicyDocument     * /Description:/ The content of the form does not meet the conditions specified in the policy document.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRange     * /Description:/ The requested range cannot be satisfied.     * /HTTP Status Code:/ 416 Requested Range Not Satisfiable     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRequest     * /Description:/ Please use AWS4-HMAC-SHA256.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ SOAP requests must be made over an HTTPS connection.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported for buckets with non-DNS compliant names.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported for buckets with periods (.) in their names.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate endpoint only supports virtual style requests.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate is not configured on this bucket.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate is disabled on this bucket.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported on this bucket. Contact AWS Support for more information.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration cannot be enabled on this bucket. Contact AWS Support for more information.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidSecurity     * /Description:/ The provided security credentials are not valid.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidSOAPRequest     * /Description:/ The SOAP request body is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidStorageClass     * /Description:/ The storage class you specified is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidTargetBucketForLogging     * /Description:/ The target bucket for logging does not exist, is not owned by you, or does not have the appropriate grants for the log-delivery group.      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidToken     * /Description:/ The provided token is malformed or otherwise invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidURI     * /Description:/ Couldn't parse the specified URI.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ KeyTooLongError     * /Description:/ Your key is too long.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedACLError     * /Description:/ The XML you provided was not well-formed or did not validate against our published schema.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedPOSTRequest      * /Description:/ The body of your POST request is not well-formed multipart/form-data.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedXML     * /Description:/ This happens when the user sends malformed XML (XML that doesn't conform to the published XSD) for the configuration. The error message is, "The XML you provided was not well-formed or did not validate against our published schema."      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MaxMessageLengthExceeded     * /Description:/ Your request was too big.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MaxPostPreDataLengthExceededError     * /Description:/ Your POST request fields preceding the upload file were too large.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MetadataTooLarge     * /Description:/ Your metadata headers exceed the maximum allowed metadata size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MethodNotAllowed     * /Description:/ The specified method is not allowed against this resource.     * /HTTP Status Code:/ 405 Method Not Allowed     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingAttachment     * /Description:/ A SOAP attachment was expected, but none were found.     * /HTTP Status Code:/ N/A     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingContentLength     * /Description:/ You must provide the Content-Length HTTP header.     * /HTTP Status Code:/ 411 Length Required     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingRequestBodyError     * /Description:/ This happens when the user sends an empty XML document as a request. The error message is, "Request body is empty."      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingSecurityElement     * /Description:/ The SOAP 1.1 request is missing a security element.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingSecurityHeader     * /Description:/ Your request is missing a required header.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoLoggingStatusForKey     * /Description:/ There is no such thing as a logging status subresource for a key.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchBucket     * /Description:/ The specified bucket does not exist.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchBucketPolicy     * /Description:/ The specified bucket does not have a bucket policy.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchKey     * /Description:/ The specified key does not exist.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchLifecycleConfiguration     * /Description:/ The lifecycle configuration does not exist.      * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchUpload     * /Description:/ The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchVersion      * /Description:/ Indicates that the version ID specified in the request does not match an existing version.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NotImplemented     * /Description:/ A header you provided implies functionality that is not implemented.     * /HTTP Status Code:/ 501 Not Implemented     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ NotSignedUp     * /Description:/ Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: https://aws.amazon.com/s3     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ OperationAborted     * /Description:/ A conflicting conditional operation is currently in progress against this resource. Try again.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ PermanentRedirect     * /Description:/ The bucket you are attempting to access must be addressed using the specified endpoint. Send all future requests to this endpoint.     * /HTTP Status Code:/ 301 Moved Permanently     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ PreconditionFailed     * /Description:/ At least one of the preconditions you specified did not hold.     * /HTTP Status Code:/ 412 Precondition Failed     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ Redirect     * /Description:/ Temporary redirect.     * /HTTP Status Code:/ 307 Moved Temporarily     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RestoreAlreadyInProgress     * /Description:/ Object restore is already in progress.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestIsNotMultiPartContent     * /Description:/ Bucket POST must be of the enclosure-type multipart/form-data.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTimeout     * /Description:/ Your socket connection to the server was not read from or written to within the timeout period.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTimeTooSkewed     * /Description:/ The difference between the request time and the server's time is too large.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTorrentOfBucketError     * /Description:/ Requesting the torrent file of a bucket is not permitted.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ SignatureDoesNotMatch     * /Description:/ The request signature we calculated does not match the signature you provided. Check your AWS secret access key and signing method. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/SOAPAuthentication.html SOAP Authentication> for details.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ ServiceUnavailable     * /Description:/ Reduce your request rate.     * /HTTP Status Code:/ 503 Service Unavailable     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ SlowDown     * /Description:/ Reduce your request rate.     * /HTTP Status Code:/ 503 Slow Down     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ TemporaryRedirect     * /Description:/ You are being redirected to the bucket while DNS updates.     * /HTTP Status Code:/ 307 Moved Temporarily     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ TokenRefreshRequired     * /Description:/ The provided token must be refreshed.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ TooManyBuckets     * /Description:/ You have attempted to create more buckets than allowed.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UnexpectedContent     * /Description:/ This request does not support content.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UnresolvableGrantByEmailAddress     * /Description:/ The email address you provided does not match any account on record.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UserKeyMustBeSpecified     * /Description:/ The bucket POST must contain the specified field name. If it is specified, check the order of the fields.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client
--
-- * 'sseMessage' - The error message contains a generic description of the error condition in English. It is intended for a human audience. Simple programs display the message directly to the end user if they encounter an error condition they don't know how or don't care to handle. Sophisticated programs with more exhaustive error handling and proper internationalization are more likely to ignore the error message.
s3ServiceError
    :: S3ServiceError
s3ServiceError =
  S3ServiceError'
    { _sseVersionId = Nothing
    , _sseKey = Nothing
    , _sseCode = Nothing
    , _sseMessage = Nothing
    }


-- | The version ID of the error.
sseVersionId :: Lens' S3ServiceError (Maybe ObjectVersionId)
sseVersionId = lens _sseVersionId (\ s a -> s{_sseVersionId = a})

-- | The error key.
sseKey :: Lens' S3ServiceError (Maybe ObjectKey)
sseKey = lens _sseKey (\ s a -> s{_sseKey = a})

-- | The error code is a string that uniquely identifies an error condition. It is meant to be read and understood by programs that detect and handle errors by type.  __Amazon S3 error codes__      *     * /Code:/ AccessDenied      * /Description:/ Access Denied     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AccountProblem     * /Description:/ There is a problem with your AWS account that prevents the operation from completing successfully. Contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AllAccessDisabled     * /Description:/ All access to this Amazon S3 resource has been disabled. Contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AmbiguousGrantByEmailAddress     * /Description:/ The email address you provided is associated with more than one account.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ AuthorizationHeaderMalformed     * /Description:/ The authorization header you provided is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /HTTP Status Code:/ N/A     *     * /Code:/ BadDigest     * /Description:/ The Content-MD5 you specified did not match what we received.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketAlreadyExists     * /Description:/ The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketAlreadyOwnedByYou     * /Description:/ The bucket you tried to create already exists, and you own it. Amazon S3 returns this error in all AWS Regions except in the North Virginia Region. For legacy compatibility, if you re-create an existing bucket that you already own in the North Virginia Region, Amazon S3 returns 200 OK and resets the bucket access control lists (ACLs).     * /Code:/ 409 Conflict (in all Regions except the North Virginia Region)      * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ BucketNotEmpty     * /Description:/ The bucket you tried to delete is not empty.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ CredentialsNotSupported     * /Description:/ This request does not support credentials.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ CrossLocationLoggingProhibited     * /Description:/ Cross-location logging not allowed. Buckets in one geographic location cannot log information to a bucket in another location.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ EntityTooSmall     * /Description:/ Your proposed upload is smaller than the minimum allowed object size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ EntityTooLarge     * /Description:/ Your proposed upload exceeds the maximum allowed object size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ ExpiredToken     * /Description:/ The provided token has expired.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IllegalVersioningConfigurationException      * /Description:/ Indicates that the versioning configuration specified in the request is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IncompleteBody     * /Description:/ You did not provide the number of bytes specified by the Content-Length HTTP header     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ IncorrectNumberOfFilesInPostRequest     * /Description:/ POST requires exactly one file upload per request.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InlineDataTooLarge     * /Description:/ Inline data exceeds the maximum allowed size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InternalError     * /Description:/ We encountered an internal error. Please try again.     * /HTTP Status Code:/ 500 Internal Server Error     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ InvalidAccessKeyId     * /Description:/ The AWS access key ID you provided does not exist in our records.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidAddressingHeader     * /Description:/ You must specify the Anonymous role.     * /HTTP Status Code:/ N/A     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidArgument     * /Description:/ Invalid Argument     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidBucketName     * /Description:/ The specified bucket is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidBucketState     * /Description:/ The request is not valid with the current state of the bucket.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidDigest     * /Description:/ The Content-MD5 you specified is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidEncryptionAlgorithmError     * /Description:/ The encryption request you specified is not valid. The valid value is AES256.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidLocationConstraint     * /Description:/ The specified location constraint is not valid. For more information about Regions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro How to Select a Region for Your Buckets> .      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidObjectState     * /Description:/ The operation is not valid for the current state of the object.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPart     * /Description:/ One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPartOrder     * /Description:/ The list of parts was not in ascending order. Parts list must be specified in order by part number.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPayer     * /Description:/ All access to this object has been disabled. Please contact AWS Support for further assistance.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidPolicyDocument     * /Description:/ The content of the form does not meet the conditions specified in the policy document.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRange     * /Description:/ The requested range cannot be satisfied.     * /HTTP Status Code:/ 416 Requested Range Not Satisfiable     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRequest     * /Description:/ Please use AWS4-HMAC-SHA256.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ SOAP requests must be made over an HTTPS connection.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported for buckets with non-DNS compliant names.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported for buckets with periods (.) in their names.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate endpoint only supports virtual style requests.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate is not configured on this bucket.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Accelerate is disabled on this bucket.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration is not supported on this bucket. Contact AWS Support for more information.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidRequest     * /Description:/ Amazon S3 Transfer Acceleration cannot be enabled on this bucket. Contact AWS Support for more information.     * /HTTP Status Code:/ 400 Bad Request     * /Code:/ N/A     *     * /Code:/ InvalidSecurity     * /Description:/ The provided security credentials are not valid.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidSOAPRequest     * /Description:/ The SOAP request body is invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidStorageClass     * /Description:/ The storage class you specified is not valid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidTargetBucketForLogging     * /Description:/ The target bucket for logging does not exist, is not owned by you, or does not have the appropriate grants for the log-delivery group.      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidToken     * /Description:/ The provided token is malformed or otherwise invalid.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ InvalidURI     * /Description:/ Couldn't parse the specified URI.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ KeyTooLongError     * /Description:/ Your key is too long.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedACLError     * /Description:/ The XML you provided was not well-formed or did not validate against our published schema.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedPOSTRequest      * /Description:/ The body of your POST request is not well-formed multipart/form-data.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MalformedXML     * /Description:/ This happens when the user sends malformed XML (XML that doesn't conform to the published XSD) for the configuration. The error message is, "The XML you provided was not well-formed or did not validate against our published schema."      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MaxMessageLengthExceeded     * /Description:/ Your request was too big.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MaxPostPreDataLengthExceededError     * /Description:/ Your POST request fields preceding the upload file were too large.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MetadataTooLarge     * /Description:/ Your metadata headers exceed the maximum allowed metadata size.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MethodNotAllowed     * /Description:/ The specified method is not allowed against this resource.     * /HTTP Status Code:/ 405 Method Not Allowed     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingAttachment     * /Description:/ A SOAP attachment was expected, but none were found.     * /HTTP Status Code:/ N/A     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingContentLength     * /Description:/ You must provide the Content-Length HTTP header.     * /HTTP Status Code:/ 411 Length Required     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingRequestBodyError     * /Description:/ This happens when the user sends an empty XML document as a request. The error message is, "Request body is empty."      * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingSecurityElement     * /Description:/ The SOAP 1.1 request is missing a security element.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ MissingSecurityHeader     * /Description:/ Your request is missing a required header.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoLoggingStatusForKey     * /Description:/ There is no such thing as a logging status subresource for a key.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchBucket     * /Description:/ The specified bucket does not exist.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchBucketPolicy     * /Description:/ The specified bucket does not have a bucket policy.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchKey     * /Description:/ The specified key does not exist.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchLifecycleConfiguration     * /Description:/ The lifecycle configuration does not exist.      * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchUpload     * /Description:/ The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NoSuchVersion      * /Description:/ Indicates that the version ID specified in the request does not match an existing version.     * /HTTP Status Code:/ 404 Not Found     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ NotImplemented     * /Description:/ A header you provided implies functionality that is not implemented.     * /HTTP Status Code:/ 501 Not Implemented     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ NotSignedUp     * /Description:/ Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: https://aws.amazon.com/s3     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ OperationAborted     * /Description:/ A conflicting conditional operation is currently in progress against this resource. Try again.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ PermanentRedirect     * /Description:/ The bucket you are attempting to access must be addressed using the specified endpoint. Send all future requests to this endpoint.     * /HTTP Status Code:/ 301 Moved Permanently     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ PreconditionFailed     * /Description:/ At least one of the preconditions you specified did not hold.     * /HTTP Status Code:/ 412 Precondition Failed     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ Redirect     * /Description:/ Temporary redirect.     * /HTTP Status Code:/ 307 Moved Temporarily     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RestoreAlreadyInProgress     * /Description:/ Object restore is already in progress.     * /HTTP Status Code:/ 409 Conflict     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestIsNotMultiPartContent     * /Description:/ Bucket POST must be of the enclosure-type multipart/form-data.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTimeout     * /Description:/ Your socket connection to the server was not read from or written to within the timeout period.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTimeTooSkewed     * /Description:/ The difference between the request time and the server's time is too large.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ RequestTorrentOfBucketError     * /Description:/ Requesting the torrent file of a bucket is not permitted.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ SignatureDoesNotMatch     * /Description:/ The request signature we calculated does not match the signature you provided. Check your AWS secret access key and signing method. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/SOAPAuthentication.html SOAP Authentication> for details.     * /HTTP Status Code:/ 403 Forbidden     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ ServiceUnavailable     * /Description:/ Reduce your request rate.     * /HTTP Status Code:/ 503 Service Unavailable     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ SlowDown     * /Description:/ Reduce your request rate.     * /HTTP Status Code:/ 503 Slow Down     * /SOAP Fault Code Prefix:/ Server     *     * /Code:/ TemporaryRedirect     * /Description:/ You are being redirected to the bucket while DNS updates.     * /HTTP Status Code:/ 307 Moved Temporarily     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ TokenRefreshRequired     * /Description:/ The provided token must be refreshed.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ TooManyBuckets     * /Description:/ You have attempted to create more buckets than allowed.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UnexpectedContent     * /Description:/ This request does not support content.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UnresolvableGrantByEmailAddress     * /Description:/ The email address you provided does not match any account on record.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client     *     * /Code:/ UserKeyMustBeSpecified     * /Description:/ The bucket POST must contain the specified field name. If it is specified, check the order of the fields.     * /HTTP Status Code:/ 400 Bad Request     * /SOAP Fault Code Prefix:/ Client
sseCode :: Lens' S3ServiceError (Maybe Text)
sseCode = lens _sseCode (\ s a -> s{_sseCode = a})

-- | The error message contains a generic description of the error condition in English. It is intended for a human audience. Simple programs display the message directly to the end user if they encounter an error condition they don't know how or don't care to handle. Sophisticated programs with more exhaustive error handling and proper internationalization are more likely to ignore the error message.
sseMessage :: Lens' S3ServiceError (Maybe Text)
sseMessage = lens _sseMessage (\ s a -> s{_sseMessage = a})

instance FromXML S3ServiceError where
        parseXML x
          = S3ServiceError' <$>
              (x .@? "VersionId") <*> (x .@? "Key") <*>
                (x .@? "Code")
                <*> (x .@? "Message")

instance Hashable S3ServiceError where

instance NFData S3ServiceError where

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
--
--
-- /See:/ 'sSEKMS' smart constructor.
newtype SSEKMS = SSEKMS'
  { _ssekKeyId :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSEKMS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssekKeyId' - Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
sSEKMS
    :: Text -- ^ 'ssekKeyId'
    -> SSEKMS
sSEKMS pKeyId_ = SSEKMS' {_ssekKeyId = _Sensitive # pKeyId_}


-- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
ssekKeyId :: Lens' SSEKMS Text
ssekKeyId = lens _ssekKeyId (\ s a -> s{_ssekKeyId = a}) . _Sensitive

instance FromXML SSEKMS where
        parseXML x = SSEKMS' <$> (x .@ "KeyId")

instance Hashable SSEKMS where

instance NFData SSEKMS where

instance ToXML SSEKMS where
        toXML SSEKMS'{..} = mconcat ["KeyId" @= _ssekKeyId]

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
--
--
-- /See:/ 'sSES3' smart constructor.
data SSES3 =
  SSES3'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSES3' with the minimum fields required to make a request.
--
sSES3
    :: SSES3
sSES3 = SSES3'


instance FromXML SSES3 where
        parseXML = const (pure SSES3')

instance Hashable SSES3 where

instance NFData SSES3 where

instance ToXML SSES3 where
        toXML = const mempty

-- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
--
--
--
-- /See:/ 'scanRange' smart constructor.
data ScanRange = ScanRange'
  { _srStart :: !(Maybe Integer)
  , _srEnd   :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScanRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStart' - Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
--
-- * 'srEnd' - Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
scanRange
    :: ScanRange
scanRange = ScanRange' {_srStart = Nothing, _srEnd = Nothing}


-- | Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
srStart :: Lens' ScanRange (Maybe Integer)
srStart = lens _srStart (\ s a -> s{_srStart = a})

-- | Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
srEnd :: Lens' ScanRange (Maybe Integer)
srEnd = lens _srEnd (\ s a -> s{_srEnd = a})

instance Hashable ScanRange where

instance NFData ScanRange where

instance ToXML ScanRange where
        toXML ScanRange'{..}
          = mconcat ["Start" @= _srStart, "End" @= _srEnd]

-- | The container for selecting objects from a content event stream.
--
--
--
-- /See:/ 'selectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { _socesProgress :: !(Maybe ProgressEvent)
  , _socesRecords  :: !(Maybe RecordsEvent)
  , _socesCont     :: !(Maybe ContinuationEvent)
  , _socesStats    :: !(Maybe StatsEvent)
  , _socesEnd      :: !(Maybe EndEvent)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectObjectContentEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socesProgress' - The Progress Event.
--
-- * 'socesRecords' - The Records Event.
--
-- * 'socesCont' - The Continuation Event.
--
-- * 'socesStats' - The Stats Event.
--
-- * 'socesEnd' - The End Event.
selectObjectContentEventStream
    :: SelectObjectContentEventStream
selectObjectContentEventStream =
  SelectObjectContentEventStream'
    { _socesProgress = Nothing
    , _socesRecords = Nothing
    , _socesCont = Nothing
    , _socesStats = Nothing
    , _socesEnd = Nothing
    }


-- | The Progress Event.
socesProgress :: Lens' SelectObjectContentEventStream (Maybe ProgressEvent)
socesProgress = lens _socesProgress (\ s a -> s{_socesProgress = a})

-- | The Records Event.
socesRecords :: Lens' SelectObjectContentEventStream (Maybe RecordsEvent)
socesRecords = lens _socesRecords (\ s a -> s{_socesRecords = a})

-- | The Continuation Event.
socesCont :: Lens' SelectObjectContentEventStream (Maybe ContinuationEvent)
socesCont = lens _socesCont (\ s a -> s{_socesCont = a})

-- | The Stats Event.
socesStats :: Lens' SelectObjectContentEventStream (Maybe StatsEvent)
socesStats = lens _socesStats (\ s a -> s{_socesStats = a})

-- | The End Event.
socesEnd :: Lens' SelectObjectContentEventStream (Maybe EndEvent)
socesEnd = lens _socesEnd (\ s a -> s{_socesEnd = a})

instance FromXML SelectObjectContentEventStream where
        parseXML x
          = SelectObjectContentEventStream' <$>
              (x .@? "Progress") <*> (x .@? "Records") <*>
                (x .@? "Cont")
                <*> (x .@? "Stats")
                <*> (x .@? "End")

instance Hashable SelectObjectContentEventStream
         where

instance NFData SelectObjectContentEventStream where

-- | Describes the parameters for Select job types.
--
--
--
-- /See:/ 'selectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { _spInputSerialization  :: !InputSerialization
  , _spExpressionType      :: !ExpressionType
  , _spExpression          :: !Text
  , _spOutputSerialization :: !OutputSerialization
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spInputSerialization' - Describes the serialization format of the object.
--
-- * 'spExpressionType' - The type of the provided expression (for example, SQL).
--
-- * 'spExpression' - The expression that is used to query the object.
--
-- * 'spOutputSerialization' - Describes how the results of the Select job are serialized.
selectParameters
    :: InputSerialization -- ^ 'spInputSerialization'
    -> ExpressionType -- ^ 'spExpressionType'
    -> Text -- ^ 'spExpression'
    -> OutputSerialization -- ^ 'spOutputSerialization'
    -> SelectParameters
selectParameters pInputSerialization_ pExpressionType_ pExpression_ pOutputSerialization_ =
  SelectParameters'
    { _spInputSerialization = pInputSerialization_
    , _spExpressionType = pExpressionType_
    , _spExpression = pExpression_
    , _spOutputSerialization = pOutputSerialization_
    }


-- | Describes the serialization format of the object.
spInputSerialization :: Lens' SelectParameters InputSerialization
spInputSerialization = lens _spInputSerialization (\ s a -> s{_spInputSerialization = a})

-- | The type of the provided expression (for example, SQL).
spExpressionType :: Lens' SelectParameters ExpressionType
spExpressionType = lens _spExpressionType (\ s a -> s{_spExpressionType = a})

-- | The expression that is used to query the object.
spExpression :: Lens' SelectParameters Text
spExpression = lens _spExpression (\ s a -> s{_spExpression = a})

-- | Describes how the results of the Select job are serialized.
spOutputSerialization :: Lens' SelectParameters OutputSerialization
spOutputSerialization = lens _spOutputSerialization (\ s a -> s{_spOutputSerialization = a})

instance Hashable SelectParameters where

instance NFData SelectParameters where

instance ToXML SelectParameters where
        toXML SelectParameters'{..}
          = mconcat
              ["InputSerialization" @= _spInputSerialization,
               "ExpressionType" @= _spExpressionType,
               "Expression" @= _spExpression,
               "OutputSerialization" @= _spOutputSerialization]

-- | Describes the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html PUT Bucket encryption> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'serverSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'
  { _ssebdKMSMasterKeyId :: !(Maybe (Sensitive Text))
  , _ssebdSSEAlgorithm   :: !ServerSideEncryption
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssebdKMSMasterKeyId' - AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ . You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .  __For example:__      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'ssebdSSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
serverSideEncryptionByDefault
    :: ServerSideEncryption -- ^ 'ssebdSSEAlgorithm'
    -> ServerSideEncryptionByDefault
serverSideEncryptionByDefault pSSEAlgorithm_ =
  ServerSideEncryptionByDefault'
    {_ssebdKMSMasterKeyId = Nothing, _ssebdSSEAlgorithm = pSSEAlgorithm_}


-- | AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ . You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .  __For example:__      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
ssebdKMSMasterKeyId :: Lens' ServerSideEncryptionByDefault (Maybe Text)
ssebdKMSMasterKeyId = lens _ssebdKMSMasterKeyId (\ s a -> s{_ssebdKMSMasterKeyId = a}) . mapping _Sensitive

-- | Server-side encryption algorithm to use for the default encryption.
ssebdSSEAlgorithm :: Lens' ServerSideEncryptionByDefault ServerSideEncryption
ssebdSSEAlgorithm = lens _ssebdSSEAlgorithm (\ s a -> s{_ssebdSSEAlgorithm = a})

instance FromXML ServerSideEncryptionByDefault where
        parseXML x
          = ServerSideEncryptionByDefault' <$>
              (x .@? "KMSMasterKeyID") <*> (x .@ "SSEAlgorithm")

instance Hashable ServerSideEncryptionByDefault where

instance NFData ServerSideEncryptionByDefault where

instance ToXML ServerSideEncryptionByDefault where
        toXML ServerSideEncryptionByDefault'{..}
          = mconcat
              ["KMSMasterKeyID" @= _ssebdKMSMasterKeyId,
               "SSEAlgorithm" @= _ssebdSSEAlgorithm]

-- | Specifies the default server-side-encryption configuration.
--
--
--
-- /See:/ 'serverSideEncryptionConfiguration' smart constructor.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { _ssecRules :: [ServerSideEncryptionRule]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssecRules' - Container for information about a particular server-side encryption configuration rule.
serverSideEncryptionConfiguration
    :: ServerSideEncryptionConfiguration
serverSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration' {_ssecRules = mempty}


-- | Container for information about a particular server-side encryption configuration rule.
ssecRules :: Lens' ServerSideEncryptionConfiguration [ServerSideEncryptionRule]
ssecRules = lens _ssecRules (\ s a -> s{_ssecRules = a}) . _Coerce

instance FromXML ServerSideEncryptionConfiguration
         where
        parseXML x
          = ServerSideEncryptionConfiguration' <$>
              (parseXMLList "Rule" x)

instance Hashable ServerSideEncryptionConfiguration
         where

instance NFData ServerSideEncryptionConfiguration
         where

instance ToXML ServerSideEncryptionConfiguration
         where
        toXML ServerSideEncryptionConfiguration'{..}
          = mconcat [toXMLList "Rule" _ssecRules]

-- | Specifies the default server-side encryption configuration.
--
--
--
-- /See:/ 'serverSideEncryptionRule' smart constructor.
data ServerSideEncryptionRule = ServerSideEncryptionRule'
  { _sserApplyServerSideEncryptionByDefault :: !(Maybe ServerSideEncryptionByDefault)
  , _sserBucketKeyEnabled :: !(Maybe Bool)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sserApplyServerSideEncryptionByDefault' - Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
--
-- * 'sserBucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key with server-side encryption using KMS (SSE-KMS) for new objects in the bucket. Existing objects are not affected. Setting the @BucketKeyEnabled@ element to @true@ causes Amazon S3 to use an S3 Bucket Key. By default, S3 Bucket Key is not enabled. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys> in the /Amazon Simple Storage Service Developer Guide/ .
serverSideEncryptionRule
    :: ServerSideEncryptionRule
serverSideEncryptionRule =
  ServerSideEncryptionRule'
    { _sserApplyServerSideEncryptionByDefault = Nothing
    , _sserBucketKeyEnabled = Nothing
    }


-- | Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
sserApplyServerSideEncryptionByDefault :: Lens' ServerSideEncryptionRule (Maybe ServerSideEncryptionByDefault)
sserApplyServerSideEncryptionByDefault = lens _sserApplyServerSideEncryptionByDefault (\ s a -> s{_sserApplyServerSideEncryptionByDefault = a})

-- | Specifies whether Amazon S3 should use an S3 Bucket Key with server-side encryption using KMS (SSE-KMS) for new objects in the bucket. Existing objects are not affected. Setting the @BucketKeyEnabled@ element to @true@ causes Amazon S3 to use an S3 Bucket Key. By default, S3 Bucket Key is not enabled. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys> in the /Amazon Simple Storage Service Developer Guide/ .
sserBucketKeyEnabled :: Lens' ServerSideEncryptionRule (Maybe Bool)
sserBucketKeyEnabled = lens _sserBucketKeyEnabled (\ s a -> s{_sserBucketKeyEnabled = a})

instance FromXML ServerSideEncryptionRule where
        parseXML x
          = ServerSideEncryptionRule' <$>
              (x .@? "ApplyServerSideEncryptionByDefault") <*>
                (x .@? "BucketKeyEnabled")

instance Hashable ServerSideEncryptionRule where

instance NFData ServerSideEncryptionRule where

instance ToXML ServerSideEncryptionRule where
        toXML ServerSideEncryptionRule'{..}
          = mconcat
              ["ApplyServerSideEncryptionByDefault" @=
                 _sserApplyServerSideEncryptionByDefault,
               "BucketKeyEnabled" @= _sserBucketKeyEnabled]

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
--
--
-- /See:/ 'sourceSelectionCriteria' smart constructor.
data SourceSelectionCriteria = SourceSelectionCriteria'
  { _sscReplicaModifications   :: !(Maybe ReplicaModifications)
  , _sscSseKMSEncryptedObjects :: !(Maybe SseKMSEncryptedObjects)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceSelectionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscReplicaModifications' - A filter that you can specify for selections for modifications on replicas. Amazon S3 doesn't replicate replica modifications by default. In the latest version of replication configuration (when @Filter@ is specified), you can specify this element and set the status to @Enabled@ to replicate modifications on replicas.
--
-- * 'sscSseKMSEncryptedObjects' - A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
sourceSelectionCriteria
    :: SourceSelectionCriteria
sourceSelectionCriteria =
  SourceSelectionCriteria'
    {_sscReplicaModifications = Nothing, _sscSseKMSEncryptedObjects = Nothing}


-- | A filter that you can specify for selections for modifications on replicas. Amazon S3 doesn't replicate replica modifications by default. In the latest version of replication configuration (when @Filter@ is specified), you can specify this element and set the status to @Enabled@ to replicate modifications on replicas.
sscReplicaModifications :: Lens' SourceSelectionCriteria (Maybe ReplicaModifications)
sscReplicaModifications = lens _sscReplicaModifications (\ s a -> s{_sscReplicaModifications = a})

-- | A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
sscSseKMSEncryptedObjects :: Lens' SourceSelectionCriteria (Maybe SseKMSEncryptedObjects)
sscSseKMSEncryptedObjects = lens _sscSseKMSEncryptedObjects (\ s a -> s{_sscSseKMSEncryptedObjects = a})

instance FromXML SourceSelectionCriteria where
        parseXML x
          = SourceSelectionCriteria' <$>
              (x .@? "ReplicaModifications") <*>
                (x .@? "SseKmsEncryptedObjects")

instance Hashable SourceSelectionCriteria where

instance NFData SourceSelectionCriteria where

instance ToXML SourceSelectionCriteria where
        toXML SourceSelectionCriteria'{..}
          = mconcat
              ["ReplicaModifications" @= _sscReplicaModifications,
               "SseKmsEncryptedObjects" @=
                 _sscSseKMSEncryptedObjects]

-- | A container for filter information for the selection of S3 objects encrypted with AWS KMS.
--
--
--
-- /See:/ 'sseKMSEncryptedObjects' smart constructor.
newtype SseKMSEncryptedObjects = SseKMSEncryptedObjects'
  { _skeoStatus :: SseKMSEncryptedObjectsStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SseKMSEncryptedObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skeoStatus' - Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
sseKMSEncryptedObjects
    :: SseKMSEncryptedObjectsStatus -- ^ 'skeoStatus'
    -> SseKMSEncryptedObjects
sseKMSEncryptedObjects pStatus_ =
  SseKMSEncryptedObjects' {_skeoStatus = pStatus_}


-- | Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
skeoStatus :: Lens' SseKMSEncryptedObjects SseKMSEncryptedObjectsStatus
skeoStatus = lens _skeoStatus (\ s a -> s{_skeoStatus = a})

instance FromXML SseKMSEncryptedObjects where
        parseXML x
          = SseKMSEncryptedObjects' <$> (x .@ "Status")

instance Hashable SseKMSEncryptedObjects where

instance NFData SseKMSEncryptedObjects where

instance ToXML SseKMSEncryptedObjects where
        toXML SseKMSEncryptedObjects'{..}
          = mconcat ["Status" @= _skeoStatus]

-- | Container for the stats details.
--
--
--
-- /See:/ 'stats' smart constructor.
data Stats = Stats'
  { _sBytesReturned  :: !(Maybe Integer)
  , _sBytesScanned   :: !(Maybe Integer)
  , _sBytesProcessed :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sBytesReturned' - The total number of bytes of records payload data returned.
--
-- * 'sBytesScanned' - The total number of object bytes scanned.
--
-- * 'sBytesProcessed' - The total number of uncompressed object bytes processed.
stats
    :: Stats
stats =
  Stats'
    { _sBytesReturned = Nothing
    , _sBytesScanned = Nothing
    , _sBytesProcessed = Nothing
    }


-- | The total number of bytes of records payload data returned.
sBytesReturned :: Lens' Stats (Maybe Integer)
sBytesReturned = lens _sBytesReturned (\ s a -> s{_sBytesReturned = a})

-- | The total number of object bytes scanned.
sBytesScanned :: Lens' Stats (Maybe Integer)
sBytesScanned = lens _sBytesScanned (\ s a -> s{_sBytesScanned = a})

-- | The total number of uncompressed object bytes processed.
sBytesProcessed :: Lens' Stats (Maybe Integer)
sBytesProcessed = lens _sBytesProcessed (\ s a -> s{_sBytesProcessed = a})

instance FromXML Stats where
        parseXML x
          = Stats' <$>
              (x .@? "BytesReturned") <*> (x .@? "BytesScanned")
                <*> (x .@? "BytesProcessed")

instance Hashable Stats where

instance NFData Stats where

-- | Container for the Stats Event.
--
--
--
-- /See:/ 'statsEvent' smart constructor.
newtype StatsEvent = StatsEvent'
  { _seDetails :: Maybe Stats
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StatsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seDetails' - The Stats event details.
statsEvent
    :: StatsEvent
statsEvent = StatsEvent' {_seDetails = Nothing}


-- | The Stats event details.
seDetails :: Lens' StatsEvent (Maybe Stats)
seDetails = lens _seDetails (\ s a -> s{_seDetails = a})

instance FromXML StatsEvent where
        parseXML x = StatsEvent' <$> (x .@? "Details")

instance Hashable StatsEvent where

instance NFData StatsEvent where

-- | Specifies data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes for an Amazon S3 bucket.
--
--
--
-- /See:/ 'storageClassAnalysis' smart constructor.
newtype StorageClassAnalysis = StorageClassAnalysis'
  { _scaDataExport :: Maybe StorageClassAnalysisDataExport
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageClassAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scaDataExport' - Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
storageClassAnalysis
    :: StorageClassAnalysis
storageClassAnalysis = StorageClassAnalysis' {_scaDataExport = Nothing}


-- | Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
scaDataExport :: Lens' StorageClassAnalysis (Maybe StorageClassAnalysisDataExport)
scaDataExport = lens _scaDataExport (\ s a -> s{_scaDataExport = a})

instance FromXML StorageClassAnalysis where
        parseXML x
          = StorageClassAnalysis' <$> (x .@? "DataExport")

instance Hashable StorageClassAnalysis where

instance NFData StorageClassAnalysis where

instance ToXML StorageClassAnalysis where
        toXML StorageClassAnalysis'{..}
          = mconcat ["DataExport" @= _scaDataExport]

-- | Container for data related to the storage class analysis for an Amazon S3 bucket for export.
--
--
--
-- /See:/ 'storageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { _scadeOutputSchemaVersion :: !StorageClassAnalysisSchemaVersion
  , _scadeDestination         :: !AnalyticsExportDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageClassAnalysisDataExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scadeOutputSchemaVersion' - The version of the output schema to use when exporting data. Must be @V_1@ .
--
-- * 'scadeDestination' - The place to store the data for an analysis.
storageClassAnalysisDataExport
    :: StorageClassAnalysisSchemaVersion -- ^ 'scadeOutputSchemaVersion'
    -> AnalyticsExportDestination -- ^ 'scadeDestination'
    -> StorageClassAnalysisDataExport
storageClassAnalysisDataExport pOutputSchemaVersion_ pDestination_ =
  StorageClassAnalysisDataExport'
    { _scadeOutputSchemaVersion = pOutputSchemaVersion_
    , _scadeDestination = pDestination_
    }


-- | The version of the output schema to use when exporting data. Must be @V_1@ .
scadeOutputSchemaVersion :: Lens' StorageClassAnalysisDataExport StorageClassAnalysisSchemaVersion
scadeOutputSchemaVersion = lens _scadeOutputSchemaVersion (\ s a -> s{_scadeOutputSchemaVersion = a})

-- | The place to store the data for an analysis.
scadeDestination :: Lens' StorageClassAnalysisDataExport AnalyticsExportDestination
scadeDestination = lens _scadeDestination (\ s a -> s{_scadeDestination = a})

instance FromXML StorageClassAnalysisDataExport where
        parseXML x
          = StorageClassAnalysisDataExport' <$>
              (x .@ "OutputSchemaVersion") <*> (x .@ "Destination")

instance Hashable StorageClassAnalysisDataExport
         where

instance NFData StorageClassAnalysisDataExport where

instance ToXML StorageClassAnalysisDataExport where
        toXML StorageClassAnalysisDataExport'{..}
          = mconcat
              ["OutputSchemaVersion" @= _scadeOutputSchemaVersion,
               "Destination" @= _scadeDestination]

-- | A container of a key value name pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !ObjectKey
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Name of the object key.
--
-- * 'tagValue' - Value of the tag.
tag
    :: ObjectKey -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | Name of the object key.
tagKey :: Lens' Tag ObjectKey
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | Value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag where

instance NFData Tag where

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Key" @= _tagKey, "Value" @= _tagValue]

-- | Container for @TagSet@ elements.
--
--
--
-- /See:/ 'tagging' smart constructor.
newtype Tagging = Tagging'
  { _tTagSet :: [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTagSet' - A collection for a set of tags
tagging
    :: Tagging
tagging = Tagging' {_tTagSet = mempty}


-- | A collection for a set of tags
tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\ s a -> s{_tTagSet = a}) . _Coerce

instance Hashable Tagging where

instance NFData Tagging where

instance ToXML Tagging where
        toXML Tagging'{..}
          = mconcat ["TagSet" @= toXMLList "Tag" _tTagSet]

-- | Container for granting information.
--
--
--
-- /See:/ 'targetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { _tgPermission :: !(Maybe BucketLogsPermission)
  , _tgGrantee    :: !(Maybe Grantee)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgPermission' - Logging permissions assigned to the grantee for the bucket.
--
-- * 'tgGrantee' - Container for the person being granted permissions.
targetGrant
    :: TargetGrant
targetGrant = TargetGrant' {_tgPermission = Nothing, _tgGrantee = Nothing}


-- | Logging permissions assigned to the grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\ s a -> s{_tgPermission = a})

-- | Container for the person being granted permissions.
tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\ s a -> s{_tgGrantee = a})

instance FromXML TargetGrant where
        parseXML x
          = TargetGrant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance Hashable TargetGrant where

instance NFData TargetGrant where

instance ToXML TargetGrant where
        toXML TargetGrant'{..}
          = mconcat
              ["Permission" @= _tgPermission,
               "Grantee" @= _tgGrantee]

-- | The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead.
--
--
--
-- /See:/ 'tiering' smart constructor.
data Tiering = Tiering'
  { _tDays       :: !Int
  , _tAccessTier :: !IntelligentTieringAccessTier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tiering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDays' - The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
--
-- * 'tAccessTier' - S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
tiering
    :: Int -- ^ 'tDays'
    -> IntelligentTieringAccessTier -- ^ 'tAccessTier'
    -> Tiering
tiering pDays_ pAccessTier_ =
  Tiering' {_tDays = pDays_, _tAccessTier = pAccessTier_}


-- | The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
tDays :: Lens' Tiering Int
tDays = lens _tDays (\ s a -> s{_tDays = a})

-- | S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
tAccessTier :: Lens' Tiering IntelligentTieringAccessTier
tAccessTier = lens _tAccessTier (\ s a -> s{_tAccessTier = a})

instance FromXML Tiering where
        parseXML x
          = Tiering' <$> (x .@ "Days") <*> (x .@ "AccessTier")

instance Hashable Tiering where

instance NFData Tiering where

instance ToXML Tiering where
        toXML Tiering'{..}
          = mconcat
              ["Days" @= _tDays, "AccessTier" @= _tAccessTier]

-- | A container for specifying the configuration for publication of messages to an Amazon Simple Notification Service (Amazon SNS) topic when Amazon S3 detects specified events.
--
--
--
-- /See:/ 'topicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { _tcId       :: !(Maybe Text)
  , _tcFilter   :: !(Maybe NotificationConfigurationFilter)
  , _tcTopicARN :: !Text
  , _tcEvents   :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TopicConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcId' - Undocumented member.
--
-- * 'tcFilter' - Undocumented member.
--
-- * 'tcTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- * 'tcEvents' - The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
topicConfiguration
    :: Text -- ^ 'tcTopicARN'
    -> TopicConfiguration
topicConfiguration pTopicARN_ =
  TopicConfiguration'
    { _tcId = Nothing
    , _tcFilter = Nothing
    , _tcTopicARN = pTopicARN_
    , _tcEvents = mempty
    }


-- | Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\ s a -> s{_tcId = a})

-- | Undocumented member.
tcFilter :: Lens' TopicConfiguration (Maybe NotificationConfigurationFilter)
tcFilter = lens _tcFilter (\ s a -> s{_tcFilter = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
tcTopicARN :: Lens' TopicConfiguration Text
tcTopicARN = lens _tcTopicARN (\ s a -> s{_tcTopicARN = a})

-- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
tcEvents :: Lens' TopicConfiguration [Event]
tcEvents = lens _tcEvents (\ s a -> s{_tcEvents = a}) . _Coerce

instance FromXML TopicConfiguration where
        parseXML x
          = TopicConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Topic")
                <*> (parseXMLList "Event" x)

instance Hashable TopicConfiguration where

instance NFData TopicConfiguration where

instance ToXML TopicConfiguration where
        toXML TopicConfiguration'{..}
          = mconcat
              ["Id" @= _tcId, "Filter" @= _tcFilter,
               "Topic" @= _tcTopicARN, toXMLList "Event" _tcEvents]

-- | Specifies when an object transitions to a specified storage class. For more information about Amazon S3 lifecycle configuration rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'transition' smart constructor.
data Transition = Transition'
  { _traDays         :: !(Maybe Int)
  , _traDate         :: !(Maybe ISO8601)
  , _traStorageClass :: !(Maybe TransitionStorageClass)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Transition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'traDays' - Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
--
-- * 'traDate' - Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
--
-- * 'traStorageClass' - The storage class to which you want the object to transition.
transition
    :: Transition
transition =
  Transition'
    {_traDays = Nothing, _traDate = Nothing, _traStorageClass = Nothing}


-- | Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
traDays :: Lens' Transition (Maybe Int)
traDays = lens _traDays (\ s a -> s{_traDays = a})

-- | Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
traDate :: Lens' Transition (Maybe UTCTime)
traDate = lens _traDate (\ s a -> s{_traDate = a}) . mapping _Time

-- | The storage class to which you want the object to transition.
traStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
traStorageClass = lens _traStorageClass (\ s a -> s{_traStorageClass = a})

instance FromXML Transition where
        parseXML x
          = Transition' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "StorageClass")

instance Hashable Transition where

instance NFData Transition where

instance ToXML Transition where
        toXML Transition'{..}
          = mconcat
              ["Days" @= _traDays, "Date" @= _traDate,
               "StorageClass" @= _traStorageClass]

-- | Describes the versioning state of an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html PUT Bucket versioning> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'versioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { _vcStatus    :: !(Maybe BucketVersioningStatus)
  , _vcMFADelete :: !(Maybe MFADelete)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcStatus' - The versioning state of the bucket.
--
-- * 'vcMFADelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
versioningConfiguration
    :: VersioningConfiguration
versioningConfiguration =
  VersioningConfiguration' {_vcStatus = Nothing, _vcMFADelete = Nothing}


-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe BucketVersioningStatus)
vcStatus = lens _vcStatus (\ s a -> s{_vcStatus = a})

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe MFADelete)
vcMFADelete = lens _vcMFADelete (\ s a -> s{_vcMFADelete = a})

instance Hashable VersioningConfiguration where

instance NFData VersioningConfiguration where

instance ToXML VersioningConfiguration where
        toXML VersioningConfiguration'{..}
          = mconcat
              ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]

-- | Specifies website configuration parameters for an Amazon S3 bucket.
--
--
--
-- /See:/ 'websiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { _wcRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
  , _wcErrorDocument         :: !(Maybe ErrorDocument)
  , _wcIndexDocument         :: !(Maybe IndexDocument)
  , _wcRoutingRules          :: !(Maybe [RoutingRule])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WebsiteConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcRedirectAllRequestsTo' - The redirect behavior for every request to this bucket's website endpoint. /Important:/ If you specify this property, you can't specify any other property.
--
-- * 'wcErrorDocument' - The name of the error document for the website.
--
-- * 'wcIndexDocument' - The name of the index document for the website.
--
-- * 'wcRoutingRules' - Rules that define when a redirect is applied and the redirect behavior.
websiteConfiguration
    :: WebsiteConfiguration
websiteConfiguration =
  WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing
    , _wcErrorDocument = Nothing
    , _wcIndexDocument = Nothing
    , _wcRoutingRules = Nothing
    }


-- | The redirect behavior for every request to this bucket's website endpoint. /Important:/ If you specify this property, you can't specify any other property.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\ s a -> s{_wcRedirectAllRequestsTo = a})

-- | The name of the error document for the website.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\ s a -> s{_wcErrorDocument = a})

-- | The name of the index document for the website.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\ s a -> s{_wcIndexDocument = a})

-- | Rules that define when a redirect is applied and the redirect behavior.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\ s a -> s{_wcRoutingRules = a}) . _Default . _Coerce

instance Hashable WebsiteConfiguration where

instance NFData WebsiteConfiguration where

instance ToXML WebsiteConfiguration where
        toXML WebsiteConfiguration'{..}
          = mconcat
              ["RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
               "ErrorDocument" @= _wcErrorDocument,
               "IndexDocument" @= _wcIndexDocument,
               "RoutingRules" @=
                 toXML (toXMLList "RoutingRule" <$> _wcRoutingRules)]
