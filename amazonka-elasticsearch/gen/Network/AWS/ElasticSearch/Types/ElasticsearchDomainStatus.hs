{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus where

import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The current status of an Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchDomainStatus' smart constructor.
data ElasticsearchDomainStatus = ElasticsearchDomainStatus'{_edsEBSOptions
                                                            ::
                                                            !(Maybe EBSOptions),
                                                            _edsAccessPolicies
                                                            :: !(Maybe Text),
                                                            _edsLogPublishingOptions
                                                            ::
                                                            !(Maybe
                                                                (Map LogType
                                                                   LogPublishingOption)),
                                                            _edsCreated ::
                                                            !(Maybe Bool),
                                                            _edsSnapshotOptions
                                                            ::
                                                            !(Maybe
                                                                SnapshotOptions),
                                                            _edsCognitoOptions
                                                            ::
                                                            !(Maybe
                                                                CognitoOptions),
                                                            _edsEncryptionAtRestOptions
                                                            ::
                                                            !(Maybe
                                                                EncryptionAtRestOptions),
                                                            _edsDeleted ::
                                                            !(Maybe Bool),
                                                            _edsVPCOptions ::
                                                            !(Maybe
                                                                VPCDerivedInfo),
                                                            _edsEndpoints ::
                                                            !(Maybe
                                                                (Map Text
                                                                   Text)),
                                                            _edsProcessing ::
                                                            !(Maybe Bool),
                                                            _edsEndpoint ::
                                                            !(Maybe Text),
                                                            _edsAdvancedOptions
                                                            ::
                                                            !(Maybe
                                                                (Map Text
                                                                   Text)),
                                                            _edsElasticsearchVersion
                                                            :: !(Maybe Text),
                                                            _edsDomainId ::
                                                            !Text,
                                                            _edsDomainName ::
                                                            !Text,
                                                            _edsARN :: !Text,
                                                            _edsElasticsearchClusterConfig
                                                            ::
                                                            !ElasticsearchClusterConfig}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'ElasticsearchDomainStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edsEBSOptions' - The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
--
-- * 'edsAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'edsLogPublishingOptions' - Log publishing options for the given domain.
--
-- * 'edsCreated' - The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
--
-- * 'edsSnapshotOptions' - Specifies the status of the @SnapshotOptions@
--
-- * 'edsCognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- * 'edsEncryptionAtRestOptions' - Specifies the status of the @EncryptionAtRestOptions@ .
--
-- * 'edsDeleted' - The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
--
-- * 'edsVPCOptions' - The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- * 'edsEndpoints' - Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
--
-- * 'edsProcessing' - The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
--
-- * 'edsEndpoint' - The Elasticsearch domain endpoint that you use to submit index and search requests.
--
-- * 'edsAdvancedOptions' - Specifies the status of the @AdvancedOptions@
--
-- * 'edsElasticsearchVersion' - Undocumented member.
--
-- * 'edsDomainId' - The unique identifier for the specified Elasticsearch domain.
--
-- * 'edsDomainName' - The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
--
-- * 'edsARN' - The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
--
-- * 'edsElasticsearchClusterConfig' - The type and number of instances in the domain cluster.
elasticsearchDomainStatus
  :: Text -- ^ 'edsDomainId'
  -> Text -- ^ 'edsDomainName'
  -> Text -- ^ 'edsARN'
  -> ElasticsearchClusterConfig -- ^ 'edsElasticsearchClusterConfig'
  -> ElasticsearchDomainStatus
elasticsearchDomainStatus pDomainId_ pDomainName_ pARN_ pElasticsearchClusterConfig_
  = ElasticsearchDomainStatus'
    { _edsEBSOptions                 = Nothing
    , _edsAccessPolicies             = Nothing
    , _edsLogPublishingOptions       = Nothing
    , _edsCreated                    = Nothing
    , _edsSnapshotOptions            = Nothing
    , _edsCognitoOptions             = Nothing
    , _edsEncryptionAtRestOptions    = Nothing
    , _edsDeleted                    = Nothing
    , _edsVPCOptions                 = Nothing
    , _edsEndpoints                  = Nothing
    , _edsProcessing                 = Nothing
    , _edsEndpoint                   = Nothing
    , _edsAdvancedOptions            = Nothing
    , _edsElasticsearchVersion       = Nothing
    , _edsDomainId                   = pDomainId_
    , _edsDomainName                 = pDomainName_
    , _edsARN                        = pARN_
    , _edsElasticsearchClusterConfig = pElasticsearchClusterConfig_
    }

-- | The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
edsEBSOptions :: Lens' ElasticsearchDomainStatus (Maybe EBSOptions)
edsEBSOptions = lens _edsEBSOptions (\s a -> s { _edsEBSOptions = a })

-- | IAM access policy as a JSON-formatted string.
edsAccessPolicies :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsAccessPolicies =
  lens _edsAccessPolicies (\s a -> s { _edsAccessPolicies = a })

-- | Log publishing options for the given domain.
edsLogPublishingOptions
  :: Lens' ElasticsearchDomainStatus (HashMap LogType LogPublishingOption)
edsLogPublishingOptions =
  lens _edsLogPublishingOptions (\s a -> s { _edsLogPublishingOptions = a })
    . _Default
    . _Map

-- | The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
edsCreated :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsCreated = lens _edsCreated (\s a -> s { _edsCreated = a })

-- | Specifies the status of the @SnapshotOptions@
edsSnapshotOptions :: Lens' ElasticsearchDomainStatus (Maybe SnapshotOptions)
edsSnapshotOptions =
  lens _edsSnapshotOptions (\s a -> s { _edsSnapshotOptions = a })

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
edsCognitoOptions :: Lens' ElasticsearchDomainStatus (Maybe CognitoOptions)
edsCognitoOptions =
  lens _edsCognitoOptions (\s a -> s { _edsCognitoOptions = a })

-- | Specifies the status of the @EncryptionAtRestOptions@ .
edsEncryptionAtRestOptions
  :: Lens' ElasticsearchDomainStatus (Maybe EncryptionAtRestOptions)
edsEncryptionAtRestOptions = lens
  _edsEncryptionAtRestOptions
  (\s a -> s { _edsEncryptionAtRestOptions = a })

-- | The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
edsDeleted :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsDeleted = lens _edsDeleted (\s a -> s { _edsDeleted = a })

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
edsVPCOptions :: Lens' ElasticsearchDomainStatus (Maybe VPCDerivedInfo)
edsVPCOptions = lens _edsVPCOptions (\s a -> s { _edsVPCOptions = a })

-- | Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
edsEndpoints :: Lens' ElasticsearchDomainStatus (HashMap Text Text)
edsEndpoints =
  lens _edsEndpoints (\s a -> s { _edsEndpoints = a }) . _Default . _Map

-- | The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
edsProcessing :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsProcessing = lens _edsProcessing (\s a -> s { _edsProcessing = a })

-- | The Elasticsearch domain endpoint that you use to submit index and search requests.
edsEndpoint :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsEndpoint = lens _edsEndpoint (\s a -> s { _edsEndpoint = a })

-- | Specifies the status of the @AdvancedOptions@
edsAdvancedOptions :: Lens' ElasticsearchDomainStatus (HashMap Text Text)
edsAdvancedOptions =
  lens _edsAdvancedOptions (\s a -> s { _edsAdvancedOptions = a })
    . _Default
    . _Map

-- | Undocumented member.
edsElasticsearchVersion :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsElasticsearchVersion =
  lens _edsElasticsearchVersion (\s a -> s { _edsElasticsearchVersion = a })

-- | The unique identifier for the specified Elasticsearch domain.
edsDomainId :: Lens' ElasticsearchDomainStatus Text
edsDomainId = lens _edsDomainId (\s a -> s { _edsDomainId = a })

-- | The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
edsDomainName :: Lens' ElasticsearchDomainStatus Text
edsDomainName = lens _edsDomainName (\s a -> s { _edsDomainName = a })

-- | The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
edsARN :: Lens' ElasticsearchDomainStatus Text
edsARN = lens _edsARN (\s a -> s { _edsARN = a })

-- | The type and number of instances in the domain cluster.
edsElasticsearchClusterConfig
  :: Lens' ElasticsearchDomainStatus ElasticsearchClusterConfig
edsElasticsearchClusterConfig = lens
  _edsElasticsearchClusterConfig
  (\s a -> s { _edsElasticsearchClusterConfig = a })

instance FromJSON ElasticsearchDomainStatus where
  parseJSON = withObject
    "ElasticsearchDomainStatus"
    (\x ->
      ElasticsearchDomainStatus'
        <$> (x .:? "EBSOptions")
        <*> (x .:? "AccessPolicies")
        <*> (x .:? "LogPublishingOptions" .!= mempty)
        <*> (x .:? "Created")
        <*> (x .:? "SnapshotOptions")
        <*> (x .:? "CognitoOptions")
        <*> (x .:? "EncryptionAtRestOptions")
        <*> (x .:? "Deleted")
        <*> (x .:? "VPCOptions")
        <*> (x .:? "Endpoints" .!= mempty)
        <*> (x .:? "Processing")
        <*> (x .:? "Endpoint")
        <*> (x .:? "AdvancedOptions" .!= mempty)
        <*> (x .:? "ElasticsearchVersion")
        <*> (x .: "DomainId")
        <*> (x .: "DomainName")
        <*> (x .: "ARN")
        <*> (x .: "ElasticsearchClusterConfig")
    )

instance Hashable ElasticsearchDomainStatus where

instance NFData ElasticsearchDomainStatus where
