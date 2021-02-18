{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances.
--
--
module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
    (
    -- * Creating a Request
      updateElasticsearchDomainConfig
    , UpdateElasticsearchDomainConfig
    -- * Request Lenses
    , uedcEBSOptions
    , uedcNodeToNodeEncryptionOptions
    , uedcAccessPolicies
    , uedcLogPublishingOptions
    , uedcAdvancedSecurityOptions
    , uedcElasticsearchClusterConfig
    , uedcSnapshotOptions
    , uedcCognitoOptions
    , uedcEncryptionAtRestOptions
    , uedcVPCOptions
    , uedcDomainEndpointOptions
    , uedcAdvancedOptions
    , uedcDomainName

    -- * Destructuring the Response
    , updateElasticsearchDomainConfigResponse
    , UpdateElasticsearchDomainConfigResponse
    -- * Response Lenses
    , uedcrsResponseStatus
    , uedcrsDomainConfig
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'UpdateElasticsearchDomain' @ operation. Specifies the type and number of instances in the domain cluster.
--
--
--
-- /See:/ 'updateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
  { _uedcEBSOptions :: !(Maybe EBSOptions)
  , _uedcNodeToNodeEncryptionOptions :: !(Maybe NodeToNodeEncryptionOptions)
  , _uedcAccessPolicies :: !(Maybe Text)
  , _uedcLogPublishingOptions :: !(Maybe (Map LogType LogPublishingOption))
  , _uedcAdvancedSecurityOptions :: !(Maybe AdvancedSecurityOptionsInput)
  , _uedcElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfig)
  , _uedcSnapshotOptions :: !(Maybe SnapshotOptions)
  , _uedcCognitoOptions :: !(Maybe CognitoOptions)
  , _uedcEncryptionAtRestOptions :: !(Maybe EncryptionAtRestOptions)
  , _uedcVPCOptions :: !(Maybe VPCOptions)
  , _uedcDomainEndpointOptions :: !(Maybe DomainEndpointOptions)
  , _uedcAdvancedOptions :: !(Maybe (Map Text Text))
  , _uedcDomainName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedcEBSOptions' - Specify the type and size of the EBS volume that you want to use.
--
-- * 'uedcNodeToNodeEncryptionOptions' - Specifies the NodeToNodeEncryptionOptions.
--
-- * 'uedcAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'uedcLogPublishingOptions' - Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- * 'uedcAdvancedSecurityOptions' - Specifies advanced security options.
--
-- * 'uedcElasticsearchClusterConfig' - The type and number of instances to instantiate for the domain cluster.
--
-- * 'uedcSnapshotOptions' - Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
--
-- * 'uedcCognitoOptions' - Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- * 'uedcEncryptionAtRestOptions' - Specifies the Encryption At Rest Options.
--
-- * 'uedcVPCOptions' - Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- * 'uedcDomainEndpointOptions' - Options to specify configuration that will be applied to the domain endpoint.
--
-- * 'uedcAdvancedOptions' - Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- * 'uedcDomainName' - The name of the Elasticsearch domain that you are updating.
updateElasticsearchDomainConfig
    :: Text -- ^ 'uedcDomainName'
    -> UpdateElasticsearchDomainConfig
updateElasticsearchDomainConfig pDomainName_ =
  UpdateElasticsearchDomainConfig'
    { _uedcEBSOptions = Nothing
    , _uedcNodeToNodeEncryptionOptions = Nothing
    , _uedcAccessPolicies = Nothing
    , _uedcLogPublishingOptions = Nothing
    , _uedcAdvancedSecurityOptions = Nothing
    , _uedcElasticsearchClusterConfig = Nothing
    , _uedcSnapshotOptions = Nothing
    , _uedcCognitoOptions = Nothing
    , _uedcEncryptionAtRestOptions = Nothing
    , _uedcVPCOptions = Nothing
    , _uedcDomainEndpointOptions = Nothing
    , _uedcAdvancedOptions = Nothing
    , _uedcDomainName = pDomainName_
    }


-- | Specify the type and size of the EBS volume that you want to use.
uedcEBSOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe EBSOptions)
uedcEBSOptions = lens _uedcEBSOptions (\ s a -> s{_uedcEBSOptions = a})

-- | Specifies the NodeToNodeEncryptionOptions.
uedcNodeToNodeEncryptionOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe NodeToNodeEncryptionOptions)
uedcNodeToNodeEncryptionOptions = lens _uedcNodeToNodeEncryptionOptions (\ s a -> s{_uedcNodeToNodeEncryptionOptions = a})

-- | IAM access policy as a JSON-formatted string.
uedcAccessPolicies :: Lens' UpdateElasticsearchDomainConfig (Maybe Text)
uedcAccessPolicies = lens _uedcAccessPolicies (\ s a -> s{_uedcAccessPolicies = a})

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
uedcLogPublishingOptions :: Lens' UpdateElasticsearchDomainConfig (HashMap LogType LogPublishingOption)
uedcLogPublishingOptions = lens _uedcLogPublishingOptions (\ s a -> s{_uedcLogPublishingOptions = a}) . _Default . _Map

-- | Specifies advanced security options.
uedcAdvancedSecurityOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe AdvancedSecurityOptionsInput)
uedcAdvancedSecurityOptions = lens _uedcAdvancedSecurityOptions (\ s a -> s{_uedcAdvancedSecurityOptions = a})

-- | The type and number of instances to instantiate for the domain cluster.
uedcElasticsearchClusterConfig :: Lens' UpdateElasticsearchDomainConfig (Maybe ElasticsearchClusterConfig)
uedcElasticsearchClusterConfig = lens _uedcElasticsearchClusterConfig (\ s a -> s{_uedcElasticsearchClusterConfig = a})

-- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is @0@ hours.
uedcSnapshotOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe SnapshotOptions)
uedcSnapshotOptions = lens _uedcSnapshotOptions (\ s a -> s{_uedcSnapshotOptions = a})

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
uedcCognitoOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe CognitoOptions)
uedcCognitoOptions = lens _uedcCognitoOptions (\ s a -> s{_uedcCognitoOptions = a})

-- | Specifies the Encryption At Rest Options.
uedcEncryptionAtRestOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe EncryptionAtRestOptions)
uedcEncryptionAtRestOptions = lens _uedcEncryptionAtRestOptions (\ s a -> s{_uedcEncryptionAtRestOptions = a})

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
uedcVPCOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe VPCOptions)
uedcVPCOptions = lens _uedcVPCOptions (\ s a -> s{_uedcVPCOptions = a})

-- | Options to specify configuration that will be applied to the domain endpoint.
uedcDomainEndpointOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe DomainEndpointOptions)
uedcDomainEndpointOptions = lens _uedcDomainEndpointOptions (\ s a -> s{_uedcDomainEndpointOptions = a})

-- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
uedcAdvancedOptions :: Lens' UpdateElasticsearchDomainConfig (HashMap Text Text)
uedcAdvancedOptions = lens _uedcAdvancedOptions (\ s a -> s{_uedcAdvancedOptions = a}) . _Default . _Map

-- | The name of the Elasticsearch domain that you are updating.
uedcDomainName :: Lens' UpdateElasticsearchDomainConfig Text
uedcDomainName = lens _uedcDomainName (\ s a -> s{_uedcDomainName = a})

instance AWSRequest UpdateElasticsearchDomainConfig
         where
        type Rs UpdateElasticsearchDomainConfig =
             UpdateElasticsearchDomainConfigResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 UpdateElasticsearchDomainConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DomainConfig"))

instance Hashable UpdateElasticsearchDomainConfig
         where

instance NFData UpdateElasticsearchDomainConfig where

instance ToHeaders UpdateElasticsearchDomainConfig
         where
        toHeaders = const mempty

instance ToJSON UpdateElasticsearchDomainConfig where
        toJSON UpdateElasticsearchDomainConfig'{..}
          = object
              (catMaybes
                 [("EBSOptions" .=) <$> _uedcEBSOptions,
                  ("NodeToNodeEncryptionOptions" .=) <$>
                    _uedcNodeToNodeEncryptionOptions,
                  ("AccessPolicies" .=) <$> _uedcAccessPolicies,
                  ("LogPublishingOptions" .=) <$>
                    _uedcLogPublishingOptions,
                  ("AdvancedSecurityOptions" .=) <$>
                    _uedcAdvancedSecurityOptions,
                  ("ElasticsearchClusterConfig" .=) <$>
                    _uedcElasticsearchClusterConfig,
                  ("SnapshotOptions" .=) <$> _uedcSnapshotOptions,
                  ("CognitoOptions" .=) <$> _uedcCognitoOptions,
                  ("EncryptionAtRestOptions" .=) <$>
                    _uedcEncryptionAtRestOptions,
                  ("VPCOptions" .=) <$> _uedcVPCOptions,
                  ("DomainEndpointOptions" .=) <$>
                    _uedcDomainEndpointOptions,
                  ("AdvancedOptions" .=) <$> _uedcAdvancedOptions])

instance ToPath UpdateElasticsearchDomainConfig where
        toPath UpdateElasticsearchDomainConfig'{..}
          = mconcat
              ["/2015-01-01/es/domain/", toBS _uedcDomainName,
               "/config"]

instance ToQuery UpdateElasticsearchDomainConfig
         where
        toQuery = const mempty

-- | The result of an @UpdateElasticsearchDomain@ request. Contains the status of the Elasticsearch domain being updated.
--
--
--
-- /See:/ 'updateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
  { _uedcrsResponseStatus :: !Int
  , _uedcrsDomainConfig   :: !ElasticsearchDomainConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateElasticsearchDomainConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedcrsResponseStatus' - -- | The response status code.
--
-- * 'uedcrsDomainConfig' - The status of the updated Elasticsearch domain.
updateElasticsearchDomainConfigResponse
    :: Int -- ^ 'uedcrsResponseStatus'
    -> ElasticsearchDomainConfig -- ^ 'uedcrsDomainConfig'
    -> UpdateElasticsearchDomainConfigResponse
updateElasticsearchDomainConfigResponse pResponseStatus_ pDomainConfig_ =
  UpdateElasticsearchDomainConfigResponse'
    { _uedcrsResponseStatus = pResponseStatus_
    , _uedcrsDomainConfig = pDomainConfig_
    }


-- | -- | The response status code.
uedcrsResponseStatus :: Lens' UpdateElasticsearchDomainConfigResponse Int
uedcrsResponseStatus = lens _uedcrsResponseStatus (\ s a -> s{_uedcrsResponseStatus = a})

-- | The status of the updated Elasticsearch domain.
uedcrsDomainConfig :: Lens' UpdateElasticsearchDomainConfigResponse ElasticsearchDomainConfig
uedcrsDomainConfig = lens _uedcrsDomainConfig (\ s a -> s{_uedcrsDomainConfig = a})

instance NFData
           UpdateElasticsearchDomainConfigResponse
         where
