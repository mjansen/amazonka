{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CreateElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
module Network.AWS.ElasticSearch.CreateElasticsearchDomain
  ( -- * Creating a request
    CreateElasticsearchDomain (..),
    mkCreateElasticsearchDomain,

    -- ** Request lenses
    cedDomainName,
    cedAccessPolicies,
    cedAdvancedOptions,
    cedAdvancedSecurityOptions,
    cedCognitoOptions,
    cedDomainEndpointOptions,
    cedEBSOptions,
    cedElasticsearchClusterConfig,
    cedElasticsearchVersion,
    cedEncryptionAtRestOptions,
    cedLogPublishingOptions,
    cedNodeToNodeEncryptionOptions,
    cedSnapshotOptions,
    cedVPCOptions,

    -- * Destructuring the response
    CreateElasticsearchDomainResponse (..),
    mkCreateElasticsearchDomainResponse,

    -- ** Response lenses
    cedrrsDomainStatus,
    cedrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
  { -- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a lowercase letter and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
    domainName :: Types.DomainName,
    -- | IAM access policy as a JSON-formatted string.
    accessPolicies :: Core.Maybe Types.PolicyDocument,
    -- | Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
    advancedOptions :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Specifies advanced security options.
    advancedSecurityOptions :: Core.Maybe Types.AdvancedSecurityOptionsInput,
    -- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
    cognitoOptions :: Core.Maybe Types.CognitoOptions,
    -- | Options to specify configuration that will be applied to the domain endpoint.
    domainEndpointOptions :: Core.Maybe Types.DomainEndpointOptions,
    -- | Options to enable, disable and specify the type and size of EBS storage volumes.
    eBSOptions :: Core.Maybe Types.EBSOptions,
    -- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
    elasticsearchClusterConfig :: Core.Maybe Types.ElasticsearchClusterConfig,
    -- | String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
    elasticsearchVersion :: Core.Maybe Types.ElasticsearchVersionString,
    -- | Specifies the Encryption At Rest Options.
    encryptionAtRestOptions :: Core.Maybe Types.EncryptionAtRestOptions,
    -- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
    logPublishingOptions :: Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption),
    -- | Specifies the NodeToNodeEncryptionOptions.
    nodeToNodeEncryptionOptions :: Core.Maybe Types.NodeToNodeEncryptionOptions,
    -- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
    snapshotOptions :: Core.Maybe Types.SnapshotOptions,
    -- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
    vPCOptions :: Core.Maybe Types.VPCOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateElasticsearchDomain' value with any optional fields omitted.
mkCreateElasticsearchDomain ::
  -- | 'domainName'
  Types.DomainName ->
  CreateElasticsearchDomain
mkCreateElasticsearchDomain domainName =
  CreateElasticsearchDomain'
    { domainName,
      accessPolicies = Core.Nothing,
      advancedOptions = Core.Nothing,
      advancedSecurityOptions = Core.Nothing,
      cognitoOptions = Core.Nothing,
      domainEndpointOptions = Core.Nothing,
      eBSOptions = Core.Nothing,
      elasticsearchClusterConfig = Core.Nothing,
      elasticsearchVersion = Core.Nothing,
      encryptionAtRestOptions = Core.Nothing,
      logPublishingOptions = Core.Nothing,
      nodeToNodeEncryptionOptions = Core.Nothing,
      snapshotOptions = Core.Nothing,
      vPCOptions = Core.Nothing
    }

-- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a lowercase letter and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDomainName :: Lens.Lens' CreateElasticsearchDomain Types.DomainName
cedDomainName = Lens.field @"domainName"
{-# DEPRECATED cedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | IAM access policy as a JSON-formatted string.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAccessPolicies :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.PolicyDocument)
cedAccessPolicies = Lens.field @"accessPolicies"
{-# DEPRECATED cedAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- /Note:/ Consider using 'advancedOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAdvancedOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe (Core.HashMap Types.String Types.String))
cedAdvancedOptions = Lens.field @"advancedOptions"
{-# DEPRECATED cedAdvancedOptions "Use generic-lens or generic-optics with 'advancedOptions' instead." #-}

-- | Specifies advanced security options.
--
-- /Note:/ Consider using 'advancedSecurityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedAdvancedSecurityOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.AdvancedSecurityOptionsInput)
cedAdvancedSecurityOptions = Lens.field @"advancedSecurityOptions"
{-# DEPRECATED cedAdvancedSecurityOptions "Use generic-lens or generic-optics with 'advancedSecurityOptions' instead." #-}

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /Note:/ Consider using 'cognitoOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedCognitoOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.CognitoOptions)
cedCognitoOptions = Lens.field @"cognitoOptions"
{-# DEPRECATED cedCognitoOptions "Use generic-lens or generic-optics with 'cognitoOptions' instead." #-}

-- | Options to specify configuration that will be applied to the domain endpoint.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDomainEndpointOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.DomainEndpointOptions)
cedDomainEndpointOptions = Lens.field @"domainEndpointOptions"
{-# DEPRECATED cedDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | Options to enable, disable and specify the type and size of EBS storage volumes.
--
-- /Note:/ Consider using 'eBSOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEBSOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.EBSOptions)
cedEBSOptions = Lens.field @"eBSOptions"
{-# DEPRECATED cedEBSOptions "Use generic-lens or generic-optics with 'eBSOptions' instead." #-}

-- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
--
-- /Note:/ Consider using 'elasticsearchClusterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedElasticsearchClusterConfig :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.ElasticsearchClusterConfig)
cedElasticsearchClusterConfig = Lens.field @"elasticsearchClusterConfig"
{-# DEPRECATED cedElasticsearchClusterConfig "Use generic-lens or generic-optics with 'elasticsearchClusterConfig' instead." #-}

-- | String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
--
-- /Note:/ Consider using 'elasticsearchVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedElasticsearchVersion :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.ElasticsearchVersionString)
cedElasticsearchVersion = Lens.field @"elasticsearchVersion"
{-# DEPRECATED cedElasticsearchVersion "Use generic-lens or generic-optics with 'elasticsearchVersion' instead." #-}

-- | Specifies the Encryption At Rest Options.
--
-- /Note:/ Consider using 'encryptionAtRestOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEncryptionAtRestOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.EncryptionAtRestOptions)
cedEncryptionAtRestOptions = Lens.field @"encryptionAtRestOptions"
{-# DEPRECATED cedEncryptionAtRestOptions "Use generic-lens or generic-optics with 'encryptionAtRestOptions' instead." #-}

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- /Note:/ Consider using 'logPublishingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedLogPublishingOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe (Core.HashMap Types.LogType Types.LogPublishingOption))
cedLogPublishingOptions = Lens.field @"logPublishingOptions"
{-# DEPRECATED cedLogPublishingOptions "Use generic-lens or generic-optics with 'logPublishingOptions' instead." #-}

-- | Specifies the NodeToNodeEncryptionOptions.
--
-- /Note:/ Consider using 'nodeToNodeEncryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedNodeToNodeEncryptionOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.NodeToNodeEncryptionOptions)
cedNodeToNodeEncryptionOptions = Lens.field @"nodeToNodeEncryptionOptions"
{-# DEPRECATED cedNodeToNodeEncryptionOptions "Use generic-lens or generic-optics with 'nodeToNodeEncryptionOptions' instead." #-}

-- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
--
-- /Note:/ Consider using 'snapshotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedSnapshotOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.SnapshotOptions)
cedSnapshotOptions = Lens.field @"snapshotOptions"
{-# DEPRECATED cedSnapshotOptions "Use generic-lens or generic-optics with 'snapshotOptions' instead." #-}

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- /Note:/ Consider using 'vPCOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedVPCOptions :: Lens.Lens' CreateElasticsearchDomain (Core.Maybe Types.VPCOptions)
cedVPCOptions = Lens.field @"vPCOptions"
{-# DEPRECATED cedVPCOptions "Use generic-lens or generic-optics with 'vPCOptions' instead." #-}

instance Core.FromJSON CreateElasticsearchDomain where
  toJSON CreateElasticsearchDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            ("AccessPolicies" Core..=) Core.<$> accessPolicies,
            ("AdvancedOptions" Core..=) Core.<$> advancedOptions,
            ("AdvancedSecurityOptions" Core..=)
              Core.<$> advancedSecurityOptions,
            ("CognitoOptions" Core..=) Core.<$> cognitoOptions,
            ("DomainEndpointOptions" Core..=) Core.<$> domainEndpointOptions,
            ("EBSOptions" Core..=) Core.<$> eBSOptions,
            ("ElasticsearchClusterConfig" Core..=)
              Core.<$> elasticsearchClusterConfig,
            ("ElasticsearchVersion" Core..=) Core.<$> elasticsearchVersion,
            ("EncryptionAtRestOptions" Core..=)
              Core.<$> encryptionAtRestOptions,
            ("LogPublishingOptions" Core..=) Core.<$> logPublishingOptions,
            ("NodeToNodeEncryptionOptions" Core..=)
              Core.<$> nodeToNodeEncryptionOptions,
            ("SnapshotOptions" Core..=) Core.<$> snapshotOptions,
            ("VPCOptions" Core..=) Core.<$> vPCOptions
          ]
      )

instance Core.AWSRequest CreateElasticsearchDomain where
  type
    Rs CreateElasticsearchDomain =
      CreateElasticsearchDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-01-01/es/domain",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateElasticsearchDomainResponse'
            Core.<$> (x Core..:? "DomainStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @CreateElasticsearchDomain@ operation. Contains the status of the newly created Elasticsearch domain.
--
-- /See:/ 'mkCreateElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
  { -- | The status of the newly created Elasticsearch domain.
    domainStatus :: Core.Maybe Types.ElasticsearchDomainStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateElasticsearchDomainResponse' value with any optional fields omitted.
mkCreateElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateElasticsearchDomainResponse
mkCreateElasticsearchDomainResponse responseStatus =
  CreateElasticsearchDomainResponse'
    { domainStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the newly created Elasticsearch domain.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedrrsDomainStatus :: Lens.Lens' CreateElasticsearchDomainResponse (Core.Maybe Types.ElasticsearchDomainStatus)
cedrrsDomainStatus = Lens.field @"domainStatus"
{-# DEPRECATED cedrrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedrrsResponseStatus :: Lens.Lens' CreateElasticsearchDomainResponse Core.Int
cedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
