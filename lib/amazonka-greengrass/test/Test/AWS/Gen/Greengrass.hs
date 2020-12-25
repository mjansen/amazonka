{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Greengrass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Greengrass where

import Data.Proxy
import Network.AWS.Greengrass
import Test.AWS.Fixture
import Test.AWS.Greengrass.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetGroupCertificateConfiguration $
--             mkGetGroupCertificateConfiguration
--
--         , requestListGroupVersions $
--             mkListGroupVersions
--
--         , requestListFunctionDefinitionVersions $
--             mkListFunctionDefinitionVersions
--
--         , requestListDeviceDefinitions $
--             mkListDeviceDefinitions
--
--         , requestAssociateRoleToGroup $
--             mkAssociateRoleToGroup
--
--         , requestUpdateCoreDefinition $
--             mkUpdateCoreDefinition
--
--         , requestDeleteCoreDefinition $
--             mkDeleteCoreDefinition
--
--         , requestGetLoggerDefinition $
--             mkGetLoggerDefinition
--
--         , requestListGroupCertificateAuthorities $
--             mkListGroupCertificateAuthorities
--
--         , requestDisassociateRoleFromGroup $
--             mkDisassociateRoleFromGroup
--
--         , requestUpdateSubscriptionDefinition $
--             mkUpdateSubscriptionDefinition
--
--         , requestDeleteSubscriptionDefinition $
--             mkDeleteSubscriptionDefinition
--
--         , requestListCoreDefinitions $
--             mkListCoreDefinitions
--
--         , requestListSubscriptionDefinitions $
--             mkListSubscriptionDefinitions
--
--         , requestCreateGroupCertificateAuthority $
--             mkCreateGroupCertificateAuthority
--
--         , requestDeleteConnectorDefinition $
--             mkDeleteConnectorDefinition
--
--         , requestUpdateConnectorDefinition $
--             mkUpdateConnectorDefinition
--
--         , requestCreateLoggerDefinitionVersion $
--             mkCreateLoggerDefinitionVersion
--
--         , requestCreateCoreDefinition $
--             mkCreateCoreDefinition
--
--         , requestGetConnectorDefinitionVersion $
--             mkGetConnectorDefinitionVersion
--
--         , requestUpdateConnectivityInfo $
--             mkUpdateConnectivityInfo
--
--         , requestCreateSubscriptionDefinition $
--             mkCreateSubscriptionDefinition
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetGroupCertificateAuthority $
--             mkGetGroupCertificateAuthority
--
--         , requestGetLoggerDefinitionVersion $
--             mkGetLoggerDefinitionVersion
--
--         , requestGetServiceRoleForAccount $
--             mkGetServiceRoleForAccount
--
--         , requestListConnectorDefinitionVersions $
--             mkListConnectorDefinitionVersions
--
--         , requestCreateSoftwareUpdateJob $
--             mkCreateSoftwareUpdateJob
--
--         , requestCreateLoggerDefinition $
--             mkCreateLoggerDefinition
--
--         , requestGetConnectivityInfo $
--             mkGetConnectivityInfo
--
--         , requestCreateDeployment $
--             mkCreateDeployment
--
--         , requestDeleteLoggerDefinition $
--             mkDeleteLoggerDefinition
--
--         , requestUpdateLoggerDefinition $
--             mkUpdateLoggerDefinition
--
--         , requestGetSubscriptionDefinition $
--             mkGetSubscriptionDefinition
--
--         , requestGetCoreDefinition $
--             mkGetCoreDefinition
--
--         , requestCreateConnectorDefinitionVersion $
--             mkCreateConnectorDefinitionVersion
--
--         , requestGetDeploymentStatus $
--             mkGetDeploymentStatus
--
--         , requestGetBulkDeploymentStatus $
--             mkGetBulkDeploymentStatus
--
--         , requestCreateResourceDefinition $
--             mkCreateResourceDefinition
--
--         , requestGetResourceDefinitionVersion $
--             mkGetResourceDefinitionVersion
--
--         , requestUpdateFunctionDefinition $
--             mkUpdateFunctionDefinition
--
--         , requestDeleteFunctionDefinition $
--             mkDeleteFunctionDefinition
--
--         , requestListResourceDefinitions $
--             mkListResourceDefinitions
--
--         , requestStopBulkDeployment $
--             mkStopBulkDeployment
--
--         , requestCreateResourceDefinitionVersion $
--             mkCreateResourceDefinitionVersion
--
--         , requestGetResourceDefinition $
--             mkGetResourceDefinition
--
--         , requestListResourceDefinitionVersions $
--             mkListResourceDefinitionVersions
--
--         , requestDisassociateServiceRoleFromAccount $
--             mkDisassociateServiceRoleFromAccount
--
--         , requestDeleteDeviceDefinition $
--             mkDeleteDeviceDefinition
--
--         , requestUpdateDeviceDefinition $
--             mkUpdateDeviceDefinition
--
--         , requestAssociateServiceRoleToAccount $
--             mkAssociateServiceRoleToAccount
--
--         , requestResetDeployments $
--             mkResetDeployments
--
--         , requestListConnectorDefinitions $
--             mkListConnectorDefinitions
--
--         , requestGetSubscriptionDefinitionVersion $
--             mkGetSubscriptionDefinitionVersion
--
--         , requestGetAssociatedRole $
--             mkGetAssociatedRole
--
--         , requestListLoggerDefinitionVersions $
--             mkListLoggerDefinitionVersions
--
--         , requestCreateConnectorDefinition $
--             mkCreateConnectorDefinition
--
--         , requestGetCoreDefinitionVersion $
--             mkGetCoreDefinitionVersion
--
--         , requestListSubscriptionDefinitionVersions $
--             mkListSubscriptionDefinitionVersions
--
--         , requestListCoreDefinitionVersions $
--             mkListCoreDefinitionVersions
--
--         , requestCreateCoreDefinitionVersion $
--             mkCreateCoreDefinitionVersion
--
--         , requestListBulkDeployments $
--             mkListBulkDeployments
--
--         , requestListDeployments $
--             mkListDeployments
--
--         , requestGetConnectorDefinition $
--             mkGetConnectorDefinition
--
--         , requestListLoggerDefinitions $
--             mkListLoggerDefinitions
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateSubscriptionDefinitionVersion $
--             mkCreateSubscriptionDefinitionVersion
--
--         , requestGetGroupVersion $
--             mkGetGroupVersion
--
--         , requestUpdateGroupCertificateConfiguration $
--             mkUpdateGroupCertificateConfiguration
--
--         , requestGetFunctionDefinitionVersion $
--             mkGetFunctionDefinitionVersion
--
--         , requestGetDeviceDefinition $
--             mkGetDeviceDefinition
--
--         , requestCreateGroup $
--             mkCreateGroup
--
--         , requestCreateFunctionDefinition $
--             mkCreateFunctionDefinition
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateDeviceDefinitionVersion $
--             mkCreateDeviceDefinitionVersion
--
--         , requestDeleteGroup $
--             mkDeleteGroup
--
--         , requestUpdateGroup $
--             mkUpdateGroup
--
--         , requestListGroups $
--             mkListGroups
--
--         , requestListBulkDeploymentDetailedReports $
--             mkListBulkDeploymentDetailedReports
--
--         , requestGetThingRuntimeConfiguration $
--             mkGetThingRuntimeConfiguration
--
--         , requestDeleteResourceDefinition $
--             mkDeleteResourceDefinition
--
--         , requestUpdateResourceDefinition $
--             mkUpdateResourceDefinition
--
--         , requestListDeviceDefinitionVersions $
--             mkListDeviceDefinitionVersions
--
--         , requestListFunctionDefinitions $
--             mkListFunctionDefinitions
--
--         , requestGetFunctionDefinition $
--             mkGetFunctionDefinition
--
--         , requestGetGroup $
--             mkGetGroup
--
--         , requestCreateDeviceDefinition $
--             mkCreateDeviceDefinition
--
--         , requestCreateGroupVersion $
--             mkCreateGroupVersion
--
--         , requestCreateFunctionDefinitionVersion $
--             mkCreateFunctionDefinitionVersion
--
--         , requestStartBulkDeployment $
--             mkStartBulkDeployment
--
--         , requestUpdateThingRuntimeConfiguration $
--             mkUpdateThingRuntimeConfiguration
--
--         , requestGetDeviceDefinitionVersion $
--             mkGetDeviceDefinitionVersion
--
--           ]

--     , testGroup "response"
--         [ responseGetGroupCertificateConfiguration $
--             mkGetGroupCertificateConfigurationResponse
--
--         , responseListGroupVersions $
--             mkListGroupVersionsResponse
--
--         , responseListFunctionDefinitionVersions $
--             mkListFunctionDefinitionVersionsResponse
--
--         , responseListDeviceDefinitions $
--             mkListDeviceDefinitionsResponse
--
--         , responseAssociateRoleToGroup $
--             mkAssociateRoleToGroupResponse
--
--         , responseUpdateCoreDefinition $
--             mkUpdateCoreDefinitionResponse
--
--         , responseDeleteCoreDefinition $
--             mkDeleteCoreDefinitionResponse
--
--         , responseGetLoggerDefinition $
--             mkGetLoggerDefinitionResponse
--
--         , responseListGroupCertificateAuthorities $
--             mkListGroupCertificateAuthoritiesResponse
--
--         , responseDisassociateRoleFromGroup $
--             mkDisassociateRoleFromGroupResponse
--
--         , responseUpdateSubscriptionDefinition $
--             mkUpdateSubscriptionDefinitionResponse
--
--         , responseDeleteSubscriptionDefinition $
--             mkDeleteSubscriptionDefinitionResponse
--
--         , responseListCoreDefinitions $
--             mkListCoreDefinitionsResponse
--
--         , responseListSubscriptionDefinitions $
--             mkListSubscriptionDefinitionsResponse
--
--         , responseCreateGroupCertificateAuthority $
--             mkCreateGroupCertificateAuthorityResponse
--
--         , responseDeleteConnectorDefinition $
--             mkDeleteConnectorDefinitionResponse
--
--         , responseUpdateConnectorDefinition $
--             mkUpdateConnectorDefinitionResponse
--
--         , responseCreateLoggerDefinitionVersion $
--             mkCreateLoggerDefinitionVersionResponse
--
--         , responseCreateCoreDefinition $
--             mkCreateCoreDefinitionResponse
--
--         , responseGetConnectorDefinitionVersion $
--             mkGetConnectorDefinitionVersionResponse
--
--         , responseUpdateConnectivityInfo $
--             mkUpdateConnectivityInfoResponse
--
--         , responseCreateSubscriptionDefinition $
--             mkCreateSubscriptionDefinitionResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetGroupCertificateAuthority $
--             mkGetGroupCertificateAuthorityResponse
--
--         , responseGetLoggerDefinitionVersion $
--             mkGetLoggerDefinitionVersionResponse
--
--         , responseGetServiceRoleForAccount $
--             mkGetServiceRoleForAccountResponse
--
--         , responseListConnectorDefinitionVersions $
--             mkListConnectorDefinitionVersionsResponse
--
--         , responseCreateSoftwareUpdateJob $
--             mkCreateSoftwareUpdateJobResponse
--
--         , responseCreateLoggerDefinition $
--             mkCreateLoggerDefinitionResponse
--
--         , responseGetConnectivityInfo $
--             mkGetConnectivityInfoResponse
--
--         , responseCreateDeployment $
--             mkCreateDeploymentResponse
--
--         , responseDeleteLoggerDefinition $
--             mkDeleteLoggerDefinitionResponse
--
--         , responseUpdateLoggerDefinition $
--             mkUpdateLoggerDefinitionResponse
--
--         , responseGetSubscriptionDefinition $
--             mkGetSubscriptionDefinitionResponse
--
--         , responseGetCoreDefinition $
--             mkGetCoreDefinitionResponse
--
--         , responseCreateConnectorDefinitionVersion $
--             mkCreateConnectorDefinitionVersionResponse
--
--         , responseGetDeploymentStatus $
--             mkGetDeploymentStatusResponse
--
--         , responseGetBulkDeploymentStatus $
--             mkGetBulkDeploymentStatusResponse
--
--         , responseCreateResourceDefinition $
--             mkCreateResourceDefinitionResponse
--
--         , responseGetResourceDefinitionVersion $
--             mkGetResourceDefinitionVersionResponse
--
--         , responseUpdateFunctionDefinition $
--             mkUpdateFunctionDefinitionResponse
--
--         , responseDeleteFunctionDefinition $
--             mkDeleteFunctionDefinitionResponse
--
--         , responseListResourceDefinitions $
--             mkListResourceDefinitionsResponse
--
--         , responseStopBulkDeployment $
--             mkStopBulkDeploymentResponse
--
--         , responseCreateResourceDefinitionVersion $
--             mkCreateResourceDefinitionVersionResponse
--
--         , responseGetResourceDefinition $
--             mkGetResourceDefinitionResponse
--
--         , responseListResourceDefinitionVersions $
--             mkListResourceDefinitionVersionsResponse
--
--         , responseDisassociateServiceRoleFromAccount $
--             mkDisassociateServiceRoleFromAccountResponse
--
--         , responseDeleteDeviceDefinition $
--             mkDeleteDeviceDefinitionResponse
--
--         , responseUpdateDeviceDefinition $
--             mkUpdateDeviceDefinitionResponse
--
--         , responseAssociateServiceRoleToAccount $
--             mkAssociateServiceRoleToAccountResponse
--
--         , responseResetDeployments $
--             mkResetDeploymentsResponse
--
--         , responseListConnectorDefinitions $
--             mkListConnectorDefinitionsResponse
--
--         , responseGetSubscriptionDefinitionVersion $
--             mkGetSubscriptionDefinitionVersionResponse
--
--         , responseGetAssociatedRole $
--             mkGetAssociatedRoleResponse
--
--         , responseListLoggerDefinitionVersions $
--             mkListLoggerDefinitionVersionsResponse
--
--         , responseCreateConnectorDefinition $
--             mkCreateConnectorDefinitionResponse
--
--         , responseGetCoreDefinitionVersion $
--             mkGetCoreDefinitionVersionResponse
--
--         , responseListSubscriptionDefinitionVersions $
--             mkListSubscriptionDefinitionVersionsResponse
--
--         , responseListCoreDefinitionVersions $
--             mkListCoreDefinitionVersionsResponse
--
--         , responseCreateCoreDefinitionVersion $
--             mkCreateCoreDefinitionVersionResponse
--
--         , responseListBulkDeployments $
--             mkListBulkDeploymentsResponse
--
--         , responseListDeployments $
--             mkListDeploymentsResponse
--
--         , responseGetConnectorDefinition $
--             mkGetConnectorDefinitionResponse
--
--         , responseListLoggerDefinitions $
--             mkListLoggerDefinitionsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateSubscriptionDefinitionVersion $
--             mkCreateSubscriptionDefinitionVersionResponse
--
--         , responseGetGroupVersion $
--             mkGetGroupVersionResponse
--
--         , responseUpdateGroupCertificateConfiguration $
--             mkUpdateGroupCertificateConfigurationResponse
--
--         , responseGetFunctionDefinitionVersion $
--             mkGetFunctionDefinitionVersionResponse
--
--         , responseGetDeviceDefinition $
--             mkGetDeviceDefinitionResponse
--
--         , responseCreateGroup $
--             mkCreateGroupResponse
--
--         , responseCreateFunctionDefinition $
--             mkCreateFunctionDefinitionResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateDeviceDefinitionVersion $
--             mkCreateDeviceDefinitionVersionResponse
--
--         , responseDeleteGroup $
--             mkDeleteGroupResponse
--
--         , responseUpdateGroup $
--             mkUpdateGroupResponse
--
--         , responseListGroups $
--             mkListGroupsResponse
--
--         , responseListBulkDeploymentDetailedReports $
--             mkListBulkDeploymentDetailedReportsResponse
--
--         , responseGetThingRuntimeConfiguration $
--             mkGetThingRuntimeConfigurationResponse
--
--         , responseDeleteResourceDefinition $
--             mkDeleteResourceDefinitionResponse
--
--         , responseUpdateResourceDefinition $
--             mkUpdateResourceDefinitionResponse
--
--         , responseListDeviceDefinitionVersions $
--             mkListDeviceDefinitionVersionsResponse
--
--         , responseListFunctionDefinitions $
--             mkListFunctionDefinitionsResponse
--
--         , responseGetFunctionDefinition $
--             mkGetFunctionDefinitionResponse
--
--         , responseGetGroup $
--             mkGetGroupResponse
--
--         , responseCreateDeviceDefinition $
--             mkCreateDeviceDefinitionResponse
--
--         , responseCreateGroupVersion $
--             mkCreateGroupVersionResponse
--
--         , responseCreateFunctionDefinitionVersion $
--             mkCreateFunctionDefinitionVersionResponse
--
--         , responseStartBulkDeployment $
--             mkStartBulkDeploymentResponse
--
--         , responseUpdateThingRuntimeConfiguration $
--             mkUpdateThingRuntimeConfigurationResponse
--
--         , responseGetDeviceDefinitionVersion $
--             mkGetDeviceDefinitionVersionResponse
--
--           ]
--     ]

-- Requests

requestGetGroupCertificateConfiguration :: GetGroupCertificateConfiguration -> TestTree
requestGetGroupCertificateConfiguration =
  req
    "GetGroupCertificateConfiguration"
    "fixture/GetGroupCertificateConfiguration.yaml"

requestListGroupVersions :: ListGroupVersions -> TestTree
requestListGroupVersions =
  req
    "ListGroupVersions"
    "fixture/ListGroupVersions.yaml"

requestListFunctionDefinitionVersions :: ListFunctionDefinitionVersions -> TestTree
requestListFunctionDefinitionVersions =
  req
    "ListFunctionDefinitionVersions"
    "fixture/ListFunctionDefinitionVersions.yaml"

requestListDeviceDefinitions :: ListDeviceDefinitions -> TestTree
requestListDeviceDefinitions =
  req
    "ListDeviceDefinitions"
    "fixture/ListDeviceDefinitions.yaml"

requestAssociateRoleToGroup :: AssociateRoleToGroup -> TestTree
requestAssociateRoleToGroup =
  req
    "AssociateRoleToGroup"
    "fixture/AssociateRoleToGroup.yaml"

requestUpdateCoreDefinition :: UpdateCoreDefinition -> TestTree
requestUpdateCoreDefinition =
  req
    "UpdateCoreDefinition"
    "fixture/UpdateCoreDefinition.yaml"

requestDeleteCoreDefinition :: DeleteCoreDefinition -> TestTree
requestDeleteCoreDefinition =
  req
    "DeleteCoreDefinition"
    "fixture/DeleteCoreDefinition.yaml"

requestGetLoggerDefinition :: GetLoggerDefinition -> TestTree
requestGetLoggerDefinition =
  req
    "GetLoggerDefinition"
    "fixture/GetLoggerDefinition.yaml"

requestListGroupCertificateAuthorities :: ListGroupCertificateAuthorities -> TestTree
requestListGroupCertificateAuthorities =
  req
    "ListGroupCertificateAuthorities"
    "fixture/ListGroupCertificateAuthorities.yaml"

requestDisassociateRoleFromGroup :: DisassociateRoleFromGroup -> TestTree
requestDisassociateRoleFromGroup =
  req
    "DisassociateRoleFromGroup"
    "fixture/DisassociateRoleFromGroup.yaml"

requestUpdateSubscriptionDefinition :: UpdateSubscriptionDefinition -> TestTree
requestUpdateSubscriptionDefinition =
  req
    "UpdateSubscriptionDefinition"
    "fixture/UpdateSubscriptionDefinition.yaml"

requestDeleteSubscriptionDefinition :: DeleteSubscriptionDefinition -> TestTree
requestDeleteSubscriptionDefinition =
  req
    "DeleteSubscriptionDefinition"
    "fixture/DeleteSubscriptionDefinition.yaml"

requestListCoreDefinitions :: ListCoreDefinitions -> TestTree
requestListCoreDefinitions =
  req
    "ListCoreDefinitions"
    "fixture/ListCoreDefinitions.yaml"

requestListSubscriptionDefinitions :: ListSubscriptionDefinitions -> TestTree
requestListSubscriptionDefinitions =
  req
    "ListSubscriptionDefinitions"
    "fixture/ListSubscriptionDefinitions.yaml"

requestCreateGroupCertificateAuthority :: CreateGroupCertificateAuthority -> TestTree
requestCreateGroupCertificateAuthority =
  req
    "CreateGroupCertificateAuthority"
    "fixture/CreateGroupCertificateAuthority.yaml"

requestDeleteConnectorDefinition :: DeleteConnectorDefinition -> TestTree
requestDeleteConnectorDefinition =
  req
    "DeleteConnectorDefinition"
    "fixture/DeleteConnectorDefinition.yaml"

requestUpdateConnectorDefinition :: UpdateConnectorDefinition -> TestTree
requestUpdateConnectorDefinition =
  req
    "UpdateConnectorDefinition"
    "fixture/UpdateConnectorDefinition.yaml"

requestCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersion -> TestTree
requestCreateLoggerDefinitionVersion =
  req
    "CreateLoggerDefinitionVersion"
    "fixture/CreateLoggerDefinitionVersion.yaml"

requestCreateCoreDefinition :: CreateCoreDefinition -> TestTree
requestCreateCoreDefinition =
  req
    "CreateCoreDefinition"
    "fixture/CreateCoreDefinition.yaml"

requestGetConnectorDefinitionVersion :: GetConnectorDefinitionVersion -> TestTree
requestGetConnectorDefinitionVersion =
  req
    "GetConnectorDefinitionVersion"
    "fixture/GetConnectorDefinitionVersion.yaml"

requestUpdateConnectivityInfo :: UpdateConnectivityInfo -> TestTree
requestUpdateConnectivityInfo =
  req
    "UpdateConnectivityInfo"
    "fixture/UpdateConnectivityInfo.yaml"

requestCreateSubscriptionDefinition :: CreateSubscriptionDefinition -> TestTree
requestCreateSubscriptionDefinition =
  req
    "CreateSubscriptionDefinition"
    "fixture/CreateSubscriptionDefinition.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetGroupCertificateAuthority :: GetGroupCertificateAuthority -> TestTree
requestGetGroupCertificateAuthority =
  req
    "GetGroupCertificateAuthority"
    "fixture/GetGroupCertificateAuthority.yaml"

requestGetLoggerDefinitionVersion :: GetLoggerDefinitionVersion -> TestTree
requestGetLoggerDefinitionVersion =
  req
    "GetLoggerDefinitionVersion"
    "fixture/GetLoggerDefinitionVersion.yaml"

requestGetServiceRoleForAccount :: GetServiceRoleForAccount -> TestTree
requestGetServiceRoleForAccount =
  req
    "GetServiceRoleForAccount"
    "fixture/GetServiceRoleForAccount.yaml"

requestListConnectorDefinitionVersions :: ListConnectorDefinitionVersions -> TestTree
requestListConnectorDefinitionVersions =
  req
    "ListConnectorDefinitionVersions"
    "fixture/ListConnectorDefinitionVersions.yaml"

requestCreateSoftwareUpdateJob :: CreateSoftwareUpdateJob -> TestTree
requestCreateSoftwareUpdateJob =
  req
    "CreateSoftwareUpdateJob"
    "fixture/CreateSoftwareUpdateJob.yaml"

requestCreateLoggerDefinition :: CreateLoggerDefinition -> TestTree
requestCreateLoggerDefinition =
  req
    "CreateLoggerDefinition"
    "fixture/CreateLoggerDefinition.yaml"

requestGetConnectivityInfo :: GetConnectivityInfo -> TestTree
requestGetConnectivityInfo =
  req
    "GetConnectivityInfo"
    "fixture/GetConnectivityInfo.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeleteLoggerDefinition :: DeleteLoggerDefinition -> TestTree
requestDeleteLoggerDefinition =
  req
    "DeleteLoggerDefinition"
    "fixture/DeleteLoggerDefinition.yaml"

requestUpdateLoggerDefinition :: UpdateLoggerDefinition -> TestTree
requestUpdateLoggerDefinition =
  req
    "UpdateLoggerDefinition"
    "fixture/UpdateLoggerDefinition.yaml"

requestGetSubscriptionDefinition :: GetSubscriptionDefinition -> TestTree
requestGetSubscriptionDefinition =
  req
    "GetSubscriptionDefinition"
    "fixture/GetSubscriptionDefinition.yaml"

requestGetCoreDefinition :: GetCoreDefinition -> TestTree
requestGetCoreDefinition =
  req
    "GetCoreDefinition"
    "fixture/GetCoreDefinition.yaml"

requestCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersion -> TestTree
requestCreateConnectorDefinitionVersion =
  req
    "CreateConnectorDefinitionVersion"
    "fixture/CreateConnectorDefinitionVersion.yaml"

requestGetDeploymentStatus :: GetDeploymentStatus -> TestTree
requestGetDeploymentStatus =
  req
    "GetDeploymentStatus"
    "fixture/GetDeploymentStatus.yaml"

requestGetBulkDeploymentStatus :: GetBulkDeploymentStatus -> TestTree
requestGetBulkDeploymentStatus =
  req
    "GetBulkDeploymentStatus"
    "fixture/GetBulkDeploymentStatus.yaml"

requestCreateResourceDefinition :: CreateResourceDefinition -> TestTree
requestCreateResourceDefinition =
  req
    "CreateResourceDefinition"
    "fixture/CreateResourceDefinition.yaml"

requestGetResourceDefinitionVersion :: GetResourceDefinitionVersion -> TestTree
requestGetResourceDefinitionVersion =
  req
    "GetResourceDefinitionVersion"
    "fixture/GetResourceDefinitionVersion.yaml"

requestUpdateFunctionDefinition :: UpdateFunctionDefinition -> TestTree
requestUpdateFunctionDefinition =
  req
    "UpdateFunctionDefinition"
    "fixture/UpdateFunctionDefinition.yaml"

requestDeleteFunctionDefinition :: DeleteFunctionDefinition -> TestTree
requestDeleteFunctionDefinition =
  req
    "DeleteFunctionDefinition"
    "fixture/DeleteFunctionDefinition.yaml"

requestListResourceDefinitions :: ListResourceDefinitions -> TestTree
requestListResourceDefinitions =
  req
    "ListResourceDefinitions"
    "fixture/ListResourceDefinitions.yaml"

requestStopBulkDeployment :: StopBulkDeployment -> TestTree
requestStopBulkDeployment =
  req
    "StopBulkDeployment"
    "fixture/StopBulkDeployment.yaml"

requestCreateResourceDefinitionVersion :: CreateResourceDefinitionVersion -> TestTree
requestCreateResourceDefinitionVersion =
  req
    "CreateResourceDefinitionVersion"
    "fixture/CreateResourceDefinitionVersion.yaml"

requestGetResourceDefinition :: GetResourceDefinition -> TestTree
requestGetResourceDefinition =
  req
    "GetResourceDefinition"
    "fixture/GetResourceDefinition.yaml"

requestListResourceDefinitionVersions :: ListResourceDefinitionVersions -> TestTree
requestListResourceDefinitionVersions =
  req
    "ListResourceDefinitionVersions"
    "fixture/ListResourceDefinitionVersions.yaml"

requestDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccount -> TestTree
requestDisassociateServiceRoleFromAccount =
  req
    "DisassociateServiceRoleFromAccount"
    "fixture/DisassociateServiceRoleFromAccount.yaml"

requestDeleteDeviceDefinition :: DeleteDeviceDefinition -> TestTree
requestDeleteDeviceDefinition =
  req
    "DeleteDeviceDefinition"
    "fixture/DeleteDeviceDefinition.yaml"

requestUpdateDeviceDefinition :: UpdateDeviceDefinition -> TestTree
requestUpdateDeviceDefinition =
  req
    "UpdateDeviceDefinition"
    "fixture/UpdateDeviceDefinition.yaml"

requestAssociateServiceRoleToAccount :: AssociateServiceRoleToAccount -> TestTree
requestAssociateServiceRoleToAccount =
  req
    "AssociateServiceRoleToAccount"
    "fixture/AssociateServiceRoleToAccount.yaml"

requestResetDeployments :: ResetDeployments -> TestTree
requestResetDeployments =
  req
    "ResetDeployments"
    "fixture/ResetDeployments.yaml"

requestListConnectorDefinitions :: ListConnectorDefinitions -> TestTree
requestListConnectorDefinitions =
  req
    "ListConnectorDefinitions"
    "fixture/ListConnectorDefinitions.yaml"

requestGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersion -> TestTree
requestGetSubscriptionDefinitionVersion =
  req
    "GetSubscriptionDefinitionVersion"
    "fixture/GetSubscriptionDefinitionVersion.yaml"

requestGetAssociatedRole :: GetAssociatedRole -> TestTree
requestGetAssociatedRole =
  req
    "GetAssociatedRole"
    "fixture/GetAssociatedRole.yaml"

requestListLoggerDefinitionVersions :: ListLoggerDefinitionVersions -> TestTree
requestListLoggerDefinitionVersions =
  req
    "ListLoggerDefinitionVersions"
    "fixture/ListLoggerDefinitionVersions.yaml"

requestCreateConnectorDefinition :: CreateConnectorDefinition -> TestTree
requestCreateConnectorDefinition =
  req
    "CreateConnectorDefinition"
    "fixture/CreateConnectorDefinition.yaml"

requestGetCoreDefinitionVersion :: GetCoreDefinitionVersion -> TestTree
requestGetCoreDefinitionVersion =
  req
    "GetCoreDefinitionVersion"
    "fixture/GetCoreDefinitionVersion.yaml"

requestListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersions -> TestTree
requestListSubscriptionDefinitionVersions =
  req
    "ListSubscriptionDefinitionVersions"
    "fixture/ListSubscriptionDefinitionVersions.yaml"

requestListCoreDefinitionVersions :: ListCoreDefinitionVersions -> TestTree
requestListCoreDefinitionVersions =
  req
    "ListCoreDefinitionVersions"
    "fixture/ListCoreDefinitionVersions.yaml"

requestCreateCoreDefinitionVersion :: CreateCoreDefinitionVersion -> TestTree
requestCreateCoreDefinitionVersion =
  req
    "CreateCoreDefinitionVersion"
    "fixture/CreateCoreDefinitionVersion.yaml"

requestListBulkDeployments :: ListBulkDeployments -> TestTree
requestListBulkDeployments =
  req
    "ListBulkDeployments"
    "fixture/ListBulkDeployments.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestGetConnectorDefinition :: GetConnectorDefinition -> TestTree
requestGetConnectorDefinition =
  req
    "GetConnectorDefinition"
    "fixture/GetConnectorDefinition.yaml"

requestListLoggerDefinitions :: ListLoggerDefinitions -> TestTree
requestListLoggerDefinitions =
  req
    "ListLoggerDefinitions"
    "fixture/ListLoggerDefinitions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersion -> TestTree
requestCreateSubscriptionDefinitionVersion =
  req
    "CreateSubscriptionDefinitionVersion"
    "fixture/CreateSubscriptionDefinitionVersion.yaml"

requestGetGroupVersion :: GetGroupVersion -> TestTree
requestGetGroupVersion =
  req
    "GetGroupVersion"
    "fixture/GetGroupVersion.yaml"

requestUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfiguration -> TestTree
requestUpdateGroupCertificateConfiguration =
  req
    "UpdateGroupCertificateConfiguration"
    "fixture/UpdateGroupCertificateConfiguration.yaml"

requestGetFunctionDefinitionVersion :: GetFunctionDefinitionVersion -> TestTree
requestGetFunctionDefinitionVersion =
  req
    "GetFunctionDefinitionVersion"
    "fixture/GetFunctionDefinitionVersion.yaml"

requestGetDeviceDefinition :: GetDeviceDefinition -> TestTree
requestGetDeviceDefinition =
  req
    "GetDeviceDefinition"
    "fixture/GetDeviceDefinition.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateFunctionDefinition :: CreateFunctionDefinition -> TestTree
requestCreateFunctionDefinition =
  req
    "CreateFunctionDefinition"
    "fixture/CreateFunctionDefinition.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersion -> TestTree
requestCreateDeviceDefinitionVersion =
  req
    "CreateDeviceDefinitionVersion"
    "fixture/CreateDeviceDefinitionVersion.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReports -> TestTree
requestListBulkDeploymentDetailedReports =
  req
    "ListBulkDeploymentDetailedReports"
    "fixture/ListBulkDeploymentDetailedReports.yaml"

requestGetThingRuntimeConfiguration :: GetThingRuntimeConfiguration -> TestTree
requestGetThingRuntimeConfiguration =
  req
    "GetThingRuntimeConfiguration"
    "fixture/GetThingRuntimeConfiguration.yaml"

requestDeleteResourceDefinition :: DeleteResourceDefinition -> TestTree
requestDeleteResourceDefinition =
  req
    "DeleteResourceDefinition"
    "fixture/DeleteResourceDefinition.yaml"

requestUpdateResourceDefinition :: UpdateResourceDefinition -> TestTree
requestUpdateResourceDefinition =
  req
    "UpdateResourceDefinition"
    "fixture/UpdateResourceDefinition.yaml"

requestListDeviceDefinitionVersions :: ListDeviceDefinitionVersions -> TestTree
requestListDeviceDefinitionVersions =
  req
    "ListDeviceDefinitionVersions"
    "fixture/ListDeviceDefinitionVersions.yaml"

requestListFunctionDefinitions :: ListFunctionDefinitions -> TestTree
requestListFunctionDefinitions =
  req
    "ListFunctionDefinitions"
    "fixture/ListFunctionDefinitions.yaml"

requestGetFunctionDefinition :: GetFunctionDefinition -> TestTree
requestGetFunctionDefinition =
  req
    "GetFunctionDefinition"
    "fixture/GetFunctionDefinition.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestCreateDeviceDefinition :: CreateDeviceDefinition -> TestTree
requestCreateDeviceDefinition =
  req
    "CreateDeviceDefinition"
    "fixture/CreateDeviceDefinition.yaml"

requestCreateGroupVersion :: CreateGroupVersion -> TestTree
requestCreateGroupVersion =
  req
    "CreateGroupVersion"
    "fixture/CreateGroupVersion.yaml"

requestCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersion -> TestTree
requestCreateFunctionDefinitionVersion =
  req
    "CreateFunctionDefinitionVersion"
    "fixture/CreateFunctionDefinitionVersion.yaml"

requestStartBulkDeployment :: StartBulkDeployment -> TestTree
requestStartBulkDeployment =
  req
    "StartBulkDeployment"
    "fixture/StartBulkDeployment.yaml"

requestUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfiguration -> TestTree
requestUpdateThingRuntimeConfiguration =
  req
    "UpdateThingRuntimeConfiguration"
    "fixture/UpdateThingRuntimeConfiguration.yaml"

requestGetDeviceDefinitionVersion :: GetDeviceDefinitionVersion -> TestTree
requestGetDeviceDefinitionVersion =
  req
    "GetDeviceDefinitionVersion"
    "fixture/GetDeviceDefinitionVersion.yaml"

-- Responses

responseGetGroupCertificateConfiguration :: GetGroupCertificateConfigurationResponse -> TestTree
responseGetGroupCertificateConfiguration =
  res
    "GetGroupCertificateConfigurationResponse"
    "fixture/GetGroupCertificateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroupCertificateConfiguration)

responseListGroupVersions :: ListGroupVersionsResponse -> TestTree
responseListGroupVersions =
  res
    "ListGroupVersionsResponse"
    "fixture/ListGroupVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroupVersions)

responseListFunctionDefinitionVersions :: ListFunctionDefinitionVersionsResponse -> TestTree
responseListFunctionDefinitionVersions =
  res
    "ListFunctionDefinitionVersionsResponse"
    "fixture/ListFunctionDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctionDefinitionVersions)

responseListDeviceDefinitions :: ListDeviceDefinitionsResponse -> TestTree
responseListDeviceDefinitions =
  res
    "ListDeviceDefinitionsResponse"
    "fixture/ListDeviceDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeviceDefinitions)

responseAssociateRoleToGroup :: AssociateRoleToGroupResponse -> TestTree
responseAssociateRoleToGroup =
  res
    "AssociateRoleToGroupResponse"
    "fixture/AssociateRoleToGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateRoleToGroup)

responseUpdateCoreDefinition :: UpdateCoreDefinitionResponse -> TestTree
responseUpdateCoreDefinition =
  res
    "UpdateCoreDefinitionResponse"
    "fixture/UpdateCoreDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCoreDefinition)

responseDeleteCoreDefinition :: DeleteCoreDefinitionResponse -> TestTree
responseDeleteCoreDefinition =
  res
    "DeleteCoreDefinitionResponse"
    "fixture/DeleteCoreDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCoreDefinition)

responseGetLoggerDefinition :: GetLoggerDefinitionResponse -> TestTree
responseGetLoggerDefinition =
  res
    "GetLoggerDefinitionResponse"
    "fixture/GetLoggerDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoggerDefinition)

responseListGroupCertificateAuthorities :: ListGroupCertificateAuthoritiesResponse -> TestTree
responseListGroupCertificateAuthorities =
  res
    "ListGroupCertificateAuthoritiesResponse"
    "fixture/ListGroupCertificateAuthoritiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroupCertificateAuthorities)

responseDisassociateRoleFromGroup :: DisassociateRoleFromGroupResponse -> TestTree
responseDisassociateRoleFromGroup =
  res
    "DisassociateRoleFromGroupResponse"
    "fixture/DisassociateRoleFromGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateRoleFromGroup)

responseUpdateSubscriptionDefinition :: UpdateSubscriptionDefinitionResponse -> TestTree
responseUpdateSubscriptionDefinition =
  res
    "UpdateSubscriptionDefinitionResponse"
    "fixture/UpdateSubscriptionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSubscriptionDefinition)

responseDeleteSubscriptionDefinition :: DeleteSubscriptionDefinitionResponse -> TestTree
responseDeleteSubscriptionDefinition =
  res
    "DeleteSubscriptionDefinitionResponse"
    "fixture/DeleteSubscriptionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSubscriptionDefinition)

responseListCoreDefinitions :: ListCoreDefinitionsResponse -> TestTree
responseListCoreDefinitions =
  res
    "ListCoreDefinitionsResponse"
    "fixture/ListCoreDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCoreDefinitions)

responseListSubscriptionDefinitions :: ListSubscriptionDefinitionsResponse -> TestTree
responseListSubscriptionDefinitions =
  res
    "ListSubscriptionDefinitionsResponse"
    "fixture/ListSubscriptionDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSubscriptionDefinitions)

responseCreateGroupCertificateAuthority :: CreateGroupCertificateAuthorityResponse -> TestTree
responseCreateGroupCertificateAuthority =
  res
    "CreateGroupCertificateAuthorityResponse"
    "fixture/CreateGroupCertificateAuthorityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGroupCertificateAuthority)

responseDeleteConnectorDefinition :: DeleteConnectorDefinitionResponse -> TestTree
responseDeleteConnectorDefinition =
  res
    "DeleteConnectorDefinitionResponse"
    "fixture/DeleteConnectorDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConnectorDefinition)

responseUpdateConnectorDefinition :: UpdateConnectorDefinitionResponse -> TestTree
responseUpdateConnectorDefinition =
  res
    "UpdateConnectorDefinitionResponse"
    "fixture/UpdateConnectorDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConnectorDefinition)

responseCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersionResponse -> TestTree
responseCreateLoggerDefinitionVersion =
  res
    "CreateLoggerDefinitionVersionResponse"
    "fixture/CreateLoggerDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoggerDefinitionVersion)

responseCreateCoreDefinition :: CreateCoreDefinitionResponse -> TestTree
responseCreateCoreDefinition =
  res
    "CreateCoreDefinitionResponse"
    "fixture/CreateCoreDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCoreDefinition)

responseGetConnectorDefinitionVersion :: GetConnectorDefinitionVersionResponse -> TestTree
responseGetConnectorDefinitionVersion =
  res
    "GetConnectorDefinitionVersionResponse"
    "fixture/GetConnectorDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConnectorDefinitionVersion)

responseUpdateConnectivityInfo :: UpdateConnectivityInfoResponse -> TestTree
responseUpdateConnectivityInfo =
  res
    "UpdateConnectivityInfoResponse"
    "fixture/UpdateConnectivityInfoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConnectivityInfo)

responseCreateSubscriptionDefinition :: CreateSubscriptionDefinitionResponse -> TestTree
responseCreateSubscriptionDefinition =
  res
    "CreateSubscriptionDefinitionResponse"
    "fixture/CreateSubscriptionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSubscriptionDefinition)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetGroupCertificateAuthority :: GetGroupCertificateAuthorityResponse -> TestTree
responseGetGroupCertificateAuthority =
  res
    "GetGroupCertificateAuthorityResponse"
    "fixture/GetGroupCertificateAuthorityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroupCertificateAuthority)

responseGetLoggerDefinitionVersion :: GetLoggerDefinitionVersionResponse -> TestTree
responseGetLoggerDefinitionVersion =
  res
    "GetLoggerDefinitionVersionResponse"
    "fixture/GetLoggerDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoggerDefinitionVersion)

responseGetServiceRoleForAccount :: GetServiceRoleForAccountResponse -> TestTree
responseGetServiceRoleForAccount =
  res
    "GetServiceRoleForAccountResponse"
    "fixture/GetServiceRoleForAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetServiceRoleForAccount)

responseListConnectorDefinitionVersions :: ListConnectorDefinitionVersionsResponse -> TestTree
responseListConnectorDefinitionVersions =
  res
    "ListConnectorDefinitionVersionsResponse"
    "fixture/ListConnectorDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConnectorDefinitionVersions)

responseCreateSoftwareUpdateJob :: CreateSoftwareUpdateJobResponse -> TestTree
responseCreateSoftwareUpdateJob =
  res
    "CreateSoftwareUpdateJobResponse"
    "fixture/CreateSoftwareUpdateJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSoftwareUpdateJob)

responseCreateLoggerDefinition :: CreateLoggerDefinitionResponse -> TestTree
responseCreateLoggerDefinition =
  res
    "CreateLoggerDefinitionResponse"
    "fixture/CreateLoggerDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoggerDefinition)

responseGetConnectivityInfo :: GetConnectivityInfoResponse -> TestTree
responseGetConnectivityInfo =
  res
    "GetConnectivityInfoResponse"
    "fixture/GetConnectivityInfoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConnectivityInfo)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeployment)

responseDeleteLoggerDefinition :: DeleteLoggerDefinitionResponse -> TestTree
responseDeleteLoggerDefinition =
  res
    "DeleteLoggerDefinitionResponse"
    "fixture/DeleteLoggerDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoggerDefinition)

responseUpdateLoggerDefinition :: UpdateLoggerDefinitionResponse -> TestTree
responseUpdateLoggerDefinition =
  res
    "UpdateLoggerDefinitionResponse"
    "fixture/UpdateLoggerDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateLoggerDefinition)

responseGetSubscriptionDefinition :: GetSubscriptionDefinitionResponse -> TestTree
responseGetSubscriptionDefinition =
  res
    "GetSubscriptionDefinitionResponse"
    "fixture/GetSubscriptionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSubscriptionDefinition)

responseGetCoreDefinition :: GetCoreDefinitionResponse -> TestTree
responseGetCoreDefinition =
  res
    "GetCoreDefinitionResponse"
    "fixture/GetCoreDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCoreDefinition)

responseCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersionResponse -> TestTree
responseCreateConnectorDefinitionVersion =
  res
    "CreateConnectorDefinitionVersionResponse"
    "fixture/CreateConnectorDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConnectorDefinitionVersion)

responseGetDeploymentStatus :: GetDeploymentStatusResponse -> TestTree
responseGetDeploymentStatus =
  res
    "GetDeploymentStatusResponse"
    "fixture/GetDeploymentStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDeploymentStatus)

responseGetBulkDeploymentStatus :: GetBulkDeploymentStatusResponse -> TestTree
responseGetBulkDeploymentStatus =
  res
    "GetBulkDeploymentStatusResponse"
    "fixture/GetBulkDeploymentStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBulkDeploymentStatus)

responseCreateResourceDefinition :: CreateResourceDefinitionResponse -> TestTree
responseCreateResourceDefinition =
  res
    "CreateResourceDefinitionResponse"
    "fixture/CreateResourceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateResourceDefinition)

responseGetResourceDefinitionVersion :: GetResourceDefinitionVersionResponse -> TestTree
responseGetResourceDefinitionVersion =
  res
    "GetResourceDefinitionVersionResponse"
    "fixture/GetResourceDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResourceDefinitionVersion)

responseUpdateFunctionDefinition :: UpdateFunctionDefinitionResponse -> TestTree
responseUpdateFunctionDefinition =
  res
    "UpdateFunctionDefinitionResponse"
    "fixture/UpdateFunctionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFunctionDefinition)

responseDeleteFunctionDefinition :: DeleteFunctionDefinitionResponse -> TestTree
responseDeleteFunctionDefinition =
  res
    "DeleteFunctionDefinitionResponse"
    "fixture/DeleteFunctionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFunctionDefinition)

responseListResourceDefinitions :: ListResourceDefinitionsResponse -> TestTree
responseListResourceDefinitions =
  res
    "ListResourceDefinitionsResponse"
    "fixture/ListResourceDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResourceDefinitions)

responseStopBulkDeployment :: StopBulkDeploymentResponse -> TestTree
responseStopBulkDeployment =
  res
    "StopBulkDeploymentResponse"
    "fixture/StopBulkDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopBulkDeployment)

responseCreateResourceDefinitionVersion :: CreateResourceDefinitionVersionResponse -> TestTree
responseCreateResourceDefinitionVersion =
  res
    "CreateResourceDefinitionVersionResponse"
    "fixture/CreateResourceDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateResourceDefinitionVersion)

responseGetResourceDefinition :: GetResourceDefinitionResponse -> TestTree
responseGetResourceDefinition =
  res
    "GetResourceDefinitionResponse"
    "fixture/GetResourceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResourceDefinition)

responseListResourceDefinitionVersions :: ListResourceDefinitionVersionsResponse -> TestTree
responseListResourceDefinitionVersions =
  res
    "ListResourceDefinitionVersionsResponse"
    "fixture/ListResourceDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResourceDefinitionVersions)

responseDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccountResponse -> TestTree
responseDisassociateServiceRoleFromAccount =
  res
    "DisassociateServiceRoleFromAccountResponse"
    "fixture/DisassociateServiceRoleFromAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateServiceRoleFromAccount)

responseDeleteDeviceDefinition :: DeleteDeviceDefinitionResponse -> TestTree
responseDeleteDeviceDefinition =
  res
    "DeleteDeviceDefinitionResponse"
    "fixture/DeleteDeviceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDeviceDefinition)

responseUpdateDeviceDefinition :: UpdateDeviceDefinitionResponse -> TestTree
responseUpdateDeviceDefinition =
  res
    "UpdateDeviceDefinitionResponse"
    "fixture/UpdateDeviceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDeviceDefinition)

responseAssociateServiceRoleToAccount :: AssociateServiceRoleToAccountResponse -> TestTree
responseAssociateServiceRoleToAccount =
  res
    "AssociateServiceRoleToAccountResponse"
    "fixture/AssociateServiceRoleToAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateServiceRoleToAccount)

responseResetDeployments :: ResetDeploymentsResponse -> TestTree
responseResetDeployments =
  res
    "ResetDeploymentsResponse"
    "fixture/ResetDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetDeployments)

responseListConnectorDefinitions :: ListConnectorDefinitionsResponse -> TestTree
responseListConnectorDefinitions =
  res
    "ListConnectorDefinitionsResponse"
    "fixture/ListConnectorDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListConnectorDefinitions)

responseGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersionResponse -> TestTree
responseGetSubscriptionDefinitionVersion =
  res
    "GetSubscriptionDefinitionVersionResponse"
    "fixture/GetSubscriptionDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSubscriptionDefinitionVersion)

responseGetAssociatedRole :: GetAssociatedRoleResponse -> TestTree
responseGetAssociatedRole =
  res
    "GetAssociatedRoleResponse"
    "fixture/GetAssociatedRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAssociatedRole)

responseListLoggerDefinitionVersions :: ListLoggerDefinitionVersionsResponse -> TestTree
responseListLoggerDefinitionVersions =
  res
    "ListLoggerDefinitionVersionsResponse"
    "fixture/ListLoggerDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLoggerDefinitionVersions)

responseCreateConnectorDefinition :: CreateConnectorDefinitionResponse -> TestTree
responseCreateConnectorDefinition =
  res
    "CreateConnectorDefinitionResponse"
    "fixture/CreateConnectorDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConnectorDefinition)

responseGetCoreDefinitionVersion :: GetCoreDefinitionVersionResponse -> TestTree
responseGetCoreDefinitionVersion =
  res
    "GetCoreDefinitionVersionResponse"
    "fixture/GetCoreDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCoreDefinitionVersion)

responseListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersionsResponse -> TestTree
responseListSubscriptionDefinitionVersions =
  res
    "ListSubscriptionDefinitionVersionsResponse"
    "fixture/ListSubscriptionDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSubscriptionDefinitionVersions)

responseListCoreDefinitionVersions :: ListCoreDefinitionVersionsResponse -> TestTree
responseListCoreDefinitionVersions =
  res
    "ListCoreDefinitionVersionsResponse"
    "fixture/ListCoreDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCoreDefinitionVersions)

responseCreateCoreDefinitionVersion :: CreateCoreDefinitionVersionResponse -> TestTree
responseCreateCoreDefinitionVersion =
  res
    "CreateCoreDefinitionVersionResponse"
    "fixture/CreateCoreDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCoreDefinitionVersion)

responseListBulkDeployments :: ListBulkDeploymentsResponse -> TestTree
responseListBulkDeployments =
  res
    "ListBulkDeploymentsResponse"
    "fixture/ListBulkDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBulkDeployments)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeployments)

responseGetConnectorDefinition :: GetConnectorDefinitionResponse -> TestTree
responseGetConnectorDefinition =
  res
    "GetConnectorDefinitionResponse"
    "fixture/GetConnectorDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConnectorDefinition)

responseListLoggerDefinitions :: ListLoggerDefinitionsResponse -> TestTree
responseListLoggerDefinitions =
  res
    "ListLoggerDefinitionsResponse"
    "fixture/ListLoggerDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLoggerDefinitions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersionResponse -> TestTree
responseCreateSubscriptionDefinitionVersion =
  res
    "CreateSubscriptionDefinitionVersionResponse"
    "fixture/CreateSubscriptionDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSubscriptionDefinitionVersion)

responseGetGroupVersion :: GetGroupVersionResponse -> TestTree
responseGetGroupVersion =
  res
    "GetGroupVersionResponse"
    "fixture/GetGroupVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroupVersion)

responseUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfigurationResponse -> TestTree
responseUpdateGroupCertificateConfiguration =
  res
    "UpdateGroupCertificateConfigurationResponse"
    "fixture/UpdateGroupCertificateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGroupCertificateConfiguration)

responseGetFunctionDefinitionVersion :: GetFunctionDefinitionVersionResponse -> TestTree
responseGetFunctionDefinitionVersion =
  res
    "GetFunctionDefinitionVersionResponse"
    "fixture/GetFunctionDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionDefinitionVersion)

responseGetDeviceDefinition :: GetDeviceDefinitionResponse -> TestTree
responseGetDeviceDefinition =
  res
    "GetDeviceDefinitionResponse"
    "fixture/GetDeviceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDeviceDefinition)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGroup)

responseCreateFunctionDefinition :: CreateFunctionDefinitionResponse -> TestTree
responseCreateFunctionDefinition =
  res
    "CreateFunctionDefinitionResponse"
    "fixture/CreateFunctionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFunctionDefinition)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersionResponse -> TestTree
responseCreateDeviceDefinitionVersion =
  res
    "CreateDeviceDefinitionVersionResponse"
    "fixture/CreateDeviceDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeviceDefinitionVersion)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGroups)

responseListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReportsResponse -> TestTree
responseListBulkDeploymentDetailedReports =
  res
    "ListBulkDeploymentDetailedReportsResponse"
    "fixture/ListBulkDeploymentDetailedReportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBulkDeploymentDetailedReports)

responseGetThingRuntimeConfiguration :: GetThingRuntimeConfigurationResponse -> TestTree
responseGetThingRuntimeConfiguration =
  res
    "GetThingRuntimeConfigurationResponse"
    "fixture/GetThingRuntimeConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetThingRuntimeConfiguration)

responseDeleteResourceDefinition :: DeleteResourceDefinitionResponse -> TestTree
responseDeleteResourceDefinition =
  res
    "DeleteResourceDefinitionResponse"
    "fixture/DeleteResourceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResourceDefinition)

responseUpdateResourceDefinition :: UpdateResourceDefinitionResponse -> TestTree
responseUpdateResourceDefinition =
  res
    "UpdateResourceDefinitionResponse"
    "fixture/UpdateResourceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateResourceDefinition)

responseListDeviceDefinitionVersions :: ListDeviceDefinitionVersionsResponse -> TestTree
responseListDeviceDefinitionVersions =
  res
    "ListDeviceDefinitionVersionsResponse"
    "fixture/ListDeviceDefinitionVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDeviceDefinitionVersions)

responseListFunctionDefinitions :: ListFunctionDefinitionsResponse -> TestTree
responseListFunctionDefinitions =
  res
    "ListFunctionDefinitionsResponse"
    "fixture/ListFunctionDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFunctionDefinitions)

responseGetFunctionDefinition :: GetFunctionDefinitionResponse -> TestTree
responseGetFunctionDefinition =
  res
    "GetFunctionDefinitionResponse"
    "fixture/GetFunctionDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFunctionDefinition)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroup)

responseCreateDeviceDefinition :: CreateDeviceDefinitionResponse -> TestTree
responseCreateDeviceDefinition =
  res
    "CreateDeviceDefinitionResponse"
    "fixture/CreateDeviceDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeviceDefinition)

responseCreateGroupVersion :: CreateGroupVersionResponse -> TestTree
responseCreateGroupVersion =
  res
    "CreateGroupVersionResponse"
    "fixture/CreateGroupVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGroupVersion)

responseCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersionResponse -> TestTree
responseCreateFunctionDefinitionVersion =
  res
    "CreateFunctionDefinitionVersionResponse"
    "fixture/CreateFunctionDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFunctionDefinitionVersion)

responseStartBulkDeployment :: StartBulkDeploymentResponse -> TestTree
responseStartBulkDeployment =
  res
    "StartBulkDeploymentResponse"
    "fixture/StartBulkDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartBulkDeployment)

responseUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfigurationResponse -> TestTree
responseUpdateThingRuntimeConfiguration =
  res
    "UpdateThingRuntimeConfigurationResponse"
    "fixture/UpdateThingRuntimeConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateThingRuntimeConfiguration)

responseGetDeviceDefinitionVersion :: GetDeviceDefinitionVersionResponse -> TestTree
responseGetDeviceDefinitionVersion =
  res
    "GetDeviceDefinitionVersionResponse"
    "fixture/GetDeviceDefinitionVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDeviceDefinitionVersion)
