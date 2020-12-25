{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeProjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes the models in an Amazon Rekognition Custom Labels project. You can specify up to 10 model versions in @ProjectVersionArns@ . If you don't specify a value, descriptions for all models are returned.
--
-- This operation requires permissions to perform the @rekognition:DescribeProjectVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjectVersions
  ( -- * Creating a request
    DescribeProjectVersions (..),
    mkDescribeProjectVersions,

    -- ** Request lenses
    dpvProjectArn,
    dpvMaxResults,
    dpvNextToken,
    dpvVersionNames,

    -- * Destructuring the response
    DescribeProjectVersionsResponse (..),
    mkDescribeProjectVersionsResponse,

    -- ** Response lenses
    dpvrrsNextToken,
    dpvrrsProjectVersionDescriptions,
    dpvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProjectVersions' smart constructor.
data DescribeProjectVersions = DescribeProjectVersions'
  { -- | The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
    projectArn :: Types.ProjectArn,
    -- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
    versionNames :: Core.Maybe (Core.NonEmpty Types.VersionName)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProjectVersions' value with any optional fields omitted.
mkDescribeProjectVersions ::
  -- | 'projectArn'
  Types.ProjectArn ->
  DescribeProjectVersions
mkDescribeProjectVersions projectArn =
  DescribeProjectVersions'
    { projectArn,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      versionNames = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project that contains the models you want to describe.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvProjectArn :: Lens.Lens' DescribeProjectVersions Types.ProjectArn
dpvProjectArn = Lens.field @"projectArn"
{-# DEPRECATED dpvProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The maximum number of results to return per paginated call. The largest value you can specify is 100. If you specify a value greater than 100, a ValidationException error occurs. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvMaxResults :: Lens.Lens' DescribeProjectVersions (Core.Maybe Core.Natural)
dpvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvNextToken :: Lens.Lens' DescribeProjectVersions (Core.Maybe Types.NextToken)
dpvNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of model version names that you want to describe. You can add up to 10 model version names to the list. If you don't specify a value, all model descriptions are returned. A version name is part of a model (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the version name in the following ARN. @arn:aws:rekognition:us-east-1:123456789012:project/getting-started/version//my-model.2020-01-21T09.10.15/ /1234567890123@ .
--
-- /Note:/ Consider using 'versionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvVersionNames :: Lens.Lens' DescribeProjectVersions (Core.Maybe (Core.NonEmpty Types.VersionName))
dpvVersionNames = Lens.field @"versionNames"
{-# DEPRECATED dpvVersionNames "Use generic-lens or generic-optics with 'versionNames' instead." #-}

instance Core.FromJSON DescribeProjectVersions where
  toJSON DescribeProjectVersions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProjectArn" Core..= projectArn),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("VersionNames" Core..=) Core.<$> versionNames
          ]
      )

instance Core.AWSRequest DescribeProjectVersions where
  type Rs DescribeProjectVersions = DescribeProjectVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.DescribeProjectVersions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ProjectVersionDescriptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeProjectVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"projectVersionDescriptions" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeProjectVersionsResponse' smart constructor.
data DescribeProjectVersionsResponse = DescribeProjectVersionsResponse'
  { -- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
    projectVersionDescriptions :: Core.Maybe [Types.ProjectVersionDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProjectVersionsResponse' value with any optional fields omitted.
mkDescribeProjectVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProjectVersionsResponse
mkDescribeProjectVersionsResponse responseStatus =
  DescribeProjectVersionsResponse'
    { nextToken = Core.Nothing,
      projectVersionDescriptions = Core.Nothing,
      responseStatus
    }

-- | If the previous response was incomplete (because there is more results to retrieve), Amazon Rekognition Custom Labels returns a pagination token in the response. You can use this pagination token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsNextToken :: Lens.Lens' DescribeProjectVersionsResponse (Core.Maybe Types.NextToken)
dpvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of model descriptions. The list is sorted by the creation date and time of the model versions, latest to earliest.
--
-- /Note:/ Consider using 'projectVersionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsProjectVersionDescriptions :: Lens.Lens' DescribeProjectVersionsResponse (Core.Maybe [Types.ProjectVersionDescription])
dpvrrsProjectVersionDescriptions = Lens.field @"projectVersionDescriptions"
{-# DEPRECATED dpvrrsProjectVersionDescriptions "Use generic-lens or generic-optics with 'projectVersionDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsResponseStatus :: Lens.Lens' DescribeProjectVersionsResponse Core.Int
dpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
