{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: @TagsToAdd@ for tags to add or update, and @TagsToRemove@ .
--
-- Elastic Beanstalk supports tagging of all of its resources. For details about resource tagging, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources> .
-- If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:
--
--     * elasticbeanstalk:AddTags
--
--     * Controls permission to call @UpdateTagsForResource@ and pass a list of tags to add in the @TagsToAdd@ parameter.
--
--
--     * elasticbeanstalk:RemoveTags
--
--     * Controls permission to call @UpdateTagsForResource@ and pass a list of tag keys to remove in the @TagsToRemove@ parameter.
--
--
-- For details about creating a custom user policy, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies Creating a Custom User Policy> .
module Network.AWS.ElasticBeanstalk.UpdateTagsForResource
  ( -- * Creating a request
    UpdateTagsForResource (..),
    mkUpdateTagsForResource,

    -- ** Request lenses
    utfrResourceArn,
    utfrTagsToAdd,
    utfrTagsToRemove,

    -- * Destructuring the response
    UpdateTagsForResourceResponse (..),
    mkUpdateTagsForResourceResponse,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTagsForResource' smart constructor.
data UpdateTagsForResource = UpdateTagsForResource'
  { -- | The Amazon Resource Name (ARN) of the resouce to be updated.
    --
    -- Must be the ARN of an Elastic Beanstalk resource.
    resourceArn :: Types.ResourceArn,
    -- | A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
    --
    -- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
    tagsToAdd :: Core.Maybe [Types.Tag],
    -- | A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
    --
    -- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
    tagsToRemove :: Core.Maybe [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagsForResource' value with any optional fields omitted.
mkUpdateTagsForResource ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  UpdateTagsForResource
mkUpdateTagsForResource resourceArn =
  UpdateTagsForResource'
    { resourceArn,
      tagsToAdd = Core.Nothing,
      tagsToRemove = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrResourceArn :: Lens.Lens' UpdateTagsForResource Types.ResourceArn
utfrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED utfrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
--
-- /Note:/ Consider using 'tagsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrTagsToAdd :: Lens.Lens' UpdateTagsForResource (Core.Maybe [Types.Tag])
utfrTagsToAdd = Lens.field @"tagsToAdd"
{-# DEPRECATED utfrTagsToAdd "Use generic-lens or generic-optics with 'tagsToAdd' instead." #-}

-- | A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
--
-- /Note:/ Consider using 'tagsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrTagsToRemove :: Lens.Lens' UpdateTagsForResource (Core.Maybe [Types.TagKey])
utfrTagsToRemove = Lens.field @"tagsToRemove"
{-# DEPRECATED utfrTagsToRemove "Use generic-lens or generic-optics with 'tagsToRemove' instead." #-}

instance Core.AWSRequest UpdateTagsForResource where
  type Rs UpdateTagsForResource = UpdateTagsForResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateTagsForResource")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ResourceArn" resourceArn)
                Core.<> ( Core.toQueryValue
                            "TagsToAdd"
                            (Core.toQueryList "member" Core.<$> tagsToAdd)
                        )
                Core.<> ( Core.toQueryValue
                            "TagsToRemove"
                            (Core.toQueryList "member" Core.<$> tagsToRemove)
                        )
            )
      }
  response = Response.receiveNull UpdateTagsForResourceResponse'

-- | /See:/ 'mkUpdateTagsForResourceResponse' smart constructor.
data UpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagsForResourceResponse' value with any optional fields omitted.
mkUpdateTagsForResourceResponse ::
  UpdateTagsForResourceResponse
mkUpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
