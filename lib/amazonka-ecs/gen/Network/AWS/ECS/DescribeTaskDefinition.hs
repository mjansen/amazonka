{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@ to find information about a specific task definition, or you can simply specify the family to find the latest @ACTIVE@ revision in that family.
module Network.AWS.ECS.DescribeTaskDefinition
  ( -- * Creating a request
    DescribeTaskDefinition (..),
    mkDescribeTaskDefinition,

    -- ** Request lenses
    dtdTaskDefinition,
    dtdInclude,

    -- * Destructuring the response
    DescribeTaskDefinitionResponse (..),
    mkDescribeTaskDefinitionResponse,

    -- ** Response lenses
    dtdrfrsTags,
    dtdrfrsTaskDefinition,
    dtdrfrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTaskDefinition' smart constructor.
data DescribeTaskDefinition = DescribeTaskDefinition'
  { -- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@ (@family:revision@ ) for a specific revision in the family, or full Amazon Resource Name (ARN) of the task definition to describe.
    taskDefinition :: Types.String,
    -- | Specifies whether to see the resource tags for the task definition. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Core.Maybe [Types.TaskDefinitionField]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTaskDefinition' value with any optional fields omitted.
mkDescribeTaskDefinition ::
  -- | 'taskDefinition'
  Types.String ->
  DescribeTaskDefinition
mkDescribeTaskDefinition taskDefinition =
  DescribeTaskDefinition' {taskDefinition, include = Core.Nothing}

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@ (@family:revision@ ) for a specific revision in the family, or full Amazon Resource Name (ARN) of the task definition to describe.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdTaskDefinition :: Lens.Lens' DescribeTaskDefinition Types.String
dtdTaskDefinition = Lens.field @"taskDefinition"
{-# DEPRECATED dtdTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | Specifies whether to see the resource tags for the task definition. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdInclude :: Lens.Lens' DescribeTaskDefinition (Core.Maybe [Types.TaskDefinitionField])
dtdInclude = Lens.field @"include"
{-# DEPRECATED dtdInclude "Use generic-lens or generic-optics with 'include' instead." #-}

instance Core.FromJSON DescribeTaskDefinition where
  toJSON DescribeTaskDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("taskDefinition" Core..= taskDefinition),
            ("include" Core..=) Core.<$> include
          ]
      )

instance Core.AWSRequest DescribeTaskDefinition where
  type Rs DescribeTaskDefinition = DescribeTaskDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DescribeTaskDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskDefinitionResponse'
            Core.<$> (x Core..:? "tags")
            Core.<*> (x Core..:? "taskDefinition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTaskDefinitionResponse' smart constructor.
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
  { -- | The metadata that is applied to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag],
    -- | The full task definition description.
    taskDefinition :: Core.Maybe Types.TaskDefinition,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTaskDefinitionResponse' value with any optional fields omitted.
mkDescribeTaskDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTaskDefinitionResponse
mkDescribeTaskDefinitionResponse responseStatus =
  DescribeTaskDefinitionResponse'
    { tags = Core.Nothing,
      taskDefinition = Core.Nothing,
      responseStatus
    }

-- | The metadata that is applied to the task definition to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrfrsTags :: Lens.Lens' DescribeTaskDefinitionResponse (Core.Maybe [Types.Tag])
dtdrfrsTags = Lens.field @"tags"
{-# DEPRECATED dtdrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The full task definition description.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrfrsTaskDefinition :: Lens.Lens' DescribeTaskDefinitionResponse (Core.Maybe Types.TaskDefinition)
dtdrfrsTaskDefinition = Lens.field @"taskDefinition"
{-# DEPRECATED dtdrfrsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrfrsResponseStatus :: Lens.Lens' DescribeTaskDefinitionResponse Core.Int
dtdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
