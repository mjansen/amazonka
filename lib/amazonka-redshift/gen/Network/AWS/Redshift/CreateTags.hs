{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a cluster.
--
-- A resource can have up to 50 tags. If you try to create more than 50 tags for a resource, you will receive an error and the attempt will fail.
-- If you specify a key that already exists for the resource, the value for that key will be updated with the new value.
module Network.AWS.Redshift.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctResourceName,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the output from the @CreateTags@ action.
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
    resourceName :: Types.String,
    -- | One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags ::
  -- | 'resourceName'
  Types.String ->
  CreateTags
mkCreateTags resourceName =
  CreateTags' {resourceName, tags = Core.mempty}

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceName :: Lens.Lens' CreateTags Types.String
ctResourceName = Lens.field @"resourceName"
{-# DEPRECATED ctResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Types.Tag]
ctTags = Lens.field @"tags"
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
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
            ( Core.pure ("Action", "CreateTags")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ResourceName" resourceName)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" tags))
            )
      }
  response = Response.receiveNull CreateTagsResponse'

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse ::
  CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
