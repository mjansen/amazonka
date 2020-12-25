{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified Kinesis data stream. Each time you invoke this operation, you can specify up to 10 tags. If you want to add more than 10 tags to your stream, you can invoke this operation multiple times. In total, each stream can have up to 50 tags.
--
-- If tags have already been assigned to the stream, @AddTagsToStream@ overwrites any existing tags that correspond to the specified tag keys.
-- 'AddTagsToStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.AddTagsToStream
  ( -- * Creating a request
    AddTagsToStream (..),
    mkAddTagsToStream,

    -- ** Request lenses
    attsStreamName,
    attsTags,

    -- * Destructuring the response
    AddTagsToStreamResponse (..),
    mkAddTagsToStreamResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @AddTagsToStream@ .
--
-- /See:/ 'mkAddTagsToStream' smart constructor.
data AddTagsToStream = AddTagsToStream'
  { -- | The name of the stream.
    streamName :: Types.StreamName,
    -- | A set of up to 10 key-value pairs to use to create the tags.
    tags :: Core.HashMap Types.TagKey Types.TagValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToStream' value with any optional fields omitted.
mkAddTagsToStream ::
  -- | 'streamName'
  Types.StreamName ->
  AddTagsToStream
mkAddTagsToStream streamName =
  AddTagsToStream' {streamName, tags = Core.mempty}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attsStreamName :: Lens.Lens' AddTagsToStream Types.StreamName
attsStreamName = Lens.field @"streamName"
{-# DEPRECATED attsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | A set of up to 10 key-value pairs to use to create the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attsTags :: Lens.Lens' AddTagsToStream (Core.HashMap Types.TagKey Types.TagValue)
attsTags = Lens.field @"tags"
{-# DEPRECATED attsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON AddTagsToStream where
  toJSON AddTagsToStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest AddTagsToStream where
  type Rs AddTagsToStream = AddTagsToStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.AddTagsToStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AddTagsToStreamResponse'

-- | /See:/ 'mkAddTagsToStreamResponse' smart constructor.
data AddTagsToStreamResponse = AddTagsToStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToStreamResponse' value with any optional fields omitted.
mkAddTagsToStreamResponse ::
  AddTagsToStreamResponse
mkAddTagsToStreamResponse = AddTagsToStreamResponse'
