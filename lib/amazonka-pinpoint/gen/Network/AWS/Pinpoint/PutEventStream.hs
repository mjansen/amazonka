{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.PutEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event stream for an application or updates the settings of an existing event stream for an application.
module Network.AWS.Pinpoint.PutEventStream
  ( -- * Creating a request
    PutEventStream (..),
    mkPutEventStream,

    -- ** Request lenses
    pesApplicationId,
    pesWriteEventStream,

    -- * Destructuring the response
    PutEventStreamResponse (..),
    mkPutEventStreamResponse,

    -- ** Response lenses
    pesrrsEventStream,
    pesrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutEventStream' smart constructor.
data PutEventStream = PutEventStream'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeEventStream :: Types.WriteEventStream
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventStream' value with any optional fields omitted.
mkPutEventStream ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'writeEventStream'
  Types.WriteEventStream ->
  PutEventStream
mkPutEventStream applicationId writeEventStream =
  PutEventStream' {applicationId, writeEventStream}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesApplicationId :: Lens.Lens' PutEventStream Core.Text
pesApplicationId = Lens.field @"applicationId"
{-# DEPRECATED pesApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeEventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesWriteEventStream :: Lens.Lens' PutEventStream Types.WriteEventStream
pesWriteEventStream = Lens.field @"writeEventStream"
{-# DEPRECATED pesWriteEventStream "Use generic-lens or generic-optics with 'writeEventStream' instead." #-}

instance Core.FromJSON PutEventStream where
  toJSON PutEventStream {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WriteEventStream" Core..= writeEventStream)]
      )

instance Core.AWSRequest PutEventStream where
  type Rs PutEventStream = PutEventStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/eventstream")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventStreamResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutEventStreamResponse' smart constructor.
data PutEventStreamResponse = PutEventStreamResponse'
  { eventStream :: Types.EventStream,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventStreamResponse' value with any optional fields omitted.
mkPutEventStreamResponse ::
  -- | 'eventStream'
  Types.EventStream ->
  -- | 'responseStatus'
  Core.Int ->
  PutEventStreamResponse
mkPutEventStreamResponse eventStream responseStatus =
  PutEventStreamResponse' {eventStream, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsEventStream :: Lens.Lens' PutEventStreamResponse Types.EventStream
pesrrsEventStream = Lens.field @"eventStream"
{-# DEPRECATED pesrrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsResponseStatus :: Lens.Lens' PutEventStreamResponse Core.Int
pesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
