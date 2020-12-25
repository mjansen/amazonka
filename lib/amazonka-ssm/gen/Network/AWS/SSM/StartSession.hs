{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a connection to a target (for example, an instance) for a Session Manager session. Returns a URL and token that can be used to open a WebSocket connection for sending input and receiving outputs.
module Network.AWS.SSM.StartSession
  ( -- * Creating a request
    StartSession (..),
    mkStartSession,

    -- ** Request lenses
    ssTarget,
    ssDocumentName,
    ssParameters,

    -- * Destructuring the response
    StartSessionResponse (..),
    mkStartSessionResponse,

    -- ** Response lenses
    ssrrsSessionId,
    ssrrsStreamUrl,
    ssrrsTokenValue,
    ssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkStartSession' smart constructor.
data StartSession = StartSession'
  { -- | The instance to connect to for the session.
    target :: Types.SessionTarget,
    -- | The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . You can call the 'GetDocument' API to verify the document exists before attempting to start a session. If no document name is provided, a shell to the instance is launched by default.
    documentName :: Core.Maybe Types.DocumentARN,
    -- | Reserved for future use.
    parameters :: Core.Maybe (Core.HashMap Types.SessionManagerParameterName [Types.SessionManagerParameterValue])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSession' value with any optional fields omitted.
mkStartSession ::
  -- | 'target'
  Types.SessionTarget ->
  StartSession
mkStartSession target =
  StartSession'
    { target,
      documentName = Core.Nothing,
      parameters = Core.Nothing
    }

-- | The instance to connect to for the session.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTarget :: Lens.Lens' StartSession Types.SessionTarget
ssTarget = Lens.field @"target"
{-# DEPRECATED ssTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . You can call the 'GetDocument' API to verify the document exists before attempting to start a session. If no document name is provided, a shell to the instance is launched by default.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDocumentName :: Lens.Lens' StartSession (Core.Maybe Types.DocumentARN)
ssDocumentName = Lens.field @"documentName"
{-# DEPRECATED ssDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParameters :: Lens.Lens' StartSession (Core.Maybe (Core.HashMap Types.SessionManagerParameterName [Types.SessionManagerParameterValue]))
ssParameters = Lens.field @"parameters"
{-# DEPRECATED ssParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON StartSession where
  toJSON StartSession {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Target" Core..= target),
            ("DocumentName" Core..=) Core.<$> documentName,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.AWSRequest StartSession where
  type Rs StartSession = StartSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.StartSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSessionResponse'
            Core.<$> (x Core..:? "SessionId")
            Core.<*> (x Core..:? "StreamUrl")
            Core.<*> (x Core..:? "TokenValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { -- | The ID of the session.
    sessionId :: Core.Maybe Types.SessionId,
    -- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@
    --
    -- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
    -- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
    streamUrl :: Core.Maybe Types.StreamUrl,
    -- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
    tokenValue :: Core.Maybe Types.TokenValue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSessionResponse' value with any optional fields omitted.
mkStartSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSessionResponse
mkStartSessionResponse responseStatus =
  StartSessionResponse'
    { sessionId = Core.Nothing,
      streamUrl = Core.Nothing,
      tokenValue = Core.Nothing,
      responseStatus
    }

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsSessionId :: Lens.Lens' StartSessionResponse (Core.Maybe Types.SessionId)
ssrrsSessionId = Lens.field @"sessionId"
{-# DEPRECATED ssrrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- /Note:/ Consider using 'streamUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsStreamUrl :: Lens.Lens' StartSessionResponse (Core.Maybe Types.StreamUrl)
ssrrsStreamUrl = Lens.field @"streamUrl"
{-# DEPRECATED ssrrsStreamUrl "Use generic-lens or generic-optics with 'streamUrl' instead." #-}

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- /Note:/ Consider using 'tokenValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsTokenValue :: Lens.Lens' StartSessionResponse (Core.Maybe Types.TokenValue)
ssrrsTokenValue = Lens.field @"tokenValue"
{-# DEPRECATED ssrrsTokenValue "Use generic-lens or generic-optics with 'tokenValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsResponseStatus :: Lens.Lens' StartSessionResponse Core.Int
ssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
