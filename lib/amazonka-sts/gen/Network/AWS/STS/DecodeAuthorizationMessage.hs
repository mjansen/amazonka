{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decodes additional information about the authorization status of a request from an encoded message returned in response to an AWS request.
--
-- For example, if a user is not authorized to perform an operation that he or she has requested, the request returns a @Client.UnauthorizedOperation@ response (an HTTP 403 response). Some AWS operations additionally return an encoded message that can provide details about this authorization failure.
-- The message is encoded because the details of the authorization status can constitute privileged information that the user who requested the operation should not see. To decode an authorization status message, a user must be granted permissions via an IAM policy to request the @DecodeAuthorizationMessage@ (@sts:DecodeAuthorizationMessage@ ) action.
-- The decoded message includes the following type of information:
--
--     * Whether the request was denied due to an explicit deny or due to the absence of an explicit allow. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-denyallow Determining Whether a Request is Allowed or Denied> in the /IAM User Guide/ .
--
--
--     * The principal who made the request.
--
--
--     * The requested action.
--
--
--     * The requested resource.
--
--
--     * The values of condition keys in the context of the user's request.
module Network.AWS.STS.DecodeAuthorizationMessage
  ( -- * Creating a request
    DecodeAuthorizationMessage (..),
    mkDecodeAuthorizationMessage,

    -- ** Request lenses
    damEncodedMessage,

    -- * Destructuring the response
    DecodeAuthorizationMessageResponse (..),
    mkDecodeAuthorizationMessageResponse,

    -- ** Response lenses
    damrrsDecodedMessage,
    damrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkDecodeAuthorizationMessage' smart constructor.
newtype DecodeAuthorizationMessage = DecodeAuthorizationMessage'
  { -- | The encoded message that was returned with the response.
    encodedMessage :: Types.EncodedMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DecodeAuthorizationMessage' value with any optional fields omitted.
mkDecodeAuthorizationMessage ::
  -- | 'encodedMessage'
  Types.EncodedMessage ->
  DecodeAuthorizationMessage
mkDecodeAuthorizationMessage encodedMessage =
  DecodeAuthorizationMessage' {encodedMessage}

-- | The encoded message that was returned with the response.
--
-- /Note:/ Consider using 'encodedMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damEncodedMessage :: Lens.Lens' DecodeAuthorizationMessage Types.EncodedMessage
damEncodedMessage = Lens.field @"encodedMessage"
{-# DEPRECATED damEncodedMessage "Use generic-lens or generic-optics with 'encodedMessage' instead." #-}

instance Core.AWSRequest DecodeAuthorizationMessage where
  type
    Rs DecodeAuthorizationMessage =
      DecodeAuthorizationMessageResponse
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
            ( Core.pure ("Action", "DecodeAuthorizationMessage")
                Core.<> (Core.pure ("Version", "2011-06-15"))
                Core.<> (Core.toQueryValue "EncodedMessage" encodedMessage)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DecodeAuthorizationMessageResult"
      ( \s h x ->
          DecodeAuthorizationMessageResponse'
            Core.<$> (x Core..@? "DecodedMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A document that contains additional information about the authorization status of a request from an encoded message that is returned in response to an AWS request.
--
-- /See:/ 'mkDecodeAuthorizationMessageResponse' smart constructor.
data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse'
  { -- | An XML document that contains the decoded message.
    decodedMessage :: Core.Maybe Types.DecodedMessageType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecodeAuthorizationMessageResponse' value with any optional fields omitted.
mkDecodeAuthorizationMessageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DecodeAuthorizationMessageResponse
mkDecodeAuthorizationMessageResponse responseStatus =
  DecodeAuthorizationMessageResponse'
    { decodedMessage =
        Core.Nothing,
      responseStatus
    }

-- | An XML document that contains the decoded message.
--
-- /Note:/ Consider using 'decodedMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsDecodedMessage :: Lens.Lens' DecodeAuthorizationMessageResponse (Core.Maybe Types.DecodedMessageType)
damrrsDecodedMessage = Lens.field @"decodedMessage"
{-# DEPRECATED damrrsDecodedMessage "Use generic-lens or generic-optics with 'decodedMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsResponseStatus :: Lens.Lens' DecodeAuthorizationMessageResponse Core.Int
damrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED damrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
