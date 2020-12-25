{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from your private CA. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this action, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. To add tags to a private CA, use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_TagCertificateAuthority.html TagCertificateAuthority> . Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListTags.html ListTags> action to see what tags are associated with your CA.
module Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
  ( -- * Creating a request
    UntagCertificateAuthority (..),
    mkUntagCertificateAuthority,

    -- ** Request lenses
    uCertificateAuthorityArn,
    uTags,

    -- * Destructuring the response
    UntagCertificateAuthorityResponse (..),
    mkUntagCertificateAuthorityResponse,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagCertificateAuthority' smart constructor.
data UntagCertificateAuthority = UntagCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityArn :: Types.Arn,
    -- | List of tags to be removed from the CA.
    tags :: Core.NonEmpty Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagCertificateAuthority' value with any optional fields omitted.
mkUntagCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Types.Arn ->
  -- | 'tags'
  Core.NonEmpty Types.Tag ->
  UntagCertificateAuthority
mkUntagCertificateAuthority certificateAuthorityArn tags =
  UntagCertificateAuthority' {certificateAuthorityArn, tags}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCertificateAuthorityArn :: Lens.Lens' UntagCertificateAuthority Types.Arn
uCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED uCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | List of tags to be removed from the CA.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' UntagCertificateAuthority (Core.NonEmpty Types.Tag)
uTags = Lens.field @"tags"
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UntagCertificateAuthority where
  toJSON UntagCertificateAuthority {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest UntagCertificateAuthority where
  type
    Rs UntagCertificateAuthority =
      UntagCertificateAuthorityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ACMPrivateCA.UntagCertificateAuthority")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UntagCertificateAuthorityResponse'

-- | /See:/ 'mkUntagCertificateAuthorityResponse' smart constructor.
data UntagCertificateAuthorityResponse = UntagCertificateAuthorityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagCertificateAuthorityResponse' value with any optional fields omitted.
mkUntagCertificateAuthorityResponse ::
  UntagCertificateAuthorityResponse
mkUntagCertificateAuthorityResponse =
  UntagCertificateAuthorityResponse'
