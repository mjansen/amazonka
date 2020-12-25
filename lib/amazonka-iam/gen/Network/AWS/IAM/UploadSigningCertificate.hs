{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an X.509 signing certificate and associates it with the specified IAM user. Some AWS services use X.509 signing certificates to validate requests that are signed with a corresponding private key. When you upload the certificate, its default status is @Active@ .
--
-- If the @UserName@ is not specified, the IAM user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.UploadSigningCertificate
  ( -- * Creating a request
    UploadSigningCertificate (..),
    mkUploadSigningCertificate,

    -- ** Request lenses
    uscfCertificateBody,
    uscfUserName,

    -- * Destructuring the response
    UploadSigningCertificateResponse (..),
    mkUploadSigningCertificateResponse,

    -- ** Response lenses
    uscrrsCertificate,
    uscrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUploadSigningCertificate' smart constructor.
data UploadSigningCertificate = UploadSigningCertificate'
  { -- | The contents of the signing certificate.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    certificateBody :: Types.CertificateBodyType,
    -- | The name of the user the signing certificate is for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadSigningCertificate' value with any optional fields omitted.
mkUploadSigningCertificate ::
  -- | 'certificateBody'
  Types.CertificateBodyType ->
  UploadSigningCertificate
mkUploadSigningCertificate certificateBody =
  UploadSigningCertificate'
    { certificateBody,
      userName = Core.Nothing
    }

-- | The contents of the signing certificate.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscfCertificateBody :: Lens.Lens' UploadSigningCertificate Types.CertificateBodyType
uscfCertificateBody = Lens.field @"certificateBody"
{-# DEPRECATED uscfCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

-- | The name of the user the signing certificate is for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscfUserName :: Lens.Lens' UploadSigningCertificate (Core.Maybe Types.UserName)
uscfUserName = Lens.field @"userName"
{-# DEPRECATED uscfUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest UploadSigningCertificate where
  type Rs UploadSigningCertificate = UploadSigningCertificateResponse
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
            ( Core.pure ("Action", "UploadSigningCertificate")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "CertificateBody" certificateBody)
                Core.<> (Core.toQueryValue "UserName" Core.<$> userName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UploadSigningCertificateResult"
      ( \s h x ->
          UploadSigningCertificateResponse'
            Core.<$> (x Core..@ "Certificate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'UploadSigningCertificate' request.
--
-- /See:/ 'mkUploadSigningCertificateResponse' smart constructor.
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
  { -- | Information about the certificate.
    certificate :: Types.SigningCertificate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UploadSigningCertificateResponse' value with any optional fields omitted.
mkUploadSigningCertificateResponse ::
  -- | 'certificate'
  Types.SigningCertificate ->
  -- | 'responseStatus'
  Core.Int ->
  UploadSigningCertificateResponse
mkUploadSigningCertificateResponse certificate responseStatus =
  UploadSigningCertificateResponse' {certificate, responseStatus}

-- | Information about the certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsCertificate :: Lens.Lens' UploadSigningCertificateResponse Types.SigningCertificate
uscrrsCertificate = Lens.field @"certificate"
{-# DEPRECATED uscrrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsResponseStatus :: Lens.Lens' UploadSigningCertificateResponse Core.Int
uscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
