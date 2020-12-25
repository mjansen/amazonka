{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeregisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes from the system the certificate that was registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DeregisterCertificate
  ( -- * Creating a request
    DeregisterCertificate (..),
    mkDeregisterCertificate,

    -- ** Request lenses
    dcgDirectoryId,
    dcgCertificateId,

    -- * Destructuring the response
    DeregisterCertificateResponse (..),
    mkDeregisterCertificateResponse,

    -- ** Response lenses
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterCertificate' smart constructor.
data DeregisterCertificate = DeregisterCertificate'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The identifier of the certificate.
    certificateId :: Types.CertificateId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterCertificate' value with any optional fields omitted.
mkDeregisterCertificate ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'certificateId'
  Types.CertificateId ->
  DeregisterCertificate
mkDeregisterCertificate directoryId certificateId =
  DeregisterCertificate' {directoryId, certificateId}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgDirectoryId :: Lens.Lens' DeregisterCertificate Types.DirectoryId
dcgDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dcgDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgCertificateId :: Lens.Lens' DeregisterCertificate Types.CertificateId
dcgCertificateId = Lens.field @"certificateId"
{-# DEPRECATED dcgCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Core.FromJSON DeregisterCertificate where
  toJSON DeregisterCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("CertificateId" Core..= certificateId)
          ]
      )

instance Core.AWSRequest DeregisterCertificate where
  type Rs DeregisterCertificate = DeregisterCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DeregisterCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterCertificateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterCertificateResponse' smart constructor.
newtype DeregisterCertificateResponse = DeregisterCertificateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterCertificateResponse' value with any optional fields omitted.
mkDeregisterCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterCertificateResponse
mkDeregisterCertificateResponse responseStatus =
  DeregisterCertificateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeregisterCertificateResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
