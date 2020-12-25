{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateDescription
  ( CertificateDescription (..),

    -- * Smart constructor
    mkCertificateDescription,

    -- * Lenses
    cdCaCertificateId,
    cdCertificateArn,
    cdCertificateId,
    cdCertificateMode,
    cdCertificatePem,
    cdCreationDate,
    cdCustomerVersion,
    cdGenerationId,
    cdLastModifiedDate,
    cdOwnedBy,
    cdPreviousOwnedBy,
    cdStatus,
    cdTransferData,
    cdValidity,
  )
where

import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.IoT.Types.CertificateMode as Types
import qualified Network.AWS.IoT.Types.CertificatePem as Types
import qualified Network.AWS.IoT.Types.CertificateStatus as Types
import qualified Network.AWS.IoT.Types.CertificateValidity as Types
import qualified Network.AWS.IoT.Types.GenerationId as Types
import qualified Network.AWS.IoT.Types.OwnedBy as Types
import qualified Network.AWS.IoT.Types.PreviousOwnedBy as Types
import qualified Network.AWS.IoT.Types.TransferData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a certificate.
--
-- /See:/ 'mkCertificateDescription' smart constructor.
data CertificateDescription = CertificateDescription'
  { -- | The certificate ID of the CA certificate used to sign this certificate.
    caCertificateId :: Core.Maybe Types.CertificateId,
    -- | The ARN of the certificate.
    certificateArn :: Core.Maybe Types.CertificateArn,
    -- | The ID of the certificate.
    certificateId :: Core.Maybe Types.CertificateId,
    -- | The mode of the certificate.
    certificateMode :: Core.Maybe Types.CertificateMode,
    -- | The certificate data, in PEM format.
    certificatePem :: Core.Maybe Types.CertificatePem,
    -- | The date and time the certificate was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The customer version of the certificate.
    customerVersion :: Core.Maybe Core.Natural,
    -- | The generation ID of the certificate.
    generationId :: Core.Maybe Types.GenerationId,
    -- | The date and time the certificate was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the AWS account that owns the certificate.
    ownedBy :: Core.Maybe Types.OwnedBy,
    -- | The ID of the AWS account of the previous owner of the certificate.
    previousOwnedBy :: Core.Maybe Types.PreviousOwnedBy,
    -- | The status of the certificate.
    status :: Core.Maybe Types.CertificateStatus,
    -- | The transfer data.
    transferData :: Core.Maybe Types.TransferData,
    -- | When the certificate is valid.
    validity :: Core.Maybe Types.CertificateValidity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CertificateDescription' value with any optional fields omitted.
mkCertificateDescription ::
  CertificateDescription
mkCertificateDescription =
  CertificateDescription'
    { caCertificateId = Core.Nothing,
      certificateArn = Core.Nothing,
      certificateId = Core.Nothing,
      certificateMode = Core.Nothing,
      certificatePem = Core.Nothing,
      creationDate = Core.Nothing,
      customerVersion = Core.Nothing,
      generationId = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      ownedBy = Core.Nothing,
      previousOwnedBy = Core.Nothing,
      status = Core.Nothing,
      transferData = Core.Nothing,
      validity = Core.Nothing
    }

-- | The certificate ID of the CA certificate used to sign this certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaCertificateId :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateId)
cdCaCertificateId = Lens.field @"caCertificateId"
{-# DEPRECATED cdCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateArn :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateArn)
cdCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED cdCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The ID of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateId :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateId)
cdCertificateId = Lens.field @"certificateId"
{-# DEPRECATED cdCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The mode of the certificate.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateMode :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateMode)
cdCertificateMode = Lens.field @"certificateMode"
{-# DEPRECATED cdCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificatePem :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificatePem)
cdCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED cdCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The date and time the certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCreationDate :: Lens.Lens' CertificateDescription (Core.Maybe Core.NominalDiffTime)
cdCreationDate = Lens.field @"creationDate"
{-# DEPRECATED cdCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The customer version of the certificate.
--
-- /Note:/ Consider using 'customerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCustomerVersion :: Lens.Lens' CertificateDescription (Core.Maybe Core.Natural)
cdCustomerVersion = Lens.field @"customerVersion"
{-# DEPRECATED cdCustomerVersion "Use generic-lens or generic-optics with 'customerVersion' instead." #-}

-- | The generation ID of the certificate.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGenerationId :: Lens.Lens' CertificateDescription (Core.Maybe Types.GenerationId)
cdGenerationId = Lens.field @"generationId"
{-# DEPRECATED cdGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | The date and time the certificate was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastModifiedDate :: Lens.Lens' CertificateDescription (Core.Maybe Core.NominalDiffTime)
cdLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED cdLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ID of the AWS account that owns the certificate.
--
-- /Note:/ Consider using 'ownedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOwnedBy :: Lens.Lens' CertificateDescription (Core.Maybe Types.OwnedBy)
cdOwnedBy = Lens.field @"ownedBy"
{-# DEPRECATED cdOwnedBy "Use generic-lens or generic-optics with 'ownedBy' instead." #-}

-- | The ID of the AWS account of the previous owner of the certificate.
--
-- /Note:/ Consider using 'previousOwnedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPreviousOwnedBy :: Lens.Lens' CertificateDescription (Core.Maybe Types.PreviousOwnedBy)
cdPreviousOwnedBy = Lens.field @"previousOwnedBy"
{-# DEPRECATED cdPreviousOwnedBy "Use generic-lens or generic-optics with 'previousOwnedBy' instead." #-}

-- | The status of the certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStatus :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateStatus)
cdStatus = Lens.field @"status"
{-# DEPRECATED cdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The transfer data.
--
-- /Note:/ Consider using 'transferData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTransferData :: Lens.Lens' CertificateDescription (Core.Maybe Types.TransferData)
cdTransferData = Lens.field @"transferData"
{-# DEPRECATED cdTransferData "Use generic-lens or generic-optics with 'transferData' instead." #-}

-- | When the certificate is valid.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdValidity :: Lens.Lens' CertificateDescription (Core.Maybe Types.CertificateValidity)
cdValidity = Lens.field @"validity"
{-# DEPRECATED cdValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

instance Core.FromJSON CertificateDescription where
  parseJSON =
    Core.withObject "CertificateDescription" Core.$
      \x ->
        CertificateDescription'
          Core.<$> (x Core..:? "caCertificateId")
          Core.<*> (x Core..:? "certificateArn")
          Core.<*> (x Core..:? "certificateId")
          Core.<*> (x Core..:? "certificateMode")
          Core.<*> (x Core..:? "certificatePem")
          Core.<*> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "customerVersion")
          Core.<*> (x Core..:? "generationId")
          Core.<*> (x Core..:? "lastModifiedDate")
          Core.<*> (x Core..:? "ownedBy")
          Core.<*> (x Core..:? "previousOwnedBy")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "transferData")
          Core.<*> (x Core..:? "validity")
