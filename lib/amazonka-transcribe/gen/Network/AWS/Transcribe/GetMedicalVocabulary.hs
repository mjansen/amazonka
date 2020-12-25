{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetMedicalVocabulary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a medical vocabulary.
module Network.AWS.Transcribe.GetMedicalVocabulary
  ( -- * Creating a request
    GetMedicalVocabulary (..),
    mkGetMedicalVocabulary,

    -- ** Request lenses
    gmvVocabularyName,

    -- * Destructuring the response
    GetMedicalVocabularyResponse (..),
    mkGetMedicalVocabularyResponse,

    -- ** Response lenses
    gmvrrsDownloadUri,
    gmvrrsFailureReason,
    gmvrrsLanguageCode,
    gmvrrsLastModifiedTime,
    gmvrrsVocabularyName,
    gmvrrsVocabularyState,
    gmvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkGetMedicalVocabulary' smart constructor.
newtype GetMedicalVocabulary = GetMedicalVocabulary'
  { -- | The name of the vocabulary that you want information about. The value is case sensitive.
    vocabularyName :: Types.VocabularyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMedicalVocabulary' value with any optional fields omitted.
mkGetMedicalVocabulary ::
  -- | 'vocabularyName'
  Types.VocabularyName ->
  GetMedicalVocabulary
mkGetMedicalVocabulary vocabularyName =
  GetMedicalVocabulary' {vocabularyName}

-- | The name of the vocabulary that you want information about. The value is case sensitive.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvVocabularyName :: Lens.Lens' GetMedicalVocabulary Types.VocabularyName
gmvVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED gmvVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

instance Core.FromJSON GetMedicalVocabulary where
  toJSON GetMedicalVocabulary {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VocabularyName" Core..= vocabularyName)]
      )

instance Core.AWSRequest GetMedicalVocabulary where
  type Rs GetMedicalVocabulary = GetMedicalVocabularyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.GetMedicalVocabulary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalVocabularyResponse'
            Core.<$> (x Core..:? "DownloadUri")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "VocabularyName")
            Core.<*> (x Core..:? "VocabularyState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMedicalVocabularyResponse' smart constructor.
data GetMedicalVocabularyResponse = GetMedicalVocabularyResponse'
  { -- | The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
    downloadUri :: Core.Maybe Types.Uri,
    -- | If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The valid language code for your vocabulary entries.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the vocabulary returned by Amazon Transcribe Medical.
    vocabularyName :: Core.Maybe Types.VocabularyName,
    -- | The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
    vocabularyState :: Core.Maybe Types.VocabularyState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMedicalVocabularyResponse' value with any optional fields omitted.
mkGetMedicalVocabularyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMedicalVocabularyResponse
mkGetMedicalVocabularyResponse responseStatus =
  GetMedicalVocabularyResponse'
    { downloadUri = Core.Nothing,
      failureReason = Core.Nothing,
      languageCode = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      vocabularyName = Core.Nothing,
      vocabularyState = Core.Nothing,
      responseStatus
    }

-- | The location in Amazon S3 where the vocabulary is stored. Use this URI to get the contents of the vocabulary. You can download your vocabulary from the URI for a limited time.
--
-- /Note:/ Consider using 'downloadUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsDownloadUri :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Types.Uri)
gmvrrsDownloadUri = Lens.field @"downloadUri"
{-# DEPRECATED gmvrrsDownloadUri "Use generic-lens or generic-optics with 'downloadUri' instead." #-}

-- | If the @VocabularyState@ is @FAILED@ , this field contains information about why the job failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsFailureReason :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Types.FailureReason)
gmvrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED gmvrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The valid language code for your vocabulary entries.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsLanguageCode :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Types.LanguageCode)
gmvrrsLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED gmvrrsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The date and time that the vocabulary was last modified with a text file different from the one that was previously used.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsLastModifiedTime :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Core.NominalDiffTime)
gmvrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED gmvrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the vocabulary returned by Amazon Transcribe Medical.
--
-- /Note:/ Consider using 'vocabularyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsVocabularyName :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Types.VocabularyName)
gmvrrsVocabularyName = Lens.field @"vocabularyName"
{-# DEPRECATED gmvrrsVocabularyName "Use generic-lens or generic-optics with 'vocabularyName' instead." #-}

-- | The processing state of the vocabulary. If the @VocabularyState@ is @READY@ then you can use it in the @StartMedicalTranscriptionJob@ operation.
--
-- /Note:/ Consider using 'vocabularyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsVocabularyState :: Lens.Lens' GetMedicalVocabularyResponse (Core.Maybe Types.VocabularyState)
gmvrrsVocabularyState = Lens.field @"vocabularyState"
{-# DEPRECATED gmvrrsVocabularyState "Use generic-lens or generic-optics with 'vocabularyState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmvrrsResponseStatus :: Lens.Lens' GetMedicalVocabularyResponse Core.Int
gmvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
