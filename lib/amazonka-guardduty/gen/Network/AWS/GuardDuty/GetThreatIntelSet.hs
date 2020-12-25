{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.GetThreatIntelSet
  ( -- * Creating a request
    GetThreatIntelSet (..),
    mkGetThreatIntelSet,

    -- ** Request lenses
    gtisDetectorId,
    gtisThreatIntelSetId,

    -- * Destructuring the response
    GetThreatIntelSetResponse (..),
    mkGetThreatIntelSetResponse,

    -- ** Response lenses
    gtisrrsName,
    gtisrrsFormat,
    gtisrrsLocation,
    gtisrrsStatus,
    gtisrrsTags,
    gtisrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetThreatIntelSet' smart constructor.
data GetThreatIntelSet = GetThreatIntelSet'
  { -- | The unique ID of the detector that the threatIntelSet is associated with.
    detectorId :: Types.DetectorId,
    -- | The unique ID of the threatIntelSet that you want to get.
    threatIntelSetId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThreatIntelSet' value with any optional fields omitted.
mkGetThreatIntelSet ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'threatIntelSetId'
  Types.String ->
  GetThreatIntelSet
mkGetThreatIntelSet detectorId threatIntelSetId =
  GetThreatIntelSet' {detectorId, threatIntelSetId}

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisDetectorId :: Lens.Lens' GetThreatIntelSet Types.DetectorId
gtisDetectorId = Lens.field @"detectorId"
{-# DEPRECATED gtisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID of the threatIntelSet that you want to get.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisThreatIntelSetId :: Lens.Lens' GetThreatIntelSet Types.String
gtisThreatIntelSetId = Lens.field @"threatIntelSetId"
{-# DEPRECATED gtisThreatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead." #-}

instance Core.AWSRequest GetThreatIntelSet where
  type Rs GetThreatIntelSet = GetThreatIntelSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/threatintelset/")
                Core.<> (Core.toText threatIntelSetId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThreatIntelSetResponse'
            Core.<$> (x Core..: "name")
            Core.<*> (x Core..: "format")
            Core.<*> (x Core..: "location")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetThreatIntelSetResponse' smart constructor.
data GetThreatIntelSetResponse = GetThreatIntelSetResponse'
  { -- | A user-friendly ThreatIntelSet name displayed in all findings that are generated by activity that involves IP addresses included in this ThreatIntelSet.
    name :: Types.Name,
    -- | The format of the threatIntelSet.
    format :: Types.ThreatIntelSetFormat,
    -- | The URI of the file that contains the ThreatIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
    location :: Types.Location,
    -- | The status of threatIntelSet file uploaded.
    status :: Types.ThreatIntelSetStatus,
    -- | The tags of the threat list resource.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThreatIntelSetResponse' value with any optional fields omitted.
mkGetThreatIntelSetResponse ::
  -- | 'name'
  Types.Name ->
  -- | 'format'
  Types.ThreatIntelSetFormat ->
  -- | 'location'
  Types.Location ->
  -- | 'status'
  Types.ThreatIntelSetStatus ->
  -- | 'responseStatus'
  Core.Int ->
  GetThreatIntelSetResponse
mkGetThreatIntelSetResponse
  name
  format
  location
  status
  responseStatus =
    GetThreatIntelSetResponse'
      { name,
        format,
        location,
        status,
        tags = Core.Nothing,
        responseStatus
      }

-- | A user-friendly ThreatIntelSet name displayed in all findings that are generated by activity that involves IP addresses included in this ThreatIntelSet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsName :: Lens.Lens' GetThreatIntelSetResponse Types.Name
gtisrrsName = Lens.field @"name"
{-# DEPRECATED gtisrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The format of the threatIntelSet.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsFormat :: Lens.Lens' GetThreatIntelSetResponse Types.ThreatIntelSetFormat
gtisrrsFormat = Lens.field @"format"
{-# DEPRECATED gtisrrsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URI of the file that contains the ThreatIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsLocation :: Lens.Lens' GetThreatIntelSetResponse Types.Location
gtisrrsLocation = Lens.field @"location"
{-# DEPRECATED gtisrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The status of threatIntelSet file uploaded.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsStatus :: Lens.Lens' GetThreatIntelSetResponse Types.ThreatIntelSetStatus
gtisrrsStatus = Lens.field @"status"
{-# DEPRECATED gtisrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tags of the threat list resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsTags :: Lens.Lens' GetThreatIntelSetResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gtisrrsTags = Lens.field @"tags"
{-# DEPRECATED gtisrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrrsResponseStatus :: Lens.Lens' GetThreatIntelSetResponse Core.Int
gtisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
