{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a unique device type.
module Network.AWS.DeviceFarm.GetDevice
  ( -- * Creating a request
    GetDevice (..),
    mkGetDevice,

    -- ** Request lenses
    gdArn,

    -- * Destructuring the response
    GetDeviceResponse (..),
    mkGetDeviceResponse,

    -- ** Response lenses
    gdrrsDevice,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device request.
--
-- /See:/ 'mkGetDevice' smart constructor.
newtype GetDevice = GetDevice'
  { -- | The device type's ARN.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevice' value with any optional fields omitted.
mkGetDevice ::
  -- | 'arn'
  Types.Arn ->
  GetDevice
mkGetDevice arn = GetDevice' {arn}

-- | The device type's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdArn :: Lens.Lens' GetDevice Types.Arn
gdArn = Lens.field @"arn"
{-# DEPRECATED gdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetDevice where
  toJSON GetDevice {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetDevice where
  type Rs GetDevice = GetDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetDevice")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Core.<$> (x Core..:? "device") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get device request.
--
-- /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | An object that contains information about the requested device.
    device :: Core.Maybe Types.Device,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceResponse' value with any optional fields omitted.
mkGetDeviceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeviceResponse
mkGetDeviceResponse responseStatus =
  GetDeviceResponse' {device = Core.Nothing, responseStatus}

-- | An object that contains information about the requested device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDevice :: Lens.Lens' GetDeviceResponse (Core.Maybe Types.Device)
gdrrsDevice = Lens.field @"device"
{-# DEPRECATED gdrrsDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeviceResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
