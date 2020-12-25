{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.LaunchApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified application as a stack in AWS CloudFormation.
module Network.AWS.SMS.LaunchApp
  ( -- * Creating a request
    LaunchApp (..),
    mkLaunchApp,

    -- ** Request lenses
    laAppId,

    -- * Destructuring the response
    LaunchAppResponse (..),
    mkLaunchAppResponse,

    -- ** Response lenses
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkLaunchApp' smart constructor.
newtype LaunchApp = LaunchApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchApp' value with any optional fields omitted.
mkLaunchApp ::
  LaunchApp
mkLaunchApp = LaunchApp' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAppId :: Lens.Lens' LaunchApp (Core.Maybe Types.AppId)
laAppId = Lens.field @"appId"
{-# DEPRECATED laAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON LaunchApp where
  toJSON LaunchApp {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest LaunchApp where
  type Rs LaunchApp = LaunchAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.LaunchApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          LaunchAppResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkLaunchAppResponse' smart constructor.
newtype LaunchAppResponse = LaunchAppResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchAppResponse' value with any optional fields omitted.
mkLaunchAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  LaunchAppResponse
mkLaunchAppResponse responseStatus =
  LaunchAppResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' LaunchAppResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
