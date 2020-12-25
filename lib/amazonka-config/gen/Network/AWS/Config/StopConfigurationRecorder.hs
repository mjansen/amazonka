{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StopConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops recording configurations of the AWS resources you have selected to record in your AWS account.
module Network.AWS.Config.StopConfigurationRecorder
  ( -- * Creating a request
    StopConfigurationRecorder (..),
    mkStopConfigurationRecorder,

    -- ** Request lenses
    scrConfigurationRecorderName,

    -- * Destructuring the response
    StopConfigurationRecorderResponse (..),
    mkStopConfigurationRecorderResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'StopConfigurationRecorder' action.
--
-- /See:/ 'mkStopConfigurationRecorder' smart constructor.
newtype StopConfigurationRecorder = StopConfigurationRecorder'
  { -- | The name of the recorder object that records each configuration change made to the resources.
    configurationRecorderName :: Types.RecorderName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopConfigurationRecorder' value with any optional fields omitted.
mkStopConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Types.RecorderName ->
  StopConfigurationRecorder
mkStopConfigurationRecorder configurationRecorderName =
  StopConfigurationRecorder' {configurationRecorderName}

-- | The name of the recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrConfigurationRecorderName :: Lens.Lens' StopConfigurationRecorder Types.RecorderName
scrConfigurationRecorderName = Lens.field @"configurationRecorderName"
{-# DEPRECATED scrConfigurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead." #-}

instance Core.FromJSON StopConfigurationRecorder where
  toJSON StopConfigurationRecorder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConfigurationRecorderName" Core..= configurationRecorderName)
          ]
      )

instance Core.AWSRequest StopConfigurationRecorder where
  type
    Rs StopConfigurationRecorder =
      StopConfigurationRecorderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.StopConfigurationRecorder")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopConfigurationRecorderResponse'

-- | /See:/ 'mkStopConfigurationRecorderResponse' smart constructor.
data StopConfigurationRecorderResponse = StopConfigurationRecorderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopConfigurationRecorderResponse' value with any optional fields omitted.
mkStopConfigurationRecorderResponse ::
  StopConfigurationRecorderResponse
mkStopConfigurationRecorderResponse =
  StopConfigurationRecorderResponse'
