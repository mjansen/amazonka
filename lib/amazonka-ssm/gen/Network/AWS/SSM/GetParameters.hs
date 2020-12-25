{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of a parameter. Don't confuse this API action with the 'GetParameter' API action.
module Network.AWS.SSM.GetParameters
  ( -- * Creating a request
    GetParameters (..),
    mkGetParameters,

    -- ** Request lenses
    gpNames,
    gpWithDecryption,

    -- * Destructuring the response
    GetParametersResponse (..),
    mkGetParametersResponse,

    -- ** Response lenses
    gprrsInvalidParameters,
    gprrsParameters,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetParameters' smart constructor.
data GetParameters = GetParameters'
  { -- | Names of the parameters for which you want to query information.
    names :: Core.NonEmpty Types.PSParameterName,
    -- | Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
    withDecryption :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParameters' value with any optional fields omitted.
mkGetParameters ::
  -- | 'names'
  Core.NonEmpty Types.PSParameterName ->
  GetParameters
mkGetParameters names =
  GetParameters' {names, withDecryption = Core.Nothing}

-- | Names of the parameters for which you want to query information.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpNames :: Lens.Lens' GetParameters (Core.NonEmpty Types.PSParameterName)
gpNames = Lens.field @"names"
{-# DEPRECATED gpNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpWithDecryption :: Lens.Lens' GetParameters (Core.Maybe Core.Bool)
gpWithDecryption = Lens.field @"withDecryption"
{-# DEPRECATED gpWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

instance Core.FromJSON GetParameters where
  toJSON GetParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Names" Core..= names),
            ("WithDecryption" Core..=) Core.<$> withDecryption
          ]
      )

instance Core.AWSRequest GetParameters where
  type Rs GetParameters = GetParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetParameters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersResponse'
            Core.<$> (x Core..:? "InvalidParameters")
            Core.<*> (x Core..:? "Parameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { -- | A list of parameters that are not formatted correctly or do not run during an execution.
    invalidParameters :: Core.Maybe [Types.PSParameterName],
    -- | A list of details for a parameter.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetParametersResponse' value with any optional fields omitted.
mkGetParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetParametersResponse
mkGetParametersResponse responseStatus =
  GetParametersResponse'
    { invalidParameters = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | A list of parameters that are not formatted correctly or do not run during an execution.
--
-- /Note:/ Consider using 'invalidParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsInvalidParameters :: Lens.Lens' GetParametersResponse (Core.Maybe [Types.PSParameterName])
gprrsInvalidParameters = Lens.field @"invalidParameters"
{-# DEPRECATED gprrsInvalidParameters "Use generic-lens or generic-optics with 'invalidParameters' instead." #-}

-- | A list of details for a parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsParameters :: Lens.Lens' GetParametersResponse (Core.Maybe [Types.Parameter])
gprrsParameters = Lens.field @"parameters"
{-# DEPRECATED gprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetParametersResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
