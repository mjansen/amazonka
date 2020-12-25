{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
  ( LambdaFunctionSucceededEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionSucceededEventDetails,

    -- * Lenses
    lfsedOutput,
    lfsedOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about a lambda function that successfully terminated during an execution.
--
-- /See:/ 'mkLambdaFunctionSucceededEventDetails' smart constructor.
data LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { -- | The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe Types.SensitiveData,
    -- | Contains details about the output of an execution history event.
    outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionSucceededEventDetails' value with any optional fields omitted.
mkLambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
mkLambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { output = Core.Nothing,
      outputDetails = Core.Nothing
    }

-- | The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedOutput :: Lens.Lens' LambdaFunctionSucceededEventDetails (Core.Maybe Types.SensitiveData)
lfsedOutput = Lens.field @"output"
{-# DEPRECATED lfsedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedOutputDetails :: Lens.Lens' LambdaFunctionSucceededEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
lfsedOutputDetails = Lens.field @"outputDetails"
{-# DEPRECATED lfsedOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Core.FromJSON LambdaFunctionSucceededEventDetails where
  parseJSON =
    Core.withObject "LambdaFunctionSucceededEventDetails" Core.$
      \x ->
        LambdaFunctionSucceededEventDetails'
          Core.<$> (x Core..:? "output") Core.<*> (x Core..:? "outputDetails")
