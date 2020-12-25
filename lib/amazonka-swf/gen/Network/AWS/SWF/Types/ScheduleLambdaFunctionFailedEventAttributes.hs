{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
  ( ScheduleLambdaFunctionFailedEventAttributes (..),

    -- * Smart constructor
    mkScheduleLambdaFunctionFailedEventAttributes,

    -- * Lenses
    slffeaId,
    slffeaName,
    slffeaCause,
    slffeaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.FunctionName as Types
import qualified Network.AWS.SWF.Types.Id as Types
import qualified Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause as Types

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkScheduleLambdaFunctionFailedEventAttributes' smart constructor.
data ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes'
  { -- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
    id :: Types.Id,
    -- | The name of the Lambda function.
    name :: Types.FunctionName,
    -- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    cause :: Types.ScheduleLambdaFunctionFailedCause,
    -- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleLambdaFunctionFailedEventAttributes' value with any optional fields omitted.
mkScheduleLambdaFunctionFailedEventAttributes ::
  -- | 'id'
  Types.Id ->
  -- | 'name'
  Types.FunctionName ->
  -- | 'cause'
  Types.ScheduleLambdaFunctionFailedCause ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  ScheduleLambdaFunctionFailedEventAttributes
mkScheduleLambdaFunctionFailedEventAttributes
  id
  name
  cause
  decisionTaskCompletedEventId =
    ScheduleLambdaFunctionFailedEventAttributes'
      { id,
        name,
        cause,
        decisionTaskCompletedEventId
      }

-- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaId :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Types.Id
slffeaId = Lens.field @"id"
{-# DEPRECATED slffeaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the Lambda function.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaName :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Types.FunctionName
slffeaName = Lens.field @"name"
{-# DEPRECATED slffeaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaCause :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Types.ScheduleLambdaFunctionFailedCause
slffeaCause = Lens.field @"cause"
{-# DEPRECATED slffeaCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slffeaDecisionTaskCompletedEventId :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Core.Integer
slffeaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED slffeaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Core.FromJSON ScheduleLambdaFunctionFailedEventAttributes where
  parseJSON =
    Core.withObject "ScheduleLambdaFunctionFailedEventAttributes" Core.$
      \x ->
        ScheduleLambdaFunctionFailedEventAttributes'
          Core.<$> (x Core..: "id")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "cause")
          Core.<*> (x Core..: "decisionTaskCompletedEventId")
