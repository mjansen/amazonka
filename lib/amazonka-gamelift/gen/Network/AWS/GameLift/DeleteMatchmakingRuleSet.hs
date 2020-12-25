{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing matchmaking rule set. To delete the rule set, provide the rule set name. Rule sets cannot be deleted if they are currently being used by a matchmaking configuration.
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
--
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--
--     * 'DescribeMatchmakingConfigurations'
--
--
--     * 'UpdateMatchmakingConfiguration'
--
--
--     * 'DeleteMatchmakingConfiguration'
--
--
--     * 'CreateMatchmakingRuleSet'
--
--
--     * 'DescribeMatchmakingRuleSets'
--
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.DeleteMatchmakingRuleSet
  ( -- * Creating a request
    DeleteMatchmakingRuleSet (..),
    mkDeleteMatchmakingRuleSet,

    -- ** Request lenses
    dmrsName,

    -- * Destructuring the response
    DeleteMatchmakingRuleSetResponse (..),
    mkDeleteMatchmakingRuleSetResponse,

    -- ** Response lenses
    dmrsrfrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteMatchmakingRuleSet' smart constructor.
newtype DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSet'
  { -- | A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
    name :: Types.MatchmakingRuleSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMatchmakingRuleSet' value with any optional fields omitted.
mkDeleteMatchmakingRuleSet ::
  -- | 'name'
  Types.MatchmakingRuleSetName ->
  DeleteMatchmakingRuleSet
mkDeleteMatchmakingRuleSet name = DeleteMatchmakingRuleSet' {name}

-- | A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsName :: Lens.Lens' DeleteMatchmakingRuleSet Types.MatchmakingRuleSetName
dmrsName = Lens.field @"name"
{-# DEPRECATED dmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteMatchmakingRuleSet where
  toJSON DeleteMatchmakingRuleSet {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteMatchmakingRuleSet where
  type Rs DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DeleteMatchmakingRuleSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDeleteMatchmakingRuleSetResponse' smart constructor.
newtype DeleteMatchmakingRuleSetResponse = DeleteMatchmakingRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMatchmakingRuleSetResponse' value with any optional fields omitted.
mkDeleteMatchmakingRuleSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMatchmakingRuleSetResponse
mkDeleteMatchmakingRuleSetResponse responseStatus =
  DeleteMatchmakingRuleSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrfrsResponseStatus :: Lens.Lens' DeleteMatchmakingRuleSetResponse Core.Int
dmrsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
