{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results of the AWS Trusted Advisor check that has the specified check ID. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains a 'TrustedAdvisorCheckResult' object, which contains these three objects:
--
--     * 'TrustedAdvisorCategorySpecificSummary'
--
--
--     * 'TrustedAdvisorResourceDetail'
--
--
--     * 'TrustedAdvisorResourcesSummary'
--
--
-- In addition, the response contains these fields:
--
--     * __status__ - The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
--
--     * __timestamp__ - The time of the last refresh of the check.
--
--
--     * __checkId__ - The unique identifier for the check.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
  ( -- * Creating a request
    DescribeTrustedAdvisorCheckResult (..),
    mkDescribeTrustedAdvisorCheckResult,

    -- ** Request lenses
    dtacrCheckId,
    dtacrLanguage,

    -- * Destructuring the response
    DescribeTrustedAdvisorCheckResultResponse (..),
    mkDescribeTrustedAdvisorCheckResultResponse,

    -- ** Response lenses
    dtacrrrsResult,
    dtacrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- |
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckResult' smart constructor.
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Types.String,
    -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorCheckResult' value with any optional fields omitted.
mkDescribeTrustedAdvisorCheckResult ::
  -- | 'checkId'
  Types.String ->
  DescribeTrustedAdvisorCheckResult
mkDescribeTrustedAdvisorCheckResult checkId =
  DescribeTrustedAdvisorCheckResult'
    { checkId,
      language = Core.Nothing
    }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrCheckId :: Lens.Lens' DescribeTrustedAdvisorCheckResult Types.String
dtacrCheckId = Lens.field @"checkId"
{-# DEPRECATED dtacrCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrLanguage :: Lens.Lens' DescribeTrustedAdvisorCheckResult (Core.Maybe Types.String)
dtacrLanguage = Lens.field @"language"
{-# DEPRECATED dtacrLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Core.FromJSON DescribeTrustedAdvisorCheckResult where
  toJSON DescribeTrustedAdvisorCheckResult {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("checkId" Core..= checkId),
            ("language" Core..=) Core.<$> language
          ]
      )

instance Core.AWSRequest DescribeTrustedAdvisorCheckResult where
  type
    Rs DescribeTrustedAdvisorCheckResult =
      DescribeTrustedAdvisorCheckResultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSSupport_20130415.DescribeTrustedAdvisorCheckResult"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckResultResponse'
            Core.<$> (x Core..:? "result") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of the Trusted Advisor check returned by the 'DescribeTrustedAdvisorCheckResult' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckResultResponse' smart constructor.
data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'
  { -- | The detailed results of the Trusted Advisor check.
    result :: Core.Maybe Types.TrustedAdvisorCheckResult,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorCheckResultResponse' value with any optional fields omitted.
mkDescribeTrustedAdvisorCheckResultResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrustedAdvisorCheckResultResponse
mkDescribeTrustedAdvisorCheckResultResponse responseStatus =
  DescribeTrustedAdvisorCheckResultResponse'
    { result = Core.Nothing,
      responseStatus
    }

-- | The detailed results of the Trusted Advisor check.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrrsResult :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse (Core.Maybe Types.TrustedAdvisorCheckResult)
dtacrrrsResult = Lens.field @"result"
{-# DEPRECATED dtacrrrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse Core.Int
dtacrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtacrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
