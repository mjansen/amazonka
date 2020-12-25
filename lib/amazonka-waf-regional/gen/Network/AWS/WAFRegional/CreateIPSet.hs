{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'IPSet' , which you use to specify which web requests that you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an @IPSet@ that contains those IP addresses and then configure AWS WAF to block the requests.
--
-- To create and configure an @IPSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateIPSet@ request.
--
--
--     * Submit a @CreateIPSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateIPSet@ request to specify the IP addresses that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateIPSet
  ( -- * Creating a request
    CreateIPSet (..),
    mkCreateIPSet,

    -- ** Request lenses
    cipsName,
    cipsChangeToken,

    -- * Destructuring the response
    CreateIPSetResponse (..),
    mkCreateIPSetResponse,

    -- ** Response lenses
    cipsrrsChangeToken,
    cipsrrsIPSet,
    cipsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
    name :: Types.ResourceName,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIPSet' value with any optional fields omitted.
mkCreateIPSet ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateIPSet
mkCreateIPSet name changeToken = CreateIPSet' {name, changeToken}

-- | A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsName :: Lens.Lens' CreateIPSet Types.ResourceName
cipsName = Lens.field @"name"
{-# DEPRECATED cipsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsChangeToken :: Lens.Lens' CreateIPSet Types.ChangeToken
cipsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cipsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON CreateIPSet where
  toJSON CreateIPSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest CreateIPSet where
  type Rs CreateIPSet = CreateIPSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateIPSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Core.<$> (x Core..:? "ChangeToken")
            Core.<*> (x Core..:? "IPSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The 'IPSet' returned in the @CreateIPSet@ response.
    iPSet :: Core.Maybe Types.IPSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIPSetResponse' value with any optional fields omitted.
mkCreateIPSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateIPSetResponse
mkCreateIPSetResponse responseStatus =
  CreateIPSetResponse'
    { changeToken = Core.Nothing,
      iPSet = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsrrsChangeToken :: Lens.Lens' CreateIPSetResponse (Core.Maybe Types.ChangeToken)
cipsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cipsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The 'IPSet' returned in the @CreateIPSet@ response.
--
-- /Note:/ Consider using 'iPSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsrrsIPSet :: Lens.Lens' CreateIPSetResponse (Core.Maybe Types.IPSet)
cipsrrsIPSet = Lens.field @"iPSet"
{-# DEPRECATED cipsrrsIPSet "Use generic-lens or generic-optics with 'iPSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsrrsResponseStatus :: Lens.Lens' CreateIPSetResponse Core.Int
cipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
