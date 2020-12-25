{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a previously associated principal ARN from a specified portfolio.
module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
  ( -- * Creating a request
    DisassociatePrincipalFromPortfolio (..),
    mkDisassociatePrincipalFromPortfolio,

    -- ** Request lenses
    dpfpfPortfolioId,
    dpfpfPrincipalARN,
    dpfpfAcceptLanguage,

    -- * Destructuring the response
    DisassociatePrincipalFromPortfolioResponse (..),
    mkDisassociatePrincipalFromPortfolioResponse,

    -- ** Response lenses
    dpfprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociatePrincipalFromPortfolio' smart constructor.
data DisassociatePrincipalFromPortfolio = DisassociatePrincipalFromPortfolio'
  { -- | The portfolio identifier.
    portfolioId :: Types.PortfolioId,
    -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Types.PrincipalARN,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociatePrincipalFromPortfolio' value with any optional fields omitted.
mkDisassociatePrincipalFromPortfolio ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  -- | 'principalARN'
  Types.PrincipalARN ->
  DisassociatePrincipalFromPortfolio
mkDisassociatePrincipalFromPortfolio portfolioId principalARN =
  DisassociatePrincipalFromPortfolio'
    { portfolioId,
      principalARN,
      acceptLanguage = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfpfPortfolioId :: Lens.Lens' DisassociatePrincipalFromPortfolio Types.PortfolioId
dpfpfPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED dpfpfPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfpfPrincipalARN :: Lens.Lens' DisassociatePrincipalFromPortfolio Types.PrincipalARN
dpfpfPrincipalARN = Lens.field @"principalARN"
{-# DEPRECATED dpfpfPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfpfAcceptLanguage :: Lens.Lens' DisassociatePrincipalFromPortfolio (Core.Maybe Types.AcceptLanguage)
dpfpfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpfpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DisassociatePrincipalFromPortfolio where
  toJSON DisassociatePrincipalFromPortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("PrincipalARN" Core..= principalARN),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DisassociatePrincipalFromPortfolio where
  type
    Rs DisassociatePrincipalFromPortfolio =
      DisassociatePrincipalFromPortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociatePrincipalFromPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociatePrincipalFromPortfolioResponse' smart constructor.
newtype DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociatePrincipalFromPortfolioResponse' value with any optional fields omitted.
mkDisassociatePrincipalFromPortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociatePrincipalFromPortfolioResponse
mkDisassociatePrincipalFromPortfolioResponse responseStatus =
  DisassociatePrincipalFromPortfolioResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfprfrsResponseStatus :: Lens.Lens' DisassociatePrincipalFromPortfolioResponse Core.Int
dpfprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpfprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
