{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a suggester for a domain. A suggester enables you to display possible matches before users finish typing their queries. When you configure a suggester, you must specify the name of the text field you want to search for possible matches and a unique name for the suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineSuggester
  ( -- * Creating a request
    DefineSuggester (..),
    mkDefineSuggester,

    -- ** Request lenses
    dsfDomainName,
    dsfSuggester,

    -- * Destructuring the response
    DefineSuggesterResponse (..),
    mkDefineSuggesterResponse,

    -- ** Response lenses
    dsrrsSuggester,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DefineSuggester' @ operation. Specifies the name of the domain you want to update and the suggester configuration.
--
-- /See:/ 'mkDefineSuggester' smart constructor.
data DefineSuggester = DefineSuggester'
  { domainName :: Types.DomainName,
    suggester :: Types.Suggester
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefineSuggester' value with any optional fields omitted.
mkDefineSuggester ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'suggester'
  Types.Suggester ->
  DefineSuggester
mkDefineSuggester domainName suggester =
  DefineSuggester' {domainName, suggester}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDomainName :: Lens.Lens' DefineSuggester Types.DomainName
dsfDomainName = Lens.field @"domainName"
{-# DEPRECATED dsfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfSuggester :: Lens.Lens' DefineSuggester Types.Suggester
dsfSuggester = Lens.field @"suggester"
{-# DEPRECATED dsfSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

instance Core.AWSRequest DefineSuggester where
  type Rs DefineSuggester = DefineSuggesterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DefineSuggester")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "Suggester" suggester)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DefineSuggesterResult"
      ( \s h x ->
          DefineSuggesterResponse'
            Core.<$> (x Core..@ "Suggester") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DefineSuggester@ request. Contains the status of the newly-configured suggester.
--
-- /See:/ 'mkDefineSuggesterResponse' smart constructor.
data DefineSuggesterResponse = DefineSuggesterResponse'
  { suggester :: Types.SuggesterStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DefineSuggesterResponse' value with any optional fields omitted.
mkDefineSuggesterResponse ::
  -- | 'suggester'
  Types.SuggesterStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DefineSuggesterResponse
mkDefineSuggesterResponse suggester responseStatus =
  DefineSuggesterResponse' {suggester, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSuggester :: Lens.Lens' DefineSuggesterResponse Types.SuggesterStatus
dsrrsSuggester = Lens.field @"suggester"
{-# DEPRECATED dsrrsSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DefineSuggesterResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
