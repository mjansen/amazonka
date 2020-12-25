{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an alias to the set of a given member (user or group) of Amazon WorkMail.
module Network.AWS.WorkMail.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caOrganizationId,
    caEntityId,
    caAlias,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,

    -- ** Response lenses
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The organization under which the member (user or group) exists.
    organizationId :: Types.OrganizationId,
    -- | The member (user or group) to which this alias is added.
    entityId :: Types.WorkMailIdentifier,
    -- | The alias to add to the member set.
    alias :: Types.EmailAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlias' value with any optional fields omitted.
mkCreateAlias ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.WorkMailIdentifier ->
  -- | 'alias'
  Types.EmailAddress ->
  CreateAlias
mkCreateAlias organizationId entityId alias =
  CreateAlias' {organizationId, entityId, alias}

-- | The organization under which the member (user or group) exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOrganizationId :: Lens.Lens' CreateAlias Types.OrganizationId
caOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED caOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The member (user or group) to which this alias is added.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEntityId :: Lens.Lens' CreateAlias Types.WorkMailIdentifier
caEntityId = Lens.field @"entityId"
{-# DEPRECATED caEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The alias to add to the member set.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlias :: Lens.Lens' CreateAlias Types.EmailAddress
caAlias = Lens.field @"alias"
{-# DEPRECATED caAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

instance Core.FromJSON CreateAlias where
  toJSON CreateAlias {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Alias" Core..= alias)
          ]
      )

instance Core.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.CreateAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAliasResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAliasResponse' smart constructor.
newtype CreateAliasResponse = CreateAliasResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAliasResponse' value with any optional fields omitted.
mkCreateAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAliasResponse
mkCreateAliasResponse responseStatus =
  CreateAliasResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAliasResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
