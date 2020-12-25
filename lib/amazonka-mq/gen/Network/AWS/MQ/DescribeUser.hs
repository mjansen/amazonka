{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an ActiveMQ user.
module Network.AWS.MQ.DescribeUser
  ( -- * Creating a request
    DescribeUser (..),
    mkDescribeUser,

    -- ** Request lenses
    duUsername,
    duBrokerId,

    -- * Destructuring the response
    DescribeUserResponse (..),
    mkDescribeUserResponse,

    -- ** Response lenses
    durrsBrokerId,
    durrsConsoleAccess,
    durrsGroups,
    durrsPending,
    durrsUsername,
    durrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUser' value with any optional fields omitted.
mkDescribeUser ::
  -- | 'username'
  Core.Text ->
  -- | 'brokerId'
  Core.Text ->
  DescribeUser
mkDescribeUser username brokerId =
  DescribeUser' {username, brokerId}

-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUsername :: Lens.Lens' DescribeUser Core.Text
duUsername = Lens.field @"username"
{-# DEPRECATED duUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duBrokerId :: Lens.Lens' DescribeUser Core.Text
duBrokerId = Lens.field @"brokerId"
{-# DEPRECATED duBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Core.AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/brokers/" Core.<> (Core.toText brokerId) Core.<> ("/users/")
                Core.<> (Core.toText username)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Core.<$> (x Core..:? "brokerId")
            Core.<*> (x Core..:? "consoleAccess")
            Core.<*> (x Core..:? "groups")
            Core.<*> (x Core..:? "pending")
            Core.<*> (x Core..:? "username")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Core.Maybe [Core.Text],
    -- | The status of the changes pending for the ActiveMQ user.
    pending :: Core.Maybe Types.UserPendingChanges,
    -- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    username :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserResponse' value with any optional fields omitted.
mkDescribeUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserResponse
mkDescribeUserResponse responseStatus =
  DescribeUserResponse'
    { brokerId = Core.Nothing,
      consoleAccess = Core.Nothing,
      groups = Core.Nothing,
      pending = Core.Nothing,
      username = Core.Nothing,
      responseStatus
    }

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsBrokerId :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
durrsBrokerId = Lens.field @"brokerId"
{-# DEPRECATED durrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsConsoleAccess :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Bool)
durrsConsoleAccess = Lens.field @"consoleAccess"
{-# DEPRECATED durrsConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsGroups :: Lens.Lens' DescribeUserResponse (Core.Maybe [Core.Text])
durrsGroups = Lens.field @"groups"
{-# DEPRECATED durrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The status of the changes pending for the ActiveMQ user.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsPending :: Lens.Lens' DescribeUserResponse (Core.Maybe Types.UserPendingChanges)
durrsPending = Lens.field @"pending"
{-# DEPRECATED durrsPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUsername :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
durrsUsername = Lens.field @"username"
{-# DEPRECATED durrsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUserResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED durrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
