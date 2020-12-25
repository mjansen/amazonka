{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A public virtual interface supports sending traffic to public services of AWS such as Amazon S3.
--
-- When creating an IPv6 public virtual interface (@addressFamily@ is @ipv6@ ), leave the @customer@ and @amazon@ address fields blank to use auto-assigned IPv6 space. Custom IPv6 addresses are not supported.
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
  ( -- * Creating a request
    CreatePublicVirtualInterface (..),
    mkCreatePublicVirtualInterface,

    -- ** Request lenses
    cpviConnectionId,
    cpviNewPublicVirtualInterface,

    -- * Destructuring the response
    Types.VirtualInterface (..),
    Types.mkVirtualInterface,

    -- ** Response lenses
    Types.viAddressFamily,
    Types.viAmazonAddress,
    Types.viAmazonSideAsn,
    Types.viAsn,
    Types.viAuthKey,
    Types.viAwsDeviceV2,
    Types.viBgpPeers,
    Types.viConnectionId,
    Types.viCustomerAddress,
    Types.viCustomerRouterConfig,
    Types.viDirectConnectGatewayId,
    Types.viJumboFrameCapable,
    Types.viLocation,
    Types.viMtu,
    Types.viOwnerAccount,
    Types.viRegion,
    Types.viRouteFilterPrefixes,
    Types.viTags,
    Types.viVirtualGatewayId,
    Types.viVirtualInterfaceId,
    Types.viVirtualInterfaceName,
    Types.viVirtualInterfaceState,
    Types.viVirtualInterfaceType,
    Types.viVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePublicVirtualInterface' smart constructor.
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Types.ConnectionId,
    -- | Information about the public virtual interface.
    newPublicVirtualInterface :: Types.NewPublicVirtualInterface
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublicVirtualInterface' value with any optional fields omitted.
mkCreatePublicVirtualInterface ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'newPublicVirtualInterface'
  Types.NewPublicVirtualInterface ->
  CreatePublicVirtualInterface
mkCreatePublicVirtualInterface
  connectionId
  newPublicVirtualInterface =
    CreatePublicVirtualInterface'
      { connectionId,
        newPublicVirtualInterface
      }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviConnectionId :: Lens.Lens' CreatePublicVirtualInterface Types.ConnectionId
cpviConnectionId = Lens.field @"connectionId"
{-# DEPRECATED cpviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | Information about the public virtual interface.
--
-- /Note:/ Consider using 'newPublicVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviNewPublicVirtualInterface :: Lens.Lens' CreatePublicVirtualInterface Types.NewPublicVirtualInterface
cpviNewPublicVirtualInterface = Lens.field @"newPublicVirtualInterface"
{-# DEPRECATED cpviNewPublicVirtualInterface "Use generic-lens or generic-optics with 'newPublicVirtualInterface' instead." #-}

instance Core.FromJSON CreatePublicVirtualInterface where
  toJSON CreatePublicVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just
              ("newPublicVirtualInterface" Core..= newPublicVirtualInterface)
          ]
      )

instance Core.AWSRequest CreatePublicVirtualInterface where
  type Rs CreatePublicVirtualInterface = Types.VirtualInterface
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.CreatePublicVirtualInterface")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
