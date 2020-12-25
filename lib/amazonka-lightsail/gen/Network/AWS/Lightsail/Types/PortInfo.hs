{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfo
  ( PortInfo (..),

    -- * Smart constructor
    mkPortInfo,

    -- * Lenses
    piCidrListAliases,
    piCidrs,
    piFromPort,
    piProtocol,
    piToPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NetworkProtocol as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes ports to open on an instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
-- /See:/ 'mkPortInfo' smart constructor.
data PortInfo = PortInfo'
  { -- | An alias that defines access for a preconfigured range of IP addresses.
    --
    -- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
    cidrListAliases :: Core.Maybe [Types.String],
    -- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
    --
    -- Examples:
    --
    --     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .
    --
    --
    --     * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ .
    --
    --
    -- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
    cidrs :: Core.Maybe [Types.String],
    -- | The first port in a range of open ports on an instance.
    --
    -- Allowed ports:
    --
    --     * TCP and UDP - @0@ to @65535@
    --
    --
    --     * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
    fromPort :: Core.Maybe Core.Int,
    -- | The IP protocol name.
    --
    -- The name can be one of the following:
    --
    --     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.
    --
    --
    --     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .
    --
    --
    --     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
    --
    --
    --     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
    protocol :: Core.Maybe Types.NetworkProtocol,
    -- | The last port in a range of open ports on an instance.
    --
    -- Allowed ports:
    --
    --     * TCP and UDP - @0@ to @65535@
    --
    --
    --     * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PortInfo' value with any optional fields omitted.
mkPortInfo ::
  PortInfo
mkPortInfo =
  PortInfo'
    { cidrListAliases = Core.Nothing,
      cidrs = Core.Nothing,
      fromPort = Core.Nothing,
      protocol = Core.Nothing,
      toPort = Core.Nothing
    }

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- /Note:/ Consider using 'cidrListAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCidrListAliases :: Lens.Lens' PortInfo (Core.Maybe [Types.String])
piCidrListAliases = Lens.field @"cidrListAliases"
{-# DEPRECATED piCidrListAliases "Use generic-lens or generic-optics with 'cidrListAliases' instead." #-}

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- Examples:
--
--     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .
--
--
--     * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ .
--
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCidrs :: Lens.Lens' PortInfo (Core.Maybe [Types.String])
piCidrs = Lens.field @"cidrs"
{-# DEPRECATED piCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

-- | The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFromPort :: Lens.Lens' PortInfo (Core.Maybe Core.Int)
piFromPort = Lens.field @"fromPort"
{-# DEPRECATED piFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The IP protocol name.
--
-- The name can be one of the following:
--
--     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.
--
--
--     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .
--
--
--     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
--
--     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
--
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piProtocol :: Lens.Lens' PortInfo (Core.Maybe Types.NetworkProtocol)
piProtocol = Lens.field @"protocol"
{-# DEPRECATED piProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piToPort :: Lens.Lens' PortInfo (Core.Maybe Core.Int)
piToPort = Lens.field @"toPort"
{-# DEPRECATED piToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Core.FromJSON PortInfo where
  toJSON PortInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("cidrListAliases" Core..=) Core.<$> cidrListAliases,
            ("cidrs" Core..=) Core.<$> cidrs,
            ("fromPort" Core..=) Core.<$> fromPort,
            ("protocol" Core..=) Core.<$> protocol,
            ("toPort" Core..=) Core.<$> toPort
          ]
      )
