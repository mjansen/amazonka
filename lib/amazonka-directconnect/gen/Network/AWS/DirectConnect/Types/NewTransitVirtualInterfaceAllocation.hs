{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
  ( NewTransitVirtualInterfaceAllocation (..),

    -- * Smart constructor
    mkNewTransitVirtualInterfaceAllocation,

    -- * Lenses
    ntviaAddressFamily,
    ntviaAmazonAddress,
    ntviaAsn,
    ntviaAuthKey,
    ntviaCustomerAddress,
    ntviaMtu,
    ntviaTags,
    ntviaVirtualInterfaceName,
    ntviaVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a transit virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewTransitVirtualInterfaceAllocation' smart constructor.
data NewTransitVirtualInterfaceAllocation = NewTransitVirtualInterfaceAllocation'
  { -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe Types.AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Types.AmazonAddress,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Maybe Core.Int,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Types.BGPAuthKey,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Types.CustomerAddress,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the transitive virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Core.Maybe Types.VirtualInterfaceName,
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewTransitVirtualInterfaceAllocation' value with any optional fields omitted.
mkNewTransitVirtualInterfaceAllocation ::
  NewTransitVirtualInterfaceAllocation
mkNewTransitVirtualInterfaceAllocation =
  NewTransitVirtualInterfaceAllocation'
    { addressFamily =
        Core.Nothing,
      amazonAddress = Core.Nothing,
      asn = Core.Nothing,
      authKey = Core.Nothing,
      customerAddress = Core.Nothing,
      mtu = Core.Nothing,
      tags = Core.Nothing,
      virtualInterfaceName = Core.Nothing,
      vlan = Core.Nothing
    }

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAddressFamily :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Types.AddressFamily)
ntviaAddressFamily = Lens.field @"addressFamily"
{-# DEPRECATED ntviaAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAmazonAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Types.AmazonAddress)
ntviaAmazonAddress = Lens.field @"amazonAddress"
{-# DEPRECATED ntviaAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAsn :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
ntviaAsn = Lens.field @"asn"
{-# DEPRECATED ntviaAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAuthKey :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Types.BGPAuthKey)
ntviaAuthKey = Lens.field @"authKey"
{-# DEPRECATED ntviaAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaCustomerAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Types.CustomerAddress)
ntviaCustomerAddress = Lens.field @"customerAddress"
{-# DEPRECATED ntviaCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaMtu :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
ntviaMtu = Lens.field @"mtu"
{-# DEPRECATED ntviaMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The tags associated with the transitive virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaTags :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe (Core.NonEmpty Types.Tag))
ntviaTags = Lens.field @"tags"
{-# DEPRECATED ntviaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaVirtualInterfaceName :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Types.VirtualInterfaceName)
ntviaVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# DEPRECATED ntviaVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaVlan :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
ntviaVlan = Lens.field @"vlan"
{-# DEPRECATED ntviaVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

instance Core.FromJSON NewTransitVirtualInterfaceAllocation where
  toJSON NewTransitVirtualInterfaceAllocation {..} =
    Core.object
      ( Core.catMaybes
          [ ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("asn" Core..=) Core.<$> asn,
            ("authKey" Core..=) Core.<$> authKey,
            ("customerAddress" Core..=) Core.<$> customerAddress,
            ("mtu" Core..=) Core.<$> mtu,
            ("tags" Core..=) Core.<$> tags,
            ("virtualInterfaceName" Core..=) Core.<$> virtualInterfaceName,
            ("vlan" Core..=) Core.<$> vlan
          ]
      )
