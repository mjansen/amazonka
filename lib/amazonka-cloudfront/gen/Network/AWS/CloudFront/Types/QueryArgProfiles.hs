{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfiles
  ( QueryArgProfiles (..),

    -- * Smart constructor
    mkQueryArgProfiles,

    -- * Lenses
    qapQuantity,
    qapItems,
  )
where

import qualified Network.AWS.CloudFront.Types.QueryArgProfile as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { -- | Number of profiles for query argument-profile mapping for field-level encryption.
    quantity :: Core.Int,
    -- | Number of items for query argument-profile mapping for field-level encryption.
    items :: Core.Maybe [Types.QueryArgProfile]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryArgProfiles' value with any optional fields omitted.
mkQueryArgProfiles ::
  -- | 'quantity'
  Core.Int ->
  QueryArgProfiles
mkQueryArgProfiles quantity =
  QueryArgProfiles' {quantity, items = Core.Nothing}

-- | Number of profiles for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapQuantity :: Lens.Lens' QueryArgProfiles Core.Int
qapQuantity = Lens.field @"quantity"
{-# DEPRECATED qapQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Number of items for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapItems :: Lens.Lens' QueryArgProfiles (Core.Maybe [Types.QueryArgProfile])
qapItems = Lens.field @"items"
{-# DEPRECATED qapItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.ToXML QueryArgProfiles where
  toXML QueryArgProfiles {..} =
    Core.toXMLNode "Quantity" quantity
      Core.<> Core.toXMLNode
        "Items"
        (Core.toXMLList "QueryArgProfile" Core.<$> items)

instance Core.FromXML QueryArgProfiles where
  parseXML x =
    QueryArgProfiles'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "QueryArgProfile")
