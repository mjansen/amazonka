{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InstanceIdDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InstanceIdDetail
  ( InstanceIdDetail (..),

    -- * Smart constructor
    mkInstanceIdDetail,

    -- * Lenses
    iidId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.String as Types

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
-- /See:/ 'mkInstanceIdDetail' smart constructor.
newtype InstanceIdDetail = InstanceIdDetail'
  { -- | The ID of a corresponding EC2 instance.
    id :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceIdDetail' value with any optional fields omitted.
mkInstanceIdDetail ::
  InstanceIdDetail
mkInstanceIdDetail = InstanceIdDetail' {id = Core.Nothing}

-- | The ID of a corresponding EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iidId :: Lens.Lens' InstanceIdDetail (Core.Maybe Types.String)
iidId = Lens.field @"id"
{-# DEPRECATED iidId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON InstanceIdDetail where
  parseJSON =
    Core.withObject "InstanceIdDetail" Core.$
      \x -> InstanceIdDetail' Core.<$> (x Core..:? "Id")
