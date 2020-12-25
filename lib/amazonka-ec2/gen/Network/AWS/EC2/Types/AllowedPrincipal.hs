{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowedPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowedPrincipal
  ( AllowedPrincipal (..),

    -- * Smart constructor
    mkAllowedPrincipal,

    -- * Lenses
    apPrincipal,
    apPrincipalType,
  )
where

import qualified Network.AWS.EC2.Types.Principal as Types
import qualified Network.AWS.EC2.Types.PrincipalType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a principal.
--
-- /See:/ 'mkAllowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { -- | The Amazon Resource Name (ARN) of the principal.
    principal :: Core.Maybe Types.Principal,
    -- | The type of principal.
    principalType :: Core.Maybe Types.PrincipalType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllowedPrincipal' value with any optional fields omitted.
mkAllowedPrincipal ::
  AllowedPrincipal
mkAllowedPrincipal =
  AllowedPrincipal'
    { principal = Core.Nothing,
      principalType = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the principal.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipal :: Lens.Lens' AllowedPrincipal (Core.Maybe Types.Principal)
apPrincipal = Lens.field @"principal"
{-# DEPRECATED apPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The type of principal.
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipalType :: Lens.Lens' AllowedPrincipal (Core.Maybe Types.PrincipalType)
apPrincipalType = Lens.field @"principalType"
{-# DEPRECATED apPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

instance Core.FromXML AllowedPrincipal where
  parseXML x =
    AllowedPrincipal'
      Core.<$> (x Core..@? "principal") Core.<*> (x Core..@? "principalType")
