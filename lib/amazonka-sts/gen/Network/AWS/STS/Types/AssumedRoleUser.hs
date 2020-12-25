{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.AssumedRoleUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.AssumedRoleUser
  ( AssumedRoleUser (..),

    -- * Smart constructor
    mkAssumedRoleUser,

    -- * Lenses
    aruAssumedRoleId,
    aruArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.STS.Types.ArnType as Types
import qualified Network.AWS.STS.Types.AssumedRoleId as Types

-- | The identifiers for the temporary security credentials that the operation returns.
--
-- /See:/ 'mkAssumedRoleUser' smart constructor.
data AssumedRoleUser = AssumedRoleUser'
  { -- | A unique identifier that contains the role ID and the role session name of the role that is being assumed. The role ID is generated by AWS when the role is created.
    assumedRoleId :: Types.AssumedRoleId,
    -- | The ARN of the temporary security credentials that are returned from the 'AssumeRole' action. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    arn :: Types.ArnType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssumedRoleUser' value with any optional fields omitted.
mkAssumedRoleUser ::
  -- | 'assumedRoleId'
  Types.AssumedRoleId ->
  -- | 'arn'
  Types.ArnType ->
  AssumedRoleUser
mkAssumedRoleUser assumedRoleId arn =
  AssumedRoleUser' {assumedRoleId, arn}

-- | A unique identifier that contains the role ID and the role session name of the role that is being assumed. The role ID is generated by AWS when the role is created.
--
-- /Note:/ Consider using 'assumedRoleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aruAssumedRoleId :: Lens.Lens' AssumedRoleUser Types.AssumedRoleId
aruAssumedRoleId = Lens.field @"assumedRoleId"
{-# DEPRECATED aruAssumedRoleId "Use generic-lens or generic-optics with 'assumedRoleId' instead." #-}

-- | The ARN of the temporary security credentials that are returned from the 'AssumeRole' action. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aruArn :: Lens.Lens' AssumedRoleUser Types.ArnType
aruArn = Lens.field @"arn"
{-# DEPRECATED aruArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromXML AssumedRoleUser where
  parseXML x =
    AssumedRoleUser'
      Core.<$> (x Core..@ "AssumedRoleId") Core.<*> (x Core..@ "Arn")
