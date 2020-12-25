{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceEntry
  ( InstanceEntry (..),

    -- * Smart constructor
    mkInstanceEntry,

    -- * Lenses
    ieSourceName,
    ieInstanceType,
    iePortInfoSource,
    ieAvailabilityZone,
    ieUserData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.PortInfoSourceType as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon Elastic Compute Cloud instance and related resources to be created using the @create cloud formation stack@ operation.
--
-- /See:/ 'mkInstanceEntry' smart constructor.
data InstanceEntry = InstanceEntry'
  { -- | The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance.
    --
    -- Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
    sourceName :: Types.ResourceName,
    -- | The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
    instanceType :: Types.NonEmptyString,
    -- | The port configuration to use for the new Amazon EC2 instance.
    --
    -- The following configuration options are available:
    --
    --     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.
    --
    --
    --     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.
    --
    --
    --     * @NONE@ - Use the default Amazon EC2 security group.
    --
    --
    --     * @CLOSED@ - All ports closed.
    portInfoSource :: Types.PortInfoSourceType,
    -- | The Availability Zone for the new Amazon EC2 instance.
    availabilityZone :: Types.String,
    -- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
    userData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceEntry' value with any optional fields omitted.
mkInstanceEntry ::
  -- | 'sourceName'
  Types.ResourceName ->
  -- | 'instanceType'
  Types.NonEmptyString ->
  -- | 'portInfoSource'
  Types.PortInfoSourceType ->
  -- | 'availabilityZone'
  Types.String ->
  InstanceEntry
mkInstanceEntry
  sourceName
  instanceType
  portInfoSource
  availabilityZone =
    InstanceEntry'
      { sourceName,
        instanceType,
        portInfoSource,
        availabilityZone,
        userData = Core.Nothing
      }

-- | The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance.
--
-- Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
--
-- /Note:/ Consider using 'sourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSourceName :: Lens.Lens' InstanceEntry Types.ResourceName
ieSourceName = Lens.field @"sourceName"
{-# DEPRECATED ieSourceName "Use generic-lens or generic-optics with 'sourceName' instead." #-}

-- | The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieInstanceType :: Lens.Lens' InstanceEntry Types.NonEmptyString
ieInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ieInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The port configuration to use for the new Amazon EC2 instance.
--
-- The following configuration options are available:
--
--     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.
--
--
--     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.
--
--
--     * @NONE@ - Use the default Amazon EC2 security group.
--
--
--     * @CLOSED@ - All ports closed.
--
--
--
-- /Note:/ Consider using 'portInfoSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iePortInfoSource :: Lens.Lens' InstanceEntry Types.PortInfoSourceType
iePortInfoSource = Lens.field @"portInfoSource"
{-# DEPRECATED iePortInfoSource "Use generic-lens or generic-optics with 'portInfoSource' instead." #-}

-- | The Availability Zone for the new Amazon EC2 instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieAvailabilityZone :: Lens.Lens' InstanceEntry Types.String
ieAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ieAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieUserData :: Lens.Lens' InstanceEntry (Core.Maybe Types.String)
ieUserData = Lens.field @"userData"
{-# DEPRECATED ieUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

instance Core.FromJSON InstanceEntry where
  toJSON InstanceEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("sourceName" Core..= sourceName),
            Core.Just ("instanceType" Core..= instanceType),
            Core.Just ("portInfoSource" Core..= portInfoSource),
            Core.Just ("availabilityZone" Core..= availabilityZone),
            ("userData" Core..=) Core.<$> userData
          ]
      )
