{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ChannelSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ChannelSpecification
  ( ChannelSpecification (..),

    -- * Smart constructor
    mkChannelSpecification,

    -- * Lenses
    csName,
    csSupportedContentTypes,
    csSupportedInputModes,
    csDescription,
    csIsRequired,
    csSupportedCompressionTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ChannelName as Types
import qualified Network.AWS.SageMaker.Types.CompressionType as Types
import qualified Network.AWS.SageMaker.Types.ContentType as Types
import qualified Network.AWS.SageMaker.Types.EntityDescription as Types
import qualified Network.AWS.SageMaker.Types.TrainingInputMode as Types

-- | Defines a named input source, called a channel, to be used by an algorithm.
--
-- /See:/ 'mkChannelSpecification' smart constructor.
data ChannelSpecification = ChannelSpecification'
  { -- | The name of the channel.
    name :: Types.ChannelName,
    -- | The supported MIME types for the data.
    supportedContentTypes :: [Types.ContentType],
    -- | The allowed input mode, either FILE or PIPE.
    --
    -- In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode.
    -- In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
    supportedInputModes :: Core.NonEmpty Types.TrainingInputMode,
    -- | A brief description of the channel.
    description :: Core.Maybe Types.EntityDescription,
    -- | Indicates whether the channel is required by the algorithm.
    isRequired :: Core.Maybe Core.Bool,
    -- | The allowed compression types, if data compression is used.
    supportedCompressionTypes :: Core.Maybe [Types.CompressionType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelSpecification' value with any optional fields omitted.
mkChannelSpecification ::
  -- | 'name'
  Types.ChannelName ->
  -- | 'supportedInputModes'
  Core.NonEmpty Types.TrainingInputMode ->
  ChannelSpecification
mkChannelSpecification name supportedInputModes =
  ChannelSpecification'
    { name,
      supportedContentTypes = Core.mempty,
      supportedInputModes,
      description = Core.Nothing,
      isRequired = Core.Nothing,
      supportedCompressionTypes = Core.Nothing
    }

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ChannelSpecification Types.ChannelName
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The supported MIME types for the data.
--
-- /Note:/ Consider using 'supportedContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedContentTypes :: Lens.Lens' ChannelSpecification [Types.ContentType]
csSupportedContentTypes = Lens.field @"supportedContentTypes"
{-# DEPRECATED csSupportedContentTypes "Use generic-lens or generic-optics with 'supportedContentTypes' instead." #-}

-- | The allowed input mode, either FILE or PIPE.
--
-- In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode.
-- In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
--
-- /Note:/ Consider using 'supportedInputModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedInputModes :: Lens.Lens' ChannelSpecification (Core.NonEmpty Types.TrainingInputMode)
csSupportedInputModes = Lens.field @"supportedInputModes"
{-# DEPRECATED csSupportedInputModes "Use generic-lens or generic-optics with 'supportedInputModes' instead." #-}

-- | A brief description of the channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' ChannelSpecification (Core.Maybe Types.EntityDescription)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the channel is required by the algorithm.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIsRequired :: Lens.Lens' ChannelSpecification (Core.Maybe Core.Bool)
csIsRequired = Lens.field @"isRequired"
{-# DEPRECATED csIsRequired "Use generic-lens or generic-optics with 'isRequired' instead." #-}

-- | The allowed compression types, if data compression is used.
--
-- /Note:/ Consider using 'supportedCompressionTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSupportedCompressionTypes :: Lens.Lens' ChannelSpecification (Core.Maybe [Types.CompressionType])
csSupportedCompressionTypes = Lens.field @"supportedCompressionTypes"
{-# DEPRECATED csSupportedCompressionTypes "Use generic-lens or generic-optics with 'supportedCompressionTypes' instead." #-}

instance Core.FromJSON ChannelSpecification where
  toJSON ChannelSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("SupportedContentTypes" Core..= supportedContentTypes),
            Core.Just ("SupportedInputModes" Core..= supportedInputModes),
            ("Description" Core..=) Core.<$> description,
            ("IsRequired" Core..=) Core.<$> isRequired,
            ("SupportedCompressionTypes" Core..=)
              Core.<$> supportedCompressionTypes
          ]
      )

instance Core.FromJSON ChannelSpecification where
  parseJSON =
    Core.withObject "ChannelSpecification" Core.$
      \x ->
        ChannelSpecification'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "SupportedContentTypes" Core..!= Core.mempty)
          Core.<*> (x Core..: "SupportedInputModes")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsRequired")
          Core.<*> (x Core..:? "SupportedCompressionTypes")
