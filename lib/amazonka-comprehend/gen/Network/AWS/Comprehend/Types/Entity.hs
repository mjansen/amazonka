{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Entity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.Entity
  ( Entity (..),

    -- * Smart constructor
    mkEntity,

    -- * Lenses
    eBeginOffset,
    eEndOffset,
    eScore,
    eText,
    eType,
  )
where

import qualified Network.AWS.Comprehend.Types.EntityType as Types
import qualified Network.AWS.Comprehend.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about an entity.
--
--
--
-- /See:/ 'mkEntity' smart constructor.
data Entity = Entity'
  { -- | A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Core.Maybe Core.Int,
    -- | A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
    endOffset :: Core.Maybe Core.Int,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
    score :: Core.Maybe Core.Double,
    -- | The text of the entity.
    text :: Core.Maybe Types.String,
    -- | The entity's type.
    type' :: Core.Maybe Types.EntityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Entity' value with any optional fields omitted.
mkEntity ::
  Entity
mkEntity =
  Entity'
    { beginOffset = Core.Nothing,
      endOffset = Core.Nothing,
      score = Core.Nothing,
      text = Core.Nothing,
      type' = Core.Nothing
    }

-- | A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'beginOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eBeginOffset :: Lens.Lens' Entity (Core.Maybe Core.Int)
eBeginOffset = Lens.field @"beginOffset"
{-# DEPRECATED eBeginOffset "Use generic-lens or generic-optics with 'beginOffset' instead." #-}

-- | A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- /Note:/ Consider using 'endOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndOffset :: Lens.Lens' Entity (Core.Maybe Core.Int)
eEndOffset = Lens.field @"endOffset"
{-# DEPRECATED eEndOffset "Use generic-lens or generic-optics with 'endOffset' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eScore :: Lens.Lens' Entity (Core.Maybe Core.Double)
eScore = Lens.field @"score"
{-# DEPRECATED eScore "Use generic-lens or generic-optics with 'score' instead." #-}

-- | The text of the entity.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eText :: Lens.Lens' Entity (Core.Maybe Types.String)
eText = Lens.field @"text"
{-# DEPRECATED eText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The entity's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eType :: Lens.Lens' Entity (Core.Maybe Types.EntityType)
eType = Lens.field @"type'"
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Entity where
  parseJSON =
    Core.withObject "Entity" Core.$
      \x ->
        Entity'
          Core.<$> (x Core..:? "BeginOffset")
          Core.<*> (x Core..:? "EndOffset")
          Core.<*> (x Core..:? "Score")
          Core.<*> (x Core..:? "Text")
          Core.<*> (x Core..:? "Type")
