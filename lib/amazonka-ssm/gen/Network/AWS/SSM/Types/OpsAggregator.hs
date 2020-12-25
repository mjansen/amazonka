{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsAggregator
  ( OpsAggregator (..),

    -- * Smart constructor
    mkOpsAggregator,

    -- * Lenses
    oaAggregatorType,
    oaAggregators,
    oaAttributeName,
    oaFilters,
    oaTypeName,
    oaValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AggregatorType as Types
import qualified Network.AWS.SSM.Types.OpsAggregatorValue as Types
import qualified Network.AWS.SSM.Types.OpsAggregatorValueKey as Types
import qualified Network.AWS.SSM.Types.OpsDataAttributeName as Types
import qualified Network.AWS.SSM.Types.OpsFilter as Types
import qualified Network.AWS.SSM.Types.TypeName as Types

-- | One or more aggregators for viewing counts of OpsItems using different dimensions such as @Source@ , @CreatedTime@ , or @Source and CreatedTime@ , to name a few.
--
-- /See:/ 'mkOpsAggregator' smart constructor.
data OpsAggregator = OpsAggregator'
  { -- | Either a Range or Count aggregator for limiting an OpsItem summary.
    aggregatorType :: Core.Maybe Types.AggregatorType,
    -- | A nested aggregator for viewing counts of OpsItems.
    aggregators :: Core.Maybe (Core.NonEmpty OpsAggregator),
    -- | The name of an OpsItem attribute on which to limit the count of OpsItems.
    attributeName :: Core.Maybe Types.OpsDataAttributeName,
    -- | The aggregator filters.
    filters :: Core.Maybe (Core.NonEmpty Types.OpsFilter),
    -- | The data type name to use for viewing counts of OpsItems.
    typeName :: Core.Maybe Types.TypeName,
    -- | The aggregator value.
    values :: Core.Maybe (Core.HashMap Types.OpsAggregatorValueKey Types.OpsAggregatorValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsAggregator' value with any optional fields omitted.
mkOpsAggregator ::
  OpsAggregator
mkOpsAggregator =
  OpsAggregator'
    { aggregatorType = Core.Nothing,
      aggregators = Core.Nothing,
      attributeName = Core.Nothing,
      filters = Core.Nothing,
      typeName = Core.Nothing,
      values = Core.Nothing
    }

-- | Either a Range or Count aggregator for limiting an OpsItem summary.
--
-- /Note:/ Consider using 'aggregatorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAggregatorType :: Lens.Lens' OpsAggregator (Core.Maybe Types.AggregatorType)
oaAggregatorType = Lens.field @"aggregatorType"
{-# DEPRECATED oaAggregatorType "Use generic-lens or generic-optics with 'aggregatorType' instead." #-}

-- | A nested aggregator for viewing counts of OpsItems.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAggregators :: Lens.Lens' OpsAggregator (Core.Maybe (Core.NonEmpty OpsAggregator))
oaAggregators = Lens.field @"aggregators"
{-# DEPRECATED oaAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | The name of an OpsItem attribute on which to limit the count of OpsItems.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAttributeName :: Lens.Lens' OpsAggregator (Core.Maybe Types.OpsDataAttributeName)
oaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED oaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The aggregator filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaFilters :: Lens.Lens' OpsAggregator (Core.Maybe (Core.NonEmpty Types.OpsFilter))
oaFilters = Lens.field @"filters"
{-# DEPRECATED oaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The data type name to use for viewing counts of OpsItems.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaTypeName :: Lens.Lens' OpsAggregator (Core.Maybe Types.TypeName)
oaTypeName = Lens.field @"typeName"
{-# DEPRECATED oaTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The aggregator value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaValues :: Lens.Lens' OpsAggregator (Core.Maybe (Core.HashMap Types.OpsAggregatorValueKey Types.OpsAggregatorValue))
oaValues = Lens.field @"values"
{-# DEPRECATED oaValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON OpsAggregator where
  toJSON OpsAggregator {..} =
    Core.object
      ( Core.catMaybes
          [ ("AggregatorType" Core..=) Core.<$> aggregatorType,
            ("Aggregators" Core..=) Core.<$> aggregators,
            ("AttributeName" Core..=) Core.<$> attributeName,
            ("Filters" Core..=) Core.<$> filters,
            ("TypeName" Core..=) Core.<$> typeName,
            ("Values" Core..=) Core.<$> values
          ]
      )
