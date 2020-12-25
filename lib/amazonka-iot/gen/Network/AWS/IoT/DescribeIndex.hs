{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a search index.
module Network.AWS.IoT.DescribeIndex
  ( -- * Creating a request
    DescribeIndex (..),
    mkDescribeIndex,

    -- ** Request lenses
    diIndexName,

    -- * Destructuring the response
    DescribeIndexResponse (..),
    mkDescribeIndexResponse,

    -- ** Response lenses
    dirrsIndexName,
    dirrsIndexStatus,
    dirrsSchema,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIndex' smart constructor.
newtype DescribeIndex = DescribeIndex'
  { -- | The index name.
    indexName :: Types.IndexName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIndex' value with any optional fields omitted.
mkDescribeIndex ::
  -- | 'indexName'
  Types.IndexName ->
  DescribeIndex
mkDescribeIndex indexName = DescribeIndex' {indexName}

-- | The index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIndexName :: Lens.Lens' DescribeIndex Types.IndexName
diIndexName = Lens.field @"indexName"
{-# DEPRECATED diIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Core.AWSRequest DescribeIndex where
  type Rs DescribeIndex = DescribeIndexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/indices/" Core.<> (Core.toText indexName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Core.<$> (x Core..:? "indexName")
            Core.<*> (x Core..:? "indexStatus")
            Core.<*> (x Core..:? "schema")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { -- | The index name.
    indexName :: Core.Maybe Types.IndexName,
    -- | The index status.
    indexStatus :: Core.Maybe Types.IndexStatus,
    -- | Contains a value that specifies the type of indexing performed. Valid values are:
    --
    --
    --     * REGISTRY – Your thing index contains only registry data.
    --
    --
    --     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.
    --
    --
    --     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.
    --
    --
    --     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
    schema :: Core.Maybe Types.IndexSchema,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIndexResponse' value with any optional fields omitted.
mkDescribeIndexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeIndexResponse
mkDescribeIndexResponse responseStatus =
  DescribeIndexResponse'
    { indexName = Core.Nothing,
      indexStatus = Core.Nothing,
      schema = Core.Nothing,
      responseStatus
    }

-- | The index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsIndexName :: Lens.Lens' DescribeIndexResponse (Core.Maybe Types.IndexName)
dirrsIndexName = Lens.field @"indexName"
{-# DEPRECATED dirrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The index status.
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsIndexStatus :: Lens.Lens' DescribeIndexResponse (Core.Maybe Types.IndexStatus)
dirrsIndexStatus = Lens.field @"indexStatus"
{-# DEPRECATED dirrsIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | Contains a value that specifies the type of indexing performed. Valid values are:
--
--
--     * REGISTRY – Your thing index contains only registry data.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.
--
--
--     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.
--
--
--     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
--
--
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsSchema :: Lens.Lens' DescribeIndexResponse (Core.Maybe Types.IndexSchema)
dirrsSchema = Lens.field @"schema"
{-# DEPRECATED dirrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeIndexResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
