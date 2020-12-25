{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchCreatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchCreatePartition
  ( -- * Creating a request
    BatchCreatePartition (..),
    mkBatchCreatePartition,

    -- ** Request lenses
    bcpDatabaseName,
    bcpTableName,
    bcpPartitionInputList,
    bcpCatalogId,

    -- * Destructuring the response
    BatchCreatePartitionResponse (..),
    mkBatchCreatePartitionResponse,

    -- ** Response lenses
    bcprrsErrors,
    bcprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchCreatePartition' smart constructor.
data BatchCreatePartition = BatchCreatePartition'
  { -- | The name of the metadata database in which the partition is to be created.
    databaseName :: Types.NameString,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Types.NameString,
    -- | A list of @PartitionInput@ structures that define the partitions to be created.
    partitionInputList :: [Types.PartitionInput],
    -- | The ID of the catalog in which the partition is to be created. Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchCreatePartition' value with any optional fields omitted.
mkBatchCreatePartition ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  BatchCreatePartition
mkBatchCreatePartition databaseName tableName =
  BatchCreatePartition'
    { databaseName,
      tableName,
      partitionInputList = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the metadata database in which the partition is to be created.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpDatabaseName :: Lens.Lens' BatchCreatePartition Types.NameString
bcpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED bcpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpTableName :: Lens.Lens' BatchCreatePartition Types.NameString
bcpTableName = Lens.field @"tableName"
{-# DEPRECATED bcpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of @PartitionInput@ structures that define the partitions to be created.
--
-- /Note:/ Consider using 'partitionInputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpPartitionInputList :: Lens.Lens' BatchCreatePartition [Types.PartitionInput]
bcpPartitionInputList = Lens.field @"partitionInputList"
{-# DEPRECATED bcpPartitionInputList "Use generic-lens or generic-optics with 'partitionInputList' instead." #-}

-- | The ID of the catalog in which the partition is to be created. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcpCatalogId :: Lens.Lens' BatchCreatePartition (Core.Maybe Types.CatalogId)
bcpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED bcpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON BatchCreatePartition where
  toJSON BatchCreatePartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionInputList" Core..= partitionInputList),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest BatchCreatePartition where
  type Rs BatchCreatePartition = BatchCreatePartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchCreatePartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreatePartitionResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchCreatePartitionResponse' smart constructor.
data BatchCreatePartitionResponse = BatchCreatePartitionResponse'
  { -- | The errors encountered when trying to create the requested partitions.
    errors :: Core.Maybe [Types.PartitionError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCreatePartitionResponse' value with any optional fields omitted.
mkBatchCreatePartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchCreatePartitionResponse
mkBatchCreatePartitionResponse responseStatus =
  BatchCreatePartitionResponse'
    { errors = Core.Nothing,
      responseStatus
    }

-- | The errors encountered when trying to create the requested partitions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcprrsErrors :: Lens.Lens' BatchCreatePartitionResponse (Core.Maybe [Types.PartitionError])
bcprrsErrors = Lens.field @"errors"
{-# DEPRECATED bcprrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcprrsResponseStatus :: Lens.Lens' BatchCreatePartitionResponse Core.Int
bcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
