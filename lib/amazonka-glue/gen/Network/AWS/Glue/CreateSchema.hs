{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema set and registers the schema definition. Returns an error if the schema set already exists without actually registering the version.
--
-- When the schema set is created, a version checkpoint will be set to the first version. Compatibility mode "DISABLED" restricts any additional schema versions from being added after the first schema version. For all other compatibility modes, validation of compatibility settings will be applied only from the second version onwards when the @RegisterSchemaVersion@ API is used.
-- When this API is called without a @RegistryId@ , this will create an entry for a "default-registry" in the registry database tables, if it is not already present.
module Network.AWS.Glue.CreateSchema
  ( -- * Creating a request
    CreateSchema (..),
    mkCreateSchema,

    -- ** Request lenses
    csSchemaName,
    csDataFormat,
    csCompatibility,
    csDescription,
    csRegistryId,
    csSchemaDefinition,
    csTags,

    -- * Destructuring the response
    CreateSchemaResponse (..),
    mkCreateSchemaResponse,

    -- ** Response lenses
    csrrsCompatibility,
    csrrsDataFormat,
    csrrsDescription,
    csrrsLatestSchemaVersion,
    csrrsNextSchemaVersion,
    csrrsRegistryArn,
    csrrsRegistryName,
    csrrsSchemaArn,
    csrrsSchemaCheckpoint,
    csrrsSchemaName,
    csrrsSchemaStatus,
    csrrsSchemaVersionId,
    csrrsSchemaVersionStatus,
    csrrsTags,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
    schemaName :: Types.SchemaName,
    -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Types.DataFormat,
    -- | The compatibility mode of the schema. The possible values are:
    --
    --
    --     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.
    --
    --
    --     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.
    --
    --
    --     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.
    --
    --
    --     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.
    --
    --
    --     * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.
    --
    --
    --     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.
    --
    --
    --     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.
    --
    --
    --     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
    compatibility :: Core.Maybe Types.Compatibility,
    -- | An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
    description :: Core.Maybe Types.DescriptionString,
    -- | This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
    registryId :: Core.Maybe Types.RegistryId,
    -- | The schema definition using the @DataFormat@ setting for @SchemaName@ .
    schemaDefinition :: Core.Maybe Types.SchemaDefinitionString,
    -- | AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchema' value with any optional fields omitted.
mkCreateSchema ::
  -- | 'schemaName'
  Types.SchemaName ->
  -- | 'dataFormat'
  Types.DataFormat ->
  CreateSchema
mkCreateSchema schemaName dataFormat =
  CreateSchema'
    { schemaName,
      dataFormat,
      compatibility = Core.Nothing,
      description = Core.Nothing,
      registryId = Core.Nothing,
      schemaDefinition = Core.Nothing,
      tags = Core.Nothing
    }

-- | Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSchemaName :: Lens.Lens' CreateSchema Types.SchemaName
csSchemaName = Lens.field @"schemaName"
{-# DEPRECATED csSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDataFormat :: Lens.Lens' CreateSchema Types.DataFormat
csDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED csDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The compatibility mode of the schema. The possible values are:
--
--
--     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.
--
--
--     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.
--
--
--     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.
--
--
--     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.
--
--
--     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.
--
--
--     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
--
--
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompatibility :: Lens.Lens' CreateSchema (Core.Maybe Types.Compatibility)
csCompatibility = Lens.field @"compatibility"
{-# DEPRECATED csCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSchema (Core.Maybe Types.DescriptionString)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRegistryId :: Lens.Lens' CreateSchema (Core.Maybe Types.RegistryId)
csRegistryId = Lens.field @"registryId"
{-# DEPRECATED csRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The schema definition using the @DataFormat@ setting for @SchemaName@ .
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSchemaDefinition :: Lens.Lens' CreateSchema (Core.Maybe Types.SchemaDefinitionString)
csSchemaDefinition = Lens.field @"schemaDefinition"
{-# DEPRECATED csSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateSchema (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSchema where
  toJSON CreateSchema {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaName" Core..= schemaName),
            Core.Just ("DataFormat" Core..= dataFormat),
            ("Compatibility" Core..=) Core.<$> compatibility,
            ("Description" Core..=) Core.<$> description,
            ("RegistryId" Core..=) Core.<$> registryId,
            ("SchemaDefinition" Core..=) Core.<$> schemaDefinition,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateSchema")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Core.<$> (x Core..:? "Compatibility")
            Core.<*> (x Core..:? "DataFormat")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LatestSchemaVersion")
            Core.<*> (x Core..:? "NextSchemaVersion")
            Core.<*> (x Core..:? "RegistryArn")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "SchemaCheckpoint")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (x Core..:? "SchemaStatus")
            Core.<*> (x Core..:? "SchemaVersionId")
            Core.<*> (x Core..:? "SchemaVersionStatus")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The schema compatibility mode.
    compatibility :: Core.Maybe Types.Compatibility,
    -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Core.Maybe Types.DataFormat,
    -- | A description of the schema if specified when created.
    description :: Core.Maybe Types.DescriptionString,
    -- | The latest version of the schema associated with the returned schema definition.
    latestSchemaVersion :: Core.Maybe Core.Natural,
    -- | The next version of the schema associated with the returned schema definition.
    nextSchemaVersion :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the registry.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The version number of the checkpoint (the last time the compatibility mode was changed).
    schemaCheckpoint :: Core.Maybe Core.Natural,
    -- | The name of the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The status of the schema.
    schemaStatus :: Core.Maybe Types.SchemaStatus,
    -- | The unique identifier of the first schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The status of the first schema version created.
    schemaVersionStatus :: Core.Maybe Types.SchemaVersionStatus,
    -- | The tags for the schema.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSchemaResponse' value with any optional fields omitted.
mkCreateSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSchemaResponse
mkCreateSchemaResponse responseStatus =
  CreateSchemaResponse'
    { compatibility = Core.Nothing,
      dataFormat = Core.Nothing,
      description = Core.Nothing,
      latestSchemaVersion = Core.Nothing,
      nextSchemaVersion = Core.Nothing,
      registryArn = Core.Nothing,
      registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaCheckpoint = Core.Nothing,
      schemaName = Core.Nothing,
      schemaStatus = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaVersionStatus = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The schema compatibility mode.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsCompatibility :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.Compatibility)
csrrsCompatibility = Lens.field @"compatibility"
{-# DEPRECATED csrrsCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsDataFormat :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.DataFormat)
csrrsDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED csrrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | A description of the schema if specified when created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsDescription :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.DescriptionString)
csrrsDescription = Lens.field @"description"
{-# DEPRECATED csrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The latest version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'latestSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsLatestSchemaVersion :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
csrrsLatestSchemaVersion = Lens.field @"latestSchemaVersion"
{-# DEPRECATED csrrsLatestSchemaVersion "Use generic-lens or generic-optics with 'latestSchemaVersion' instead." #-}

-- | The next version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'nextSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsNextSchemaVersion :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
csrrsNextSchemaVersion = Lens.field @"nextSchemaVersion"
{-# DEPRECATED csrrsNextSchemaVersion "Use generic-lens or generic-optics with 'nextSchemaVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsRegistryArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.GlueResourceArn)
csrrsRegistryArn = Lens.field @"registryArn"
{-# DEPRECATED csrrsRegistryArn "Use generic-lens or generic-optics with 'registryArn' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsRegistryName :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.RegistryName)
csrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED csrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.GlueResourceArn)
csrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED csrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- /Note:/ Consider using 'schemaCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaCheckpoint :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
csrrsSchemaCheckpoint = Lens.field @"schemaCheckpoint"
{-# DEPRECATED csrrsSchemaCheckpoint "Use generic-lens or generic-optics with 'schemaCheckpoint' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaName :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.SchemaName)
csrrsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED csrrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaStatus :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.SchemaStatus)
csrrsSchemaStatus = Lens.field @"schemaStatus"
{-# DEPRECATED csrrsSchemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead." #-}

-- | The unique identifier of the first schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaVersionId :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.SchemaVersionId)
csrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED csrrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The status of the first schema version created.
--
-- /Note:/ Consider using 'schemaVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSchemaVersionStatus :: Lens.Lens' CreateSchemaResponse (Core.Maybe Types.SchemaVersionStatus)
csrrsSchemaVersionStatus = Lens.field @"schemaVersionStatus"
{-# DEPRECATED csrrsSchemaVersionStatus "Use generic-lens or generic-optics with 'schemaVersionStatus' instead." #-}

-- | The tags for the schema.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsTags :: Lens.Lens' CreateSchemaResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
csrrsTags = Lens.field @"tags"
{-# DEPRECATED csrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSchemaResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
