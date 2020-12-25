{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Database
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Database
  ( Database (..),

    -- * Smart constructor
    mkDatabase,

    -- * Lenses
    dName,
    dCatalogId,
    dCreateTableDefaultPermissions,
    dCreateTime,
    dDescription,
    dLocationUri,
    dParameters,
    dTargetDatabase,
  )
where

import qualified Network.AWS.Glue.Types.CatalogId as Types
import qualified Network.AWS.Glue.Types.DatabaseIdentifier as Types
import qualified Network.AWS.Glue.Types.Description as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.LocationUri as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.PrincipalPermissions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @Database@ object represents a logical grouping of tables that might reside in a Hive metastore or an RDBMS.
--
-- /See:/ 'mkDatabase' smart constructor.
data Database = Database'
  { -- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
    name :: Types.Name,
    -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Core.Maybe Types.CatalogId,
    -- | Creates a set of default permissions on the table for principals.
    createTableDefaultPermissions :: Core.Maybe [Types.PrincipalPermissions],
    -- | The time at which the metadata database was created in the catalog.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the database.
    description :: Core.Maybe Types.Description,
    -- | The location of the database (for example, an HDFS path).
    locationUri :: Core.Maybe Types.LocationUri,
    -- | These key-value pairs define parameters and properties of the database.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
    targetDatabase :: Core.Maybe Types.DatabaseIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Database' value with any optional fields omitted.
mkDatabase ::
  -- | 'name'
  Types.Name ->
  Database
mkDatabase name =
  Database'
    { name,
      catalogId = Core.Nothing,
      createTableDefaultPermissions = Core.Nothing,
      createTime = Core.Nothing,
      description = Core.Nothing,
      locationUri = Core.Nothing,
      parameters = Core.Nothing,
      targetDatabase = Core.Nothing
    }

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Database Types.Name
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Data Catalog in which the database resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCatalogId :: Lens.Lens' Database (Core.Maybe Types.CatalogId)
dCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | Creates a set of default permissions on the table for principals.
--
-- /Note:/ Consider using 'createTableDefaultPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTableDefaultPermissions :: Lens.Lens' Database (Core.Maybe [Types.PrincipalPermissions])
dCreateTableDefaultPermissions = Lens.field @"createTableDefaultPermissions"
{-# DEPRECATED dCreateTableDefaultPermissions "Use generic-lens or generic-optics with 'createTableDefaultPermissions' instead." #-}

-- | The time at which the metadata database was created in the catalog.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTime :: Lens.Lens' Database (Core.Maybe Core.NominalDiffTime)
dCreateTime = Lens.field @"createTime"
{-# DEPRECATED dCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | A description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Database (Core.Maybe Types.Description)
dDescription = Lens.field @"description"
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The location of the database (for example, an HDFS path).
--
-- /Note:/ Consider using 'locationUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLocationUri :: Lens.Lens' Database (Core.Maybe Types.LocationUri)
dLocationUri = Lens.field @"locationUri"
{-# DEPRECATED dLocationUri "Use generic-lens or generic-optics with 'locationUri' instead." #-}

-- | These key-value pairs define parameters and properties of the database.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameters :: Lens.Lens' Database (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
dParameters = Lens.field @"parameters"
{-# DEPRECATED dParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
--
-- /Note:/ Consider using 'targetDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetDatabase :: Lens.Lens' Database (Core.Maybe Types.DatabaseIdentifier)
dTargetDatabase = Lens.field @"targetDatabase"
{-# DEPRECATED dTargetDatabase "Use generic-lens or generic-optics with 'targetDatabase' instead." #-}

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject "Database" Core.$
      \x ->
        Database'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "CatalogId")
          Core.<*> (x Core..:? "CreateTableDefaultPermissions")
          Core.<*> (x Core..:? "CreateTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "LocationUri")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "TargetDatabase")
