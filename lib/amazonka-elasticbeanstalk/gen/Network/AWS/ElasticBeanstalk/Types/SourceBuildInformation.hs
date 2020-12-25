{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
  ( SourceBuildInformation (..),

    -- * Smart constructor
    mkSourceBuildInformation,

    -- * Lenses
    sbiSourceType,
    sbiSourceRepository,
    sbiSourceLocation,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.SourceLocation as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SourceRepository as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Location of the source code for an application version.
--
-- /See:/ 'mkSourceBuildInformation' smart constructor.
data SourceBuildInformation = SourceBuildInformation'
  { -- | The type of repository.
    --
    --
    --     * @Git@
    --
    --
    --     * @Zip@
    sourceType :: Types.SourceType,
    -- | Location where the repository is stored.
    --
    --
    --     * @CodeCommit@
    --
    --
    --     * @S3@
    sourceRepository :: Types.SourceRepository,
    -- | The location of the source code, as a formatted string, depending on the value of @SourceRepository@
    --
    --
    --     * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .
    --
    --
    --     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
    sourceLocation :: Types.SourceLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceBuildInformation' value with any optional fields omitted.
mkSourceBuildInformation ::
  -- | 'sourceType'
  Types.SourceType ->
  -- | 'sourceRepository'
  Types.SourceRepository ->
  -- | 'sourceLocation'
  Types.SourceLocation ->
  SourceBuildInformation
mkSourceBuildInformation sourceType sourceRepository sourceLocation =
  SourceBuildInformation'
    { sourceType,
      sourceRepository,
      sourceLocation
    }

-- | The type of repository.
--
--
--     * @Git@
--
--
--     * @Zip@
--
--
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceType :: Lens.Lens' SourceBuildInformation Types.SourceType
sbiSourceType = Lens.field @"sourceType"
{-# DEPRECATED sbiSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Location where the repository is stored.
--
--
--     * @CodeCommit@
--
--
--     * @S3@
--
--
--
-- /Note:/ Consider using 'sourceRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceRepository :: Lens.Lens' SourceBuildInformation Types.SourceRepository
sbiSourceRepository = Lens.field @"sourceRepository"
{-# DEPRECATED sbiSourceRepository "Use generic-lens or generic-optics with 'sourceRepository' instead." #-}

-- | The location of the source code, as a formatted string, depending on the value of @SourceRepository@
--
--
--     * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .
--
--
--     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
--
--
--
-- /Note:/ Consider using 'sourceLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceLocation :: Lens.Lens' SourceBuildInformation Types.SourceLocation
sbiSourceLocation = Lens.field @"sourceLocation"
{-# DEPRECATED sbiSourceLocation "Use generic-lens or generic-optics with 'sourceLocation' instead." #-}

instance Core.FromXML SourceBuildInformation where
  parseXML x =
    SourceBuildInformation'
      Core.<$> (x Core..@ "SourceType")
      Core.<*> (x Core..@ "SourceRepository")
      Core.<*> (x Core..@ "SourceLocation")
