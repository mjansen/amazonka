{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutput
  ( ProcessingOutput (..),

    -- * Smart constructor
    mkProcessingOutput,

    -- * Lenses
    poOutputName,
    poS3Output,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.OutputName as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3Output as Types

-- | Describes the results of a processing job.
--
-- /See:/ 'mkProcessingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { -- | The name for the processing job output.
    outputName :: Types.OutputName,
    -- | Configuration for processing job outputs in Amazon S3.
    s3Output :: Types.ProcessingS3Output
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingOutput' value with any optional fields omitted.
mkProcessingOutput ::
  -- | 'outputName'
  Types.OutputName ->
  -- | 's3Output'
  Types.ProcessingS3Output ->
  ProcessingOutput
mkProcessingOutput outputName s3Output =
  ProcessingOutput' {outputName, s3Output}

-- | The name for the processing job output.
--
-- /Note:/ Consider using 'outputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOutputName :: Lens.Lens' ProcessingOutput Types.OutputName
poOutputName = Lens.field @"outputName"
{-# DEPRECATED poOutputName "Use generic-lens or generic-optics with 'outputName' instead." #-}

-- | Configuration for processing job outputs in Amazon S3.
--
-- /Note:/ Consider using 's3Output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poS3Output :: Lens.Lens' ProcessingOutput Types.ProcessingS3Output
poS3Output = Lens.field @"s3Output"
{-# DEPRECATED poS3Output "Use generic-lens or generic-optics with 's3Output' instead." #-}

instance Core.FromJSON ProcessingOutput where
  toJSON ProcessingOutput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OutputName" Core..= outputName),
            Core.Just ("S3Output" Core..= s3Output)
          ]
      )

instance Core.FromJSON ProcessingOutput where
  parseJSON =
    Core.withObject "ProcessingOutput" Core.$
      \x ->
        ProcessingOutput'
          Core.<$> (x Core..: "OutputName") Core.<*> (x Core..: "S3Output")
