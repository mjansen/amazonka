{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionMetadata
  ( AutomationExecutionMetadata (..),

    -- * Smart constructor
    mkAutomationExecutionMetadata,

    -- * Lenses
    aemAutomationExecutionId,
    aemAutomationExecutionStatus,
    aemAutomationType,
    aemCurrentAction,
    aemCurrentStepName,
    aemDocumentName,
    aemDocumentVersion,
    aemExecutedBy,
    aemExecutionEndTime,
    aemExecutionStartTime,
    aemFailureMessage,
    aemLogFile,
    aemMaxConcurrency,
    aemMaxErrors,
    aemMode,
    aemOutputs,
    aemParentAutomationExecutionId,
    aemResolvedTargets,
    aemTarget,
    aemTargetMaps,
    aemTargetParameterName,
    aemTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AutomationExecutionId as Types
import qualified Network.AWS.SSM.Types.AutomationExecutionStatus as Types
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types
import qualified Network.AWS.SSM.Types.AutomationType as Types
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.ExecutionMode as Types
import qualified Network.AWS.SSM.Types.MaxConcurrency as Types
import qualified Network.AWS.SSM.Types.MaxErrors as Types
import qualified Network.AWS.SSM.Types.ParentAutomationExecutionId as Types
import qualified Network.AWS.SSM.Types.ResolvedTargets as Types
import qualified Network.AWS.SSM.Types.String as Types
import qualified Network.AWS.SSM.Types.Target as Types
import qualified Network.AWS.SSM.Types.TargetMapKey as Types
import qualified Network.AWS.SSM.Types.TargetMapValue as Types
import qualified Network.AWS.SSM.Types.TargetParameterName as Types

-- | Details about a specific Automation execution.
--
-- /See:/ 'mkAutomationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { -- | The execution ID.
    automationExecutionId :: Core.Maybe Types.AutomationExecutionId,
    -- | The status of the execution.
    automationExecutionStatus :: Core.Maybe Types.AutomationExecutionStatus,
    -- | Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
    automationType :: Core.Maybe Types.AutomationType,
    -- | The action of the step that is currently running.
    currentAction :: Core.Maybe Types.String,
    -- | The name of the step that is currently running.
    currentStepName :: Core.Maybe Types.String,
    -- | The name of the Automation document used during execution.
    documentName :: Core.Maybe Types.DocumentName,
    -- | The document version used during the execution.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The IAM role ARN of the user who ran the Automation.
    executedBy :: Core.Maybe Types.String,
    -- | The time the execution finished. This is not populated if the execution is still in progress.
    executionEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time the execution started.
    executionStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The list of execution outputs as defined in the Automation document.
    failureMessage :: Core.Maybe Types.String,
    -- | An S3 bucket where execution information is stored.
    logFile :: Core.Maybe Types.String,
    -- | The MaxConcurrency value specified by the user when starting the Automation.
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The MaxErrors value specified by the user when starting the Automation.
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | The Automation execution mode.
    mode :: Core.Maybe Types.ExecutionMode,
    -- | The list of execution outputs as defined in the Automation document.
    outputs :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]),
    -- | The ExecutionId of the parent Automation.
    parentAutomationExecutionId :: Core.Maybe Types.ParentAutomationExecutionId,
    -- | A list of targets that resolved during the execution.
    resolvedTargets :: Core.Maybe Types.ResolvedTargets,
    -- | The list of execution outputs as defined in the Automation document.
    target :: Core.Maybe Types.String,
    -- | The specified key-value mapping of document parameters to target resources.
    targetMaps :: Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]],
    -- | The list of execution outputs as defined in the Automation document.
    targetParameterName :: Core.Maybe Types.TargetParameterName,
    -- | The targets defined by the user when starting the Automation.
    targets :: Core.Maybe [Types.Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AutomationExecutionMetadata' value with any optional fields omitted.
mkAutomationExecutionMetadata ::
  AutomationExecutionMetadata
mkAutomationExecutionMetadata =
  AutomationExecutionMetadata'
    { automationExecutionId =
        Core.Nothing,
      automationExecutionStatus = Core.Nothing,
      automationType = Core.Nothing,
      currentAction = Core.Nothing,
      currentStepName = Core.Nothing,
      documentName = Core.Nothing,
      documentVersion = Core.Nothing,
      executedBy = Core.Nothing,
      executionEndTime = Core.Nothing,
      executionStartTime = Core.Nothing,
      failureMessage = Core.Nothing,
      logFile = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      mode = Core.Nothing,
      outputs = Core.Nothing,
      parentAutomationExecutionId = Core.Nothing,
      resolvedTargets = Core.Nothing,
      target = Core.Nothing,
      targetMaps = Core.Nothing,
      targetParameterName = Core.Nothing,
      targets = Core.Nothing
    }

-- | The execution ID.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.AutomationExecutionId)
aemAutomationExecutionId = Lens.field @"automationExecutionId"
{-# DEPRECATED aemAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | The status of the execution.
--
-- /Note:/ Consider using 'automationExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationExecutionStatus :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.AutomationExecutionStatus)
aemAutomationExecutionStatus = Lens.field @"automationExecutionStatus"
{-# DEPRECATED aemAutomationExecutionStatus "Use generic-lens or generic-optics with 'automationExecutionStatus' instead." #-}

-- | Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'automationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationType :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.AutomationType)
aemAutomationType = Lens.field @"automationType"
{-# DEPRECATED aemAutomationType "Use generic-lens or generic-optics with 'automationType' instead." #-}

-- | The action of the step that is currently running.
--
-- /Note:/ Consider using 'currentAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemCurrentAction :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemCurrentAction = Lens.field @"currentAction"
{-# DEPRECATED aemCurrentAction "Use generic-lens or generic-optics with 'currentAction' instead." #-}

-- | The name of the step that is currently running.
--
-- /Note:/ Consider using 'currentStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemCurrentStepName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemCurrentStepName = Lens.field @"currentStepName"
{-# DEPRECATED aemCurrentStepName "Use generic-lens or generic-optics with 'currentStepName' instead." #-}

-- | The name of the Automation document used during execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemDocumentName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.DocumentName)
aemDocumentName = Lens.field @"documentName"
{-# DEPRECATED aemDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The document version used during the execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemDocumentVersion :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.DocumentVersion)
aemDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED aemDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The IAM role ARN of the user who ran the Automation.
--
-- /Note:/ Consider using 'executedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutedBy :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemExecutedBy = Lens.field @"executedBy"
{-# DEPRECATED aemExecutedBy "Use generic-lens or generic-optics with 'executedBy' instead." #-}

-- | The time the execution finished. This is not populated if the execution is still in progress.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutionEndTime :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.NominalDiffTime)
aemExecutionEndTime = Lens.field @"executionEndTime"
{-# DEPRECATED aemExecutionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutionStartTime :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.NominalDiffTime)
aemExecutionStartTime = Lens.field @"executionStartTime"
{-# DEPRECATED aemExecutionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemFailureMessage :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED aemFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | An S3 bucket where execution information is stored.
--
-- /Note:/ Consider using 'logFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemLogFile :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemLogFile = Lens.field @"logFile"
{-# DEPRECATED aemLogFile "Use generic-lens or generic-optics with 'logFile' instead." #-}

-- | The MaxConcurrency value specified by the user when starting the Automation.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMaxConcurrency :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.MaxConcurrency)
aemMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED aemMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The MaxErrors value specified by the user when starting the Automation.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMaxErrors :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.MaxErrors)
aemMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED aemMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The Automation execution mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMode :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.ExecutionMode)
aemMode = Lens.field @"mode"
{-# DEPRECATED aemMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemOutputs :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
aemOutputs = Lens.field @"outputs"
{-# DEPRECATED aemOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The ExecutionId of the parent Automation.
--
-- /Note:/ Consider using 'parentAutomationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemParentAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.ParentAutomationExecutionId)
aemParentAutomationExecutionId = Lens.field @"parentAutomationExecutionId"
{-# DEPRECATED aemParentAutomationExecutionId "Use generic-lens or generic-optics with 'parentAutomationExecutionId' instead." #-}

-- | A list of targets that resolved during the execution.
--
-- /Note:/ Consider using 'resolvedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemResolvedTargets :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.ResolvedTargets)
aemResolvedTargets = Lens.field @"resolvedTargets"
{-# DEPRECATED aemResolvedTargets "Use generic-lens or generic-optics with 'resolvedTargets' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTarget :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.String)
aemTarget = Lens.field @"target"
{-# DEPRECATED aemTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The specified key-value mapping of document parameters to target resources.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargetMaps :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]])
aemTargetMaps = Lens.field @"targetMaps"
{-# DEPRECATED aemTargetMaps "Use generic-lens or generic-optics with 'targetMaps' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargetParameterName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Types.TargetParameterName)
aemTargetParameterName = Lens.field @"targetParameterName"
{-# DEPRECATED aemTargetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead." #-}

-- | The targets defined by the user when starting the Automation.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargets :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe [Types.Target])
aemTargets = Lens.field @"targets"
{-# DEPRECATED aemTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.FromJSON AutomationExecutionMetadata where
  parseJSON =
    Core.withObject "AutomationExecutionMetadata" Core.$
      \x ->
        AutomationExecutionMetadata'
          Core.<$> (x Core..:? "AutomationExecutionId")
          Core.<*> (x Core..:? "AutomationExecutionStatus")
          Core.<*> (x Core..:? "AutomationType")
          Core.<*> (x Core..:? "CurrentAction")
          Core.<*> (x Core..:? "CurrentStepName")
          Core.<*> (x Core..:? "DocumentName")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "ExecutedBy")
          Core.<*> (x Core..:? "ExecutionEndTime")
          Core.<*> (x Core..:? "ExecutionStartTime")
          Core.<*> (x Core..:? "FailureMessage")
          Core.<*> (x Core..:? "LogFile")
          Core.<*> (x Core..:? "MaxConcurrency")
          Core.<*> (x Core..:? "MaxErrors")
          Core.<*> (x Core..:? "Mode")
          Core.<*> (x Core..:? "Outputs")
          Core.<*> (x Core..:? "ParentAutomationExecutionId")
          Core.<*> (x Core..:? "ResolvedTargets")
          Core.<*> (x Core..:? "Target")
          Core.<*> (x Core..:? "TargetMaps")
          Core.<*> (x Core..:? "TargetParameterName")
          Core.<*> (x Core..:? "Targets")
