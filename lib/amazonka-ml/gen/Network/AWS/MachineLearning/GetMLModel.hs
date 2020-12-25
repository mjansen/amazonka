{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @MLModel@ that includes detailed metadata, data source information, and the current status of the @MLModel@ .
--
-- @GetMLModel@ provides results in normal or verbose format.
module Network.AWS.MachineLearning.GetMLModel
  ( -- * Creating a request
    GetMLModel (..),
    mkGetMLModel,

    -- ** Request lenses
    gmlmMLModelId,
    gmlmVerbose,

    -- * Destructuring the response
    GetMLModelResponse (..),
    mkGetMLModelResponse,

    -- ** Response lenses
    gmlmrrsComputeTime,
    gmlmrrsCreatedAt,
    gmlmrrsCreatedByIamUser,
    gmlmrrsEndpointInfo,
    gmlmrrsFinishedAt,
    gmlmrrsInputDataLocationS3,
    gmlmrrsLastUpdatedAt,
    gmlmrrsLogUri,
    gmlmrrsMLModelId,
    gmlmrrsMLModelType,
    gmlmrrsMessage,
    gmlmrrsName,
    gmlmrrsRecipe,
    gmlmrrsSchema,
    gmlmrrsScoreThreshold,
    gmlmrrsScoreThresholdLastUpdatedAt,
    gmlmrrsSizeInBytes,
    gmlmrrsStartedAt,
    gmlmrrsStatus,
    gmlmrrsTrainingDataSourceId,
    gmlmrrsTrainingParameters,
    gmlmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMLModel' smart constructor.
data GetMLModel = GetMLModel'
  { -- | The ID assigned to the @MLModel@ at creation.
    mLModelId :: Types.MLModelId,
    -- | Specifies whether the @GetMLModel@ operation should return @Recipe@ .
    --
    -- If true, @Recipe@ is returned.
    -- If false, @Recipe@ is not returned.
    verbose :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMLModel' value with any optional fields omitted.
mkGetMLModel ::
  -- | 'mLModelId'
  Types.MLModelId ->
  GetMLModel
mkGetMLModel mLModelId =
  GetMLModel' {mLModelId, verbose = Core.Nothing}

-- | The ID assigned to the @MLModel@ at creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmMLModelId :: Lens.Lens' GetMLModel Types.MLModelId
gmlmMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED gmlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Specifies whether the @GetMLModel@ operation should return @Recipe@ .
--
-- If true, @Recipe@ is returned.
-- If false, @Recipe@ is not returned.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmVerbose :: Lens.Lens' GetMLModel (Core.Maybe Core.Bool)
gmlmVerbose = Lens.field @"verbose"
{-# DEPRECATED gmlmVerbose "Use generic-lens or generic-optics with 'verbose' instead." #-}

instance Core.FromJSON GetMLModel where
  toJSON GetMLModel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MLModelId" Core..= mLModelId),
            ("Verbose" Core..=) Core.<$> verbose
          ]
      )

instance Core.AWSRequest GetMLModel where
  type Rs GetMLModel = GetMLModelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.GetMLModel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLModelResponse'
            Core.<$> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "EndpointInfo")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "InputDataLocationS3")
            Core.<*> (x Core..:? "LastUpdatedAt")
            Core.<*> (x Core..:? "LogUri")
            Core.<*> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "MLModelType")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Recipe")
            Core.<*> (x Core..:? "Schema")
            Core.<*> (x Core..:? "ScoreThreshold")
            Core.<*> (x Core..:? "ScoreThresholdLastUpdatedAt")
            Core.<*> (x Core..:? "SizeInBytes")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "TrainingDataSourceId")
            Core.<*> (x Core..:? "TrainingParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetMLModel@ operation, and provides detailed information about a @MLModel@ .
--
-- /See:/ 'mkGetMLModelResponse' smart constructor.
data GetMLModelResponse = GetMLModelResponse'
  { -- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The time that the @MLModel@ was created. The time is expressed in epoch time.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIamUser :: Core.Maybe Types.AwsUserArn,
    -- | The current endpoint of the @MLModel@
    endpointInfo :: Core.Maybe Types.RealtimeEndpointInfo,
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Types.InputDataLocationS3,
    -- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | A link to the file that contains logs of the @CreateMLModel@ operation.
    logUri :: Core.Maybe Types.LogUri,
    -- | The MLModel ID, which is same as the @MLModelId@ in the request.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | Identifies the @MLModel@ category. The following are the available types:
    --
    --
    --     * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"
    --
    --     * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"
    --
    --     * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
    mLModelType :: Core.Maybe Types.MLModelType,
    -- | A description of the most recent details about accessing the @MLModel@ .
    message :: Core.Maybe Types.Message,
    -- | A user-supplied name or description of the @MLModel@ .
    name :: Core.Maybe Types.MLModelName,
    -- | The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
    recipe :: Core.Maybe Types.Recipe,
    -- | The schema used by all of the data files referenced by the @DataSource@ .
    schema :: Core.Maybe Types.DataSchema,
    -- | The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
    scoreThreshold :: Core.Maybe Core.Double,
    -- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
    scoreThresholdLastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    sizeInBytes :: Core.Maybe Core.Integer,
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
    startedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The current status of the @MLModel@ . This element can have one of the following values:
    --
    --
    --     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .
    --
    --     * @INPROGRESS@ - The request is processing.
    --
    --     * @FAILED@ - The request did not run to completion. The ML model isn't usable.
    --
    --     * @COMPLETED@ - The request completed successfully.
    --
    --     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
    status :: Core.Maybe Types.EntityStatus,
    -- | The ID of the training @DataSource@ .
    trainingDataSourceId :: Core.Maybe Types.TrainingDataSourceId,
    -- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
    --
    -- The following is the current set of training parameters:
    --
    --     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
    -- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
    --
    --
    --     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
    --
    --
    --     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
    --
    --
    --     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
    -- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
    --
    --
    --     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
    -- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
    trainingParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMLModelResponse' value with any optional fields omitted.
mkGetMLModelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMLModelResponse
mkGetMLModelResponse responseStatus =
  GetMLModelResponse'
    { computeTime = Core.Nothing,
      createdAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      endpointInfo = Core.Nothing,
      finishedAt = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
      mLModelId = Core.Nothing,
      mLModelType = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      recipe = Core.Nothing,
      schema = Core.Nothing,
      scoreThreshold = Core.Nothing,
      scoreThresholdLastUpdatedAt = Core.Nothing,
      sizeInBytes = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing,
      trainingDataSourceId = Core.Nothing,
      trainingParameters = Core.Nothing,
      responseStatus
    }

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsComputeTime :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Integer)
gmlmrrsComputeTime = Lens.field @"computeTime"
{-# DEPRECATED gmlmrrsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsCreatedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.NominalDiffTime)
gmlmrrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED gmlmrrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsCreatedByIamUser :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.AwsUserArn)
gmlmrrsCreatedByIamUser = Lens.field @"createdByIamUser"
{-# DEPRECATED gmlmrrsCreatedByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead." #-}

-- | The current endpoint of the @MLModel@
--
-- /Note:/ Consider using 'endpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsEndpointInfo :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.RealtimeEndpointInfo)
gmlmrrsEndpointInfo = Lens.field @"endpointInfo"
{-# DEPRECATED gmlmrrsEndpointInfo "Use generic-lens or generic-optics with 'endpointInfo' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsFinishedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.NominalDiffTime)
gmlmrrsFinishedAt = Lens.field @"finishedAt"
{-# DEPRECATED gmlmrrsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsInputDataLocationS3 :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.InputDataLocationS3)
gmlmrrsInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# DEPRECATED gmlmrrsInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsLastUpdatedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.NominalDiffTime)
gmlmrrsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED gmlmrrsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsLogUri :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.LogUri)
gmlmrrsLogUri = Lens.field @"logUri"
{-# DEPRECATED gmlmrrsLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

-- | The MLModel ID, which is same as the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsMLModelId :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.MLModelId)
gmlmrrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED gmlmrrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Identifies the @MLModel@ category. The following are the available types:
--
--
--     * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"
--
--     * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
--
--
-- /Note:/ Consider using 'mLModelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsMLModelType :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.MLModelType)
gmlmrrsMLModelType = Lens.field @"mLModelType"
{-# DEPRECATED gmlmrrsMLModelType "Use generic-lens or generic-optics with 'mLModelType' instead." #-}

-- | A description of the most recent details about accessing the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsMessage :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.Message)
gmlmrrsMessage = Lens.field @"message"
{-# DEPRECATED gmlmrrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsName :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.MLModelName)
gmlmrrsName = Lens.field @"name"
{-# DEPRECATED gmlmrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
--
-- /Note:/ Consider using 'recipe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsRecipe :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.Recipe)
gmlmrrsRecipe = Lens.field @"recipe"
{-# DEPRECATED gmlmrrsRecipe "Use generic-lens or generic-optics with 'recipe' instead." #-}

-- | The schema used by all of the data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsSchema :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.DataSchema)
gmlmrrsSchema = Lens.field @"schema"
{-# DEPRECATED gmlmrrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsScoreThreshold :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Double)
gmlmrrsScoreThreshold = Lens.field @"scoreThreshold"
{-# DEPRECATED gmlmrrsScoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead." #-}

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'scoreThresholdLastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsScoreThresholdLastUpdatedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.NominalDiffTime)
gmlmrrsScoreThresholdLastUpdatedAt = Lens.field @"scoreThresholdLastUpdatedAt"
{-# DEPRECATED gmlmrrsScoreThresholdLastUpdatedAt "Use generic-lens or generic-optics with 'scoreThresholdLastUpdatedAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsSizeInBytes :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Integer)
gmlmrrsSizeInBytes = Lens.field @"sizeInBytes"
{-# DEPRECATED gmlmrrsSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsStartedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.NominalDiffTime)
gmlmrrsStartedAt = Lens.field @"startedAt"
{-# DEPRECATED gmlmrrsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The current status of the @MLModel@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .
--
--     * @INPROGRESS@ - The request is processing.
--
--     * @FAILED@ - The request did not run to completion. The ML model isn't usable.
--
--     * @COMPLETED@ - The request completed successfully.
--
--     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsStatus :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.EntityStatus)
gmlmrrsStatus = Lens.field @"status"
{-# DEPRECATED gmlmrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the training @DataSource@ .
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsTrainingDataSourceId :: Lens.Lens' GetMLModelResponse (Core.Maybe Types.TrainingDataSourceId)
gmlmrrsTrainingDataSourceId = Lens.field @"trainingDataSourceId"
{-# DEPRECATED gmlmrrsTrainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead." #-}

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
--     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
-- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
--
--
--     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
--
--
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
--
-- /Note:/ Consider using 'trainingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsTrainingParameters :: Lens.Lens' GetMLModelResponse (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
gmlmrrsTrainingParameters = Lens.field @"trainingParameters"
{-# DEPRECATED gmlmrrsTrainingParameters "Use generic-lens or generic-optics with 'trainingParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrrsResponseStatus :: Lens.Lens' GetMLModelResponse Core.Int
gmlmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmlmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
