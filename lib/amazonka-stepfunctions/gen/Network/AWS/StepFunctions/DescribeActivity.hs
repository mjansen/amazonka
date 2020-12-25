{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an activity.
module Network.AWS.StepFunctions.DescribeActivity
  ( -- * Creating a request
    DescribeActivity (..),
    mkDescribeActivity,

    -- ** Request lenses
    dActivityArn,

    -- * Destructuring the response
    DescribeActivityResponse (..),
    mkDescribeActivityResponse,

    -- ** Response lenses
    drsActivityArn,
    drsName,
    drsCreationDate,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDescribeActivity' smart constructor.
newtype DescribeActivity = DescribeActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to describe.
    activityArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeActivity' value with any optional fields omitted.
mkDescribeActivity ::
  -- | 'activityArn'
  Types.Arn ->
  DescribeActivity
mkDescribeActivity activityArn = DescribeActivity' {activityArn}

-- | The Amazon Resource Name (ARN) of the activity to describe.
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActivityArn :: Lens.Lens' DescribeActivity Types.Arn
dActivityArn = Lens.field @"activityArn"
{-# DEPRECATED dActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

instance Core.FromJSON DescribeActivity where
  toJSON DescribeActivity {..} =
    Core.object
      (Core.catMaybes [Core.Just ("activityArn" Core..= activityArn)])

instance Core.AWSRequest DescribeActivity where
  type Rs DescribeActivity = DescribeActivityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.DescribeActivity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivityResponse'
            Core.<$> (x Core..: "activityArn")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "creationDate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeActivityResponse' smart constructor.
data DescribeActivityResponse = DescribeActivityResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the activity.
    activityArn :: Types.Arn,
    -- | The name of the activity.
    --
    -- A name must /not/ contain:
    --
    --     * white space
    --
    --
    --     * brackets @< > { } [ ]@
    --
    --
    --     * wildcard characters @? *@
    --
    --
    --     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
    --
    --
    --     * control characters (@U+0000-001F@ , @U+007F-009F@ )
    --
    --
    -- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
    name :: Types.Name,
    -- | The date the activity is created.
    creationDate :: Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeActivityResponse' value with any optional fields omitted.
mkDescribeActivityResponse ::
  -- | 'activityArn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeActivityResponse
mkDescribeActivityResponse
  activityArn
  name
  creationDate
  responseStatus =
    DescribeActivityResponse'
      { activityArn,
        name,
        creationDate,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) that identifies the activity.
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsActivityArn :: Lens.Lens' DescribeActivityResponse Types.Arn
drsActivityArn = Lens.field @"activityArn"
{-# DEPRECATED drsActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

-- | The name of the activity.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeActivityResponse Types.Name
drsName = Lens.field @"name"
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationDate :: Lens.Lens' DescribeActivityResponse Core.NominalDiffTime
drsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED drsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeActivityResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
