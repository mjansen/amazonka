{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable =
  Wait
    { _waitName = "DBInstanceAvailable",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleted"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-restore"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-parameters"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
dbSnapshotCompleted :: Wait DescribeDBSnapshots
dbSnapshotCompleted =
  Wait
    { _waitName = "DBSnapshotCompleted",
      _waitAttempts = 40,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchError "DBSnapshotNotFound" AcceptSuccess,
          matchAll
            "available"
            AcceptSuccess
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbSnapshotDeleted :: Wait DescribeDBSnapshots
dbSnapshotDeleted =
  Wait
    { _waitName = "DBSnapshotDeleted",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll True AcceptSuccess (length (^. ddsrsDBSnapshots) == 0),
          matchError "DBSnapshotNotFound" AcceptSuccess,
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "rebooting"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "resetting-master-credentials"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted =
  Wait
    { _waitName = "DBInstanceDeleted",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll True AcceptSuccess (length (^. ddbirsDBInstances) == 0),
          matchError "DBInstanceNotFound" AcceptSuccess,
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "rebooting"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "resetting-master-credentials"
            AcceptFailure
            ( folding (concatOf (ddbirsDBInstances . to toList))
                . diDBInstanceStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbClusterSnapshotDeleted :: Wait DescribeDBClusterSnapshots
dbClusterSnapshotDeleted =
  Wait
    { _waitName = "DBClusterSnapshotDeleted",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll
            True
            AcceptSuccess
            (length (^. ddbcsrsDBClusterSnapshots) == 0),
          matchError "DBClusterSnapshotNotFoundFault" AcceptSuccess,
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "rebooting"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "resetting-master-credentials"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbSnapshotAvailable :: Wait DescribeDBSnapshots
dbSnapshotAvailable =
  Wait
    { _waitName = "DBSnapshotAvailable",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleted"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-restore"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-parameters"
            AcceptFailure
            ( folding (concatOf (ddsrsDBSnapshots . to toList))
                . dsStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dbClusterSnapshotAvailable :: Wait DescribeDBClusterSnapshots
dbClusterSnapshotAvailable =
  Wait
    { _waitName = "DBClusterSnapshotAvailable",
      _waitAttempts = 60,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleted"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-restore"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-parameters"
            AcceptFailure
            ( folding (concatOf (ddbcsrsDBClusterSnapshots . to toList))
                . dcsStatus
                . _Just
                . to toTextCI
            )
        ]
    }
