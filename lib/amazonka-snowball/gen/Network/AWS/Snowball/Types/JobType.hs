{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobType
  ( JobType
      ( JobType',
        JobTypeImport,
        JobTypeExport,
        JobTypeLocalUse,
        fromJobType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype JobType = JobType' {fromJobType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern JobTypeImport :: JobType
pattern JobTypeImport = JobType' "IMPORT"

pattern JobTypeExport :: JobType
pattern JobTypeExport = JobType' "EXPORT"

pattern JobTypeLocalUse :: JobType
pattern JobTypeLocalUse = JobType' "LOCAL_USE"

{-# COMPLETE
  JobTypeImport,
  JobTypeExport,
  JobTypeLocalUse,
  JobType'
  #-}
