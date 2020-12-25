{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderContentType
  ( FolderContentType
      ( FolderContentType',
        FolderContentTypeAll,
        FolderContentTypeDocument,
        FolderContentTypeFolder,
        fromFolderContentType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FolderContentType = FolderContentType'
  { fromFolderContentType ::
      Core.Text
  }
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

pattern FolderContentTypeAll :: FolderContentType
pattern FolderContentTypeAll = FolderContentType' "ALL"

pattern FolderContentTypeDocument :: FolderContentType
pattern FolderContentTypeDocument = FolderContentType' "DOCUMENT"

pattern FolderContentTypeFolder :: FolderContentType
pattern FolderContentTypeFolder = FolderContentType' "FOLDER"

{-# COMPLETE
  FolderContentTypeAll,
  FolderContentTypeDocument,
  FolderContentTypeFolder,
  FolderContentType'
  #-}
