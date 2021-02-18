{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Tag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a key value pair for a resource tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'{_tagKey :: !Text, _tagValue :: !Text}
             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Specifies the @TagKey@ , the name of the tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.
--
-- * 'tagValue' - Specifies the @TagValue@ , the value assigned to the corresponding tag key. Tag values can be null and do not have to be unique in a tag set. For example, you can have a key value pair in a tag set of @project : Trinity@ and @cost-center : Trinity@
tag
  :: Text -- ^ 'tagKey'
  -> Text -- ^ 'tagValue'
  -> Tag
tag pKey_ pValue_ = Tag' { _tagKey = pKey_, _tagValue = pValue_ }

-- | Specifies the @TagKey@ , the name of the tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | Specifies the @TagValue@ , the value assigned to the corresponding tag key. Tag values can be null and do not have to be unique in a tag set. For example, you can have a key value pair in a tag set of @project : Trinity@ and @cost-center : Trinity@
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromJSON Tag where
  parseJSON = withObject "Tag" (\x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
  toJSON Tag' {..} =
    object (catMaybes [Just ("Key" .= _tagKey), Just ("Value" .= _tagValue)])
