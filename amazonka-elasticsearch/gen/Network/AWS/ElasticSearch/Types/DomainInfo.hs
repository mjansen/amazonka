{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DomainInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'domainInfo' smart constructor.
newtype DomainInfo = DomainInfo'{_diDomainName ::
                                 Maybe Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDomainName' - Specifies the @DomainName@ .
domainInfo :: DomainInfo
domainInfo = DomainInfo' { _diDomainName = Nothing }

-- | Specifies the @DomainName@ .
diDomainName :: Lens' DomainInfo (Maybe Text)
diDomainName = lens _diDomainName (\s a -> s { _diDomainName = a })

instance FromJSON DomainInfo where
  parseJSON =
    withObject "DomainInfo" (\x -> DomainInfo' <$> (x .:? "DomainName"))

instance Hashable DomainInfo where

instance NFData DomainInfo where
