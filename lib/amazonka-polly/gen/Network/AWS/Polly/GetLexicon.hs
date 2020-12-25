{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.GetLexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
module Network.AWS.Polly.GetLexicon
  ( -- * Creating a request
    GetLexicon (..),
    mkGetLexicon,

    -- ** Request lenses
    glName,

    -- * Destructuring the response
    GetLexiconResponse (..),
    mkGetLexiconResponse,

    -- ** Response lenses
    glrrsLexicon,
    glrrsLexiconAttributes,
    glrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLexicon' smart constructor.
newtype GetLexicon = GetLexicon'
  { -- | Name of the lexicon.
    name :: Types.LexiconName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLexicon' value with any optional fields omitted.
mkGetLexicon ::
  -- | 'name'
  Types.LexiconName ->
  GetLexicon
mkGetLexicon name = GetLexicon' {name}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glName :: Lens.Lens' GetLexicon Types.LexiconName
glName = Lens.field @"name"
{-# DEPRECATED glName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest GetLexicon where
  type Rs GetLexicon = GetLexiconResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/v1/lexicons/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLexiconResponse'
            Core.<$> (x Core..:? "Lexicon")
            Core.<*> (x Core..:? "LexiconAttributes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLexiconResponse' smart constructor.
data GetLexiconResponse = GetLexiconResponse'
  { -- | Lexicon object that provides name and the string content of the lexicon.
    lexicon :: Core.Maybe Types.Lexicon,
    -- | Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
    lexiconAttributes :: Core.Maybe Types.LexiconAttributes,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLexiconResponse' value with any optional fields omitted.
mkGetLexiconResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLexiconResponse
mkGetLexiconResponse responseStatus =
  GetLexiconResponse'
    { lexicon = Core.Nothing,
      lexiconAttributes = Core.Nothing,
      responseStatus
    }

-- | Lexicon object that provides name and the string content of the lexicon.
--
-- /Note:/ Consider using 'lexicon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrsLexicon :: Lens.Lens' GetLexiconResponse (Core.Maybe Types.Lexicon)
glrrsLexicon = Lens.field @"lexicon"
{-# DEPRECATED glrrsLexicon "Use generic-lens or generic-optics with 'lexicon' instead." #-}

-- | Metadata of the lexicon, including phonetic alphabetic used, language code, lexicon ARN, number of lexemes defined in the lexicon, and size of lexicon in bytes.
--
-- /Note:/ Consider using 'lexiconAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrsLexiconAttributes :: Lens.Lens' GetLexiconResponse (Core.Maybe Types.LexiconAttributes)
glrrsLexiconAttributes = Lens.field @"lexiconAttributes"
{-# DEPRECATED glrrsLexiconAttributes "Use generic-lens or generic-optics with 'lexiconAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrsResponseStatus :: Lens.Lens' GetLexiconResponse Core.Int
glrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
