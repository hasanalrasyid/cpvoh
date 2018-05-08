module CPVO.IO (
    showDouble
    , markdownToTex
  ) where

import Text.Printf as TP
import Text.Pandoc
import qualified Data.Text as T
import Control.Monad ((<=<))

showDouble _ 0 = show 0
showDouble i x = (flip TP.printf) x $ concat ["%0.",show i,"f"]


-- Prosesor untuk pandoc
-- LaTeX writer...
--
markdownToTex str = runIOorExplode
              $ ((writeLaTeX def) <=< (readMarkdown def{
                  readerExtensions = foldr enableExtension pandocExtensions [Ext_tex_math_dollars, Ext_raw_tex, Ext_table_captions]
                                                       }))
              $ T.pack $ str
