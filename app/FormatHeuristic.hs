-- |
-- Originally based on:
--   https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/App/FormatHeuristics.hs
module FormatHeuristic
  ( formatFromFilePath
  )
where

import Data.Char (toLower)
import System.FilePath (takeExtension)
import Text.Pandoc

formatFromFilePath :: PandocMonad m => FilePath -> Maybe (Writer m)
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc" -> f "asciidoc"
    ".asciidoc" -> f "asciidoc"
    ".context" -> f "context"
    ".ctx" -> f "context"
    ".db" -> f "docbook"
    ".doc" -> f "doc" -- so we get an "unknown reader" error
    ".docx" -> f "docx"
    ".dokuwiki" -> f "dokuwiki"
    ".epub" -> f "epub"
    ".fb2" -> f "fb2"
    ".htm" -> f "html"
    ".html" -> f "html"
    ".icml" -> f "icml"
    ".json" -> f "json"
    ".latex" -> f "latex"
    ".lhs" -> f "markdown+lhs"
    ".ltx" -> f "latex"
    ".markdown" -> f "markdown"
    ".mkdn" -> f "markdown"
    ".mkd" -> f "markdown"
    ".mdwn" -> f "markdown"
    ".mdown" -> f "markdown"
    ".Rmd" -> f "markdown"
    ".md" -> f "markdown"
    ".ms" -> f "ms"
    ".muse" -> f "muse"
    ".native" -> f "native"
    ".odt" -> f "odt"
    ".opml" -> f "opml"
    ".org" -> f "org"
    ".pdf" -> f "pdf" -- so we get an "unknown reader" error
    ".pptx" -> f "pptx"
    ".roff" -> f "ms"
    ".rst" -> f "rst"
    ".rtf" -> f "rtf"
    ".s5" -> f "s5"
    ".t2t" -> f "t2t"
    ".tei" -> f "tei"
    ".tei.xml" -> f "tei"
    ".tex" -> f "latex"
    ".texi" -> f "texinfo"
    ".texinfo" -> f "texinfo"
    ".text" -> f "markdown"
    ".textile" -> f "textile"
    ".txt" -> f "markdown"
    ".wiki" -> f "mediawiki"
    ".xhtml" -> f "html"
    ".ipynb" -> f "ipynb"
    ".csv" -> f "csv"
    ".bib" -> f "biblatex"
    ['.', y] | y `elem` ['1' .. '9'] -> f "man"
    _ -> Nothing
  where
    f k = lookup k writers
