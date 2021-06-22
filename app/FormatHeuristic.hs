-- |
-- Originally based on:
--   https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/App/FormatHeuristics.hs
module FormatHeuristic
  ( formatFromFilePath
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified OpenAPI.Checker.Report.Html.Template as Html
import System.FilePath (takeExtension)
import Text.DocTemplates.Internal
import Text.Pandoc

formatFromFilePath :: forall m. PandocMonad m => FilePath -> Maybe (Pandoc -> m ByteString)
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc" -> f "asciidoc" def
    ".asciidoc" -> f "asciidoc" def
    ".context" -> f "context" def
    ".ctx" -> f "context" def
    ".db" -> f "docbook" def
    ".doc" -> f "doc" def -- so we get an "unknown reader" error
    ".docx" -> f "docx" def
    ".dokuwiki" -> f "dokuwiki" def
    ".epub" -> f "epub" def
    ".fb2" -> f "fb2" def
    ".htm" -> html
    ".html" -> html
    ".icml" -> f "icml" def
    ".json" -> f "json" def
    ".latex" -> f "latex" def
    ".lhs" -> f "markdown+lhs" def
    ".ltx" -> f "latex" def
    ".markdown" -> markdown
    ".mkdn" -> markdown
    ".mkd" -> markdown
    ".mdwn" -> markdown
    ".mdown" -> markdown
    ".Rmd" -> markdown
    ".md" -> markdown
    ".ms" -> f "ms" def
    ".muse" -> f "muse" def
    ".native" -> f "native" def
    ".odt" -> f "odt" def
    ".opml" -> f "opml" def
    ".org" -> f "org" def
    -- so we get an "unknown reader" error
    ".pdf" -> f "pdf" def
    ".pptx" -> f "pptx" def
    ".roff" -> f "ms" def
    ".rst" -> f "rst" def
    ".rtf" -> f "rtf" def
    ".s5" -> f "s5" def
    ".t2t" -> f "t2t" def
    ".tei" -> f "tei" def
    ".tei.xml" -> f "tei" def
    ".tex" -> f "latex" def
    ".texi" -> f "texinfo" def
    ".texinfo" -> f "texinfo" def
    ".text" -> markdown
    ".textile" -> f "textile" def
    ".txt" -> markdown
    ".wiki" -> f "mediawiki" def
    ".xhtml" -> f "html" def
    ".ipynb" -> f "ipynb" def
    ".csv" -> f "csv" def
    ".bib" -> f "biblatex" def
    ['.', y] | y `elem` ['1' .. '9'] -> f "man" def
    _ -> Nothing
  where
    markdown, html :: Maybe (Pandoc -> m ByteString)
    markdown = f "markdown" markdownOpt
    html =
      f
        "html"
        def
          { writerTemplate = Just Html.template
          , -- Not actually used. Needed to silence warning.
            writerVariables = Context $ M.fromList [("pagetitle", toVal ("OpenApi Diff" :: Text))]
          }
    markdownOpt = def {writerExtensions = githubMarkdownExtensions}
    f k opt =
      lookup k writers <&> \case
        TextWriter g -> fmap (TL.encodeUtf8 . TL.fromStrict) . g opt
        ByteStringWriter g -> g opt
