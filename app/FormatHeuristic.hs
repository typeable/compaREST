-- |
-- Originally based on:
--   https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/App/FormatHeuristics.hs
module FormatHeuristic
  ( formatFromFilePath,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Functor
import qualified Data.Map as M
import qualified Data.OpenApi.Compare.Report.Html.Template as Html
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import System.FilePath (takeExtension)
import Text.DocTemplates.Internal
import Text.Pandoc

formatFromFilePath :: forall m. PandocMonad m => FilePath -> Maybe (Pandoc -> m ByteString, FilePath)
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc" -> fWithDefaultFile "asciidoc" def
    ".asciidoc" -> fWithDefaultFile "asciidoc" def
    ".context" -> fWithDefaultFile "context" def
    ".ctx" -> fWithDefaultFile "context" def
    ".db" -> fWithDefaultFile "docbook" def
    ".doc" -> fWithDefaultFile "doc" def -- so we get an "unknown reader" error
    ".docx" -> fWithDefaultFile "docx" def
    ".dokuwiki" -> fWithDefaultFile "dokuwiki" def
    ".epub" -> fWithDefaultFile "epub" def
    ".fb2" -> fWithDefaultFile "fb2" def
    ".htm" -> fWithDefaultFile "html" def
    ".html" -> fWithDefaultFile "html" def
    ".icml" -> fWithDefaultFile "icml" def
    ".json" -> fWithDefaultFile "json" def
    ".latex" -> fWithDefaultFile "latex" def
    ".lhs" -> fWithDefaultFile "markdown+lhs" def
    ".ltx" -> fWithDefaultFile "latex" def
    ".markdown" -> markdown
    ".mkdn" -> markdown
    ".mkd" -> markdown
    ".mdwn" -> markdown
    ".mdown" -> markdown
    ".Rmd" -> markdown
    ".md" -> markdown
    ".ms" -> fWithDefaultFile "ms" def
    ".muse" -> fWithDefaultFile "muse" def
    ".native" -> fWithDefaultFile "native" def
    ".odt" -> fWithDefaultFile "odt" def
    ".opml" -> fWithDefaultFile "opml" def
    ".org" -> fWithDefaultFile "org" def
    -- so we get an "unknown reader" error
    ".pdf" -> fWithDefaultFile "pdf" def
    ".pptx" -> fWithDefaultFile "pptx" def
    ".roff" -> fWithDefaultFile "ms" def
    ".rst" -> fWithDefaultFile "rst" def
    ".rtf" -> fWithDefaultFile "rtf" def
    ".s5" -> fWithDefaultFile "s5" def
    ".t2t" -> fWithDefaultFile "t2t" def
    ".tei" -> fWithDefaultFile "tei" def
    ".tei.xml" -> fWithDefaultFile "tei" def
    ".tex" -> fWithDefaultFile "latex" def
    ".texi" -> fWithDefaultFile "texinfo" def
    ".texinfo" -> fWithDefaultFile "texinfo" def
    ".text" -> markdown
    ".textile" -> fWithDefaultFile "textile" def
    ".txt" -> markdown
    ".wiki" -> fWithDefaultFile "mediawiki" def
    ".xhtml" -> fWithDefaultFile "html" def
    ".ipynb" -> fWithDefaultFile "ipynb" def
    ".csv" -> fWithDefaultFile "csv" def
    ".bib" -> fWithDefaultFile "biblatex" def
    ['.', y] | y `elem` ['1' .. '9'] -> fWithDefaultFile "man" def
    "" ->
      (,x <> ".html")
        <$> f
          "html"
          def
            { writerTemplate = Just Html.template
            , -- Not actually used. Needed to silence warning.
              writerVariables = Context $ M.fromList [("pagetitle", toVal ("CompaREST" :: Text))]
            }
    _ -> Nothing
  where
    markdown :: Maybe (Pandoc -> m ByteString, FilePath)
    markdown = fWithDefaultFile "markdown" markdownOpt

    markdownOpt = def {writerExtensions = githubMarkdownExtensions}
    fWithDefaultFile k opt = (,x) <$> f k opt
    f k opt =
      lookup k writers <&> \case
        TextWriter g -> fmap (TL.encodeUtf8 . TL.fromStrict) . g opt
        ByteStringWriter g -> g opt
