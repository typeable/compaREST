module OpenAPI.Checker.Report.Html.Template
  ( template
  )
where

import Control.Monad.Identity
import Data.Text (Text)
import Text.DocTemplates

template :: Template Text
template =
  either error id . runIdentity . compileTemplate "" $
    "<!doctype html>\
    \<html lang=\"en\">\
    \<head>\
    \<meta charset=\"utf-8\">\
    \<title></title>\
    \<meta name=\"description\" content=\"\">\
    \<meta name=\"generator\" content=\"OpenApi Diff\" />\
    \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\" />\
    \</head>\
    \<body>\
    \$body$\
    \</body>\
    \</html>"
