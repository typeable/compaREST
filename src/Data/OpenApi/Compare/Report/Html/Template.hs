{-# LANGUAGE TemplateHaskell #-}

module Data.OpenApi.Compare.Report.Html.Template
  ( template,
  )
where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Text.DocTemplates

template :: Template Text
template =
  either error id . runIdentity . compileTemplate "" $
    "<!doctype html>\
    \<html lang=\"en\">\
    \<head>\
    \<style>"
      <> T.decodeUtf8 awsmCss
      <> "</style>\
         \<meta charset=\"utf-8\">\
         \<title></title>\
         \<meta name=\"description\" content=\"\">\
         \<meta name=\"generator\" content=\"CompaREST\" />\
         \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\" />\
         \</head>\
         \<body>\
         \<header><h1>CompaREST</h1></header>\
         \<main>\
         \$body$\
         \</main>\
         \</body>\
         \</html>"

awsmCss :: ByteString
awsmCss = $(makeRelativeToProject "awsm-css/dist/awsm.min.css" >>= embedFile)
