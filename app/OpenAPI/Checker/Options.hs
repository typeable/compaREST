module OpenAPI.Checker.Options
  ( Options (..)
  , Mode (..)
  , OutputMode (..)
  , optionsParserInfo
  , execParser
  )
where

import GHC.Generics (Generic)
import OpenAPI.Checker.Report
import Options.Applicative

data Options = Options
  { clientFile :: FilePath
  , serverFile :: FilePath
  , mode :: Mode
  , outputMode :: OutputMode
  , reportConfig :: ReportConfig
  }
  deriving stock (Generic)

data Mode = Silent | OnlyErrors | All

data OutputMode = StdoutMode | FileMode FilePath

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (helper <*> optionsParser)
    (fullDesc
       <> header "openapi-diff"
       <> progDesc "A tool to check compatibility between two OpenApi specifications.")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      (short 'c'
         <> long "client"
         <> help "The specification that will be used for the client of the API.")
    <*> strOption
      (short 's'
         <> long "server"
         <> help "The specification that will be used for the server of the API.")
    <*> (flag'
           Silent
           (long "silent"
              <> help "Silence all output.")
           <|> flag'
             OnlyErrors
             (long "only-errors"
                <> help "Only report incompatibility errors in the output.")
           <|> flag'
             All
             (long "all"
                <> help
                  "Report both incompatible and compatible changes. \
                  \Compatible changes will not trigger a failure exit code.")
           <|> pure All)
    <*> ((FileMode
            <$> strOption
              (short 'o' <> long "output"
                 <> help
                   "The file path where the output should be writtrn. \
                   \Leave blank to output result to stdout."))
           <|> pure StdoutMode)
    <*> (ReportConfig
           <$> (flag'
                  FoldingBlockquotesTreeStyle
                  (long "folding-block-quotes-style"
                     <> help
                       "The report tree is structured using \
                       \summary/detail HTML elements and indented using \
                       \block quotes. This style renders well on GitHub.\
                       \Intended for HTML output format. Markdown has rendering \
                       \bugs on GitHub.")
                  <|> flag'
                    HeadersTreeStyle
                    (long "header-style"
                       <> help
                         "The report tree is structured using \
                         \increasing levels of headers.")
                  <|> pure HeadersTreeStyle))
