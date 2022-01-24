module Data.OpenApi.Compare.Options
  ( Options (..),
    OutputMode (..),
    parseOptions,
  )
where

import Data.OpenApi.Compare.Report
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.Help hiding (fullDesc)

parseOptions :: IO Options
parseOptions = customExecParser (prefs $ showHelpOnError) optionsParserInfo

data Options = Options
  { clientFile :: FilePath
  , serverFile :: FilePath
  , -- | 'Nothing' means "don't produce any output"
    mode :: Maybe ReportMode
  , outputMode :: OutputMode
  , reportTreeStyle :: ReportTreeStyle
  , signalExitCode :: Bool
  }
  deriving stock (Generic)

data OutputMode = StdoutMode | FileMode FilePath

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (helper <*> optionsParser)
    ( fullDesc
        <> header "CompaREST"
        <> progDescDoc
          ( Just $
              par "A tool to check compatibility between two OpenAPI specifications."
                <$$> hardline <> par "Usage examples" <> hardline
                <$$> indent
                  4
                  ( par "Compare files old.yaml with new.yaml and output the resulting report to stdout:"
                      <$$> hardline <> indent 4 "comparest -c old.yaml -s new.yaml"
                      <$$> hardline <> par "Only output breaking changes and write a styled HTML report to file report.html:"
                      <$$> hardline <> indent 4 "comparest -c old.yaml -s new.yaml --only-breaking -o report"
                      <$$> hardline <> par "Don't output anything, only fail if there are breaking changes:"
                      <$$> hardline <> indent 4 "comparest -c old.json -s new.json --silent"
                      <$$> hardline <> par "Write full report suitable for embedding into a GitHub comment to report.html:"
                      <$$> hardline <> indent 4 "comparest -c old.json -s new.json --folding-block-quotes-style -o report.html"
                  )
          )
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( short 'c'
          <> long "client"
          <> help
            "A path to the file containing the specification that will be \
            \used for the client of the API. Can be either a YAML or JSON file."
      )
    <*> strOption
      ( short 's'
          <> long "server"
          <> help
            "A path to the file containing the specification that will be \
            \used for the server of the API. Can be either a YAML or JSON file."
      )
    <*> ( flag'
            Nothing
            ( long "silent"
                <> help "Silence all output. Only makes sense in combination with --signal-exit-code."
            )
            <|> flag'
              (Just OnlyErrors)
              ( long "only-breaking"
                  <> help "Only report breaking changes in the output."
              )
            <|> flag'
              (Just All)
              ( long "all"
                  <> help
                    "Report both incompatible and compatible changes. \
                    \Compatible changes will not trigger a failure exit code."
              )
            <|> pure (Just All)
        )
    <*> ( ( FileMode
              <$> strOption
                ( short 'o' <> long "output"
                    <> helpDoc
                      ( Just $
                          par "The file path where the output should be written. If the option is omitted the result will be written to stdout."
                            <$$> hardline <> par "The file extension is used to determine the type of the output file."
                            <$$> hardline <> par "Supports many formats such as markdown, html, rtf, doc, txt, rst, and many more."
                            <$$> hardline <> par "Leave out the extension to produce a self-contained HTML report with styling."
                      )
                )
          )
            <|> pure StdoutMode
        )
    <*> ( flag'
            FoldingBlockquotesTreeStyle
            ( long "folding-block-quotes-style"
                <> help
                  "The report tree is structured using \
                  \summary/detail HTML elements and indented using \
                  \block quotes. This style renders well on GitHub.\
                  \Intended for HTML output format. Markdown has rendering \
                  \bugs on GitHub."
            )
            <|> flag'
              HeadersTreeStyle
              ( long "header-style"
                  <> help
                    "The report tree is structured using \
                    \increasing levels of headers."
              )
            <|> pure HeadersTreeStyle
        )
    <*> switch
      ( long "signal-exit-code"
          <> helpDoc
            ( Just $
                par "Signal API compatibility with the exit code."
                  <$$> hardline <> par "Exit with 0 if there are no breaking changes."
                  <$$> par "Exit with 1 if there are breaking changes."
                  <$$> par "Exit with 2 if could not determine compatibility."
            )
      )

par :: String -> Doc
par = foldr1 (</>) . fmap string . words
