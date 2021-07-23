# openapi-diff

[![Hackage](https://img.shields.io/hackage/v/openapi-diff.svg?logo=haskell)](https://hackage.haskell.org/package/openapi-diff)
[![Stackage Lts](http://stackage.org/package/openapi-diff/badge/lts)](http://stackage.org/lts/package/openapi-diff)
[![Stackage Nightly](http://stackage.org/package/openapi-diff/badge/nightly)](http://stackage.org/nightly/package/openapi-diff)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Compatibility checker for OpenAPI


```
Usage: openapi-diff (-c|--client ARG) (-s|--server ARG)
                    [--silent | --only-breaking | --all] [-o|--output ARG]
                    [--folding-block-quotes-style | --header-style]
                    [--signal-exit-code]
  A tool to check compatibility between two OpenApi specifications.

  Usage examples

      Compare files old.yaml with new.yaml and output the resulting report to
      stdout:

          openapi-diff -c old.yaml -s new.yaml

      Only output breaking changes and write a styled HTML report to file
      report.html:

          openapi-diff -c old.yaml -s new.yaml --only-breaking -o report

      Don't output anything, only fail if there are breaking changes:

          openapi-diff -c old.json -s new.json --silent

      Write full report suitable for embedding into a GitHub comment to
      report.html:

          openapi-diff -c old.json -s new.json --folding-block-quotes-style -o report.html

Available options:
  -h,--help                Show this help text
  -c,--client ARG          A path to the file containing the specification that
                           will be used for the client of the API. Can be either
                           a YAML or JSON file.
  -s,--server ARG          A path to the file containing the specification that
                           will be used for the server of the API. Can be either
                           a YAML or JSON file.
  --silent                 Silence all output. Only makes sense in combination
                           with --signal-exit-code.
  --only-breaking          Only report breaking changes in the output.
  --all                    Report both incompatible and compatible changes.
                           Compatible changes will not trigger a failure exit
                           code.
  -o,--output ARG          The file path where the output should be written. If
                           the option is omitted the result will be written to
                           stdout.

                           The file extension is used to determine the type of
                           the output file.

                           Supports many formats such as markdown, html, rtf,
                           doc, txt, rst, and many more.

                           Leave out the extension to produce a self-contained
                           HTML report with styling.
  --folding-block-quotes-style
                           The report tree is structured using summary/detail
                           HTML elements and indented using block quotes. This
                           style renders well on GitHub.Intended for HTML output
                           format. Markdown has rendering bugs on GitHub.
  --header-style           The report tree is structured using increasing levels
                           of headers.
  --signal-exit-code       Signal API compatibility with the exit code.

                           Exit with 0 if there are no breaking changes.
                           Exit with 1 if there are breaking changes.
                           Exit with 2 if could not determine compatibility.
```
