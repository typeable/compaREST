# openapi-diff

[![Hackage](https://img.shields.io/hackage/v/openapi-diff.svg?logo=haskell)](https://hackage.haskell.org/package/openapi-diff)
[![Stackage Lts](http://stackage.org/package/openapi-diff/badge/lts)](http://stackage.org/lts/package/openapi-diff)
[![Stackage Nightly](http://stackage.org/package/openapi-diff/badge/nightly)](http://stackage.org/nightly/package/openapi-diff)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Compatibility checker for OpenAPI
# Quick Start Guide

## Your situation

You are developing a very important server with a REST API. You have clients who use your API that you do not control. Say, you are also developing a mobile app that uses your API and you can't force someone to update to the latest version. (Or you prefer not to for UX reasons.)

You have recently released version 1.0 and things are going great: user are downloading your app, servers are processing requests.

You describe your API in a file `api-1.0.0.yaml`:

```yaml
openapi: "3.0.1"
info:
  version: 1.0.0
  title: Swagger Petstore
  license: name: MIT
servers:
  - url: https://example.com
paths:
  /pets:
    get:
      parameters:
        - name: limit
          in: query
          required: false
          schema:
            type: integer
            maximum: 20
      responses:
        '200':
          headers:
            x-next:
              schema: type: string
          content:
            application/json:
              schema: $ref: "#/components/schemas/Pets"
    post:
      requestBody:
        content:
          application/json:
            schema: $ref: "#/components/schemas/Pet"
      responses: '201':
components:
  schemas:
    Pet:
      type: object
      required:
        - id
        - name
      properties:
        id: type: integer
        name:
          type: string
          minLength: 3
          maxLength: 10
    Pets:
      type: array
      items: $ref: "#/components/schemas/Pet"
```

## A sudden change

Enthused over your initial success you hurry to release a new and improved version of your API and mobile app.

After a time of very intensive programming you take a look at your new `api-1.1.0.yaml`:

```yaml
openapi: "3.0.1"
info:
  version: 1.1.0
  title: Swagger Petstore
  license: name: MIT
servers:
  - url: https://example.com
paths:
  /pets:
    get:
      parameters:
        - name: limit
          in: query
          required: false
          schema:
            type: integer
            maximum: 30
      responses:
        '200':
          headers:
            x-next:
              schema: type: string
          content:
            application/json:
              schema: $ref: "#/components/schemas/Pets"
    post:
      requestBody:
        content:
          application/json:
            schema: $ref: "#/components/schemas/Pet"
      responses: '201':
components:
  schemas:
    Pet:
      type: object
      required:
        - id
        - name
      properties:
        id: type: integer
        name:
          type: string
          minLength: 1
          maxLength: 15
        weight: type: integer
    Pets:
      type: array
      items: $ref: "#/components/schemas/Pet"
```

Looking at the very large and complex API description, you grow more and more concerned that your old mobile app might stop working when you update the server. But the spec is too large and too complex to reasonably assess this manually.

# CLI docs

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
