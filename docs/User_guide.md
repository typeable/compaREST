CompaREST User Guide
====================

Running the Tool
----------------

The tool accepts two OpenAPI 3.0.0 schema files, in either JSON or YAML format.
One is assumed to be the client version of the schema, and the other is the
server version.

The tool will look for changes between the schemas, and detect whether they are
breaking or not, that is, if they prevent interoperability between the client
and the server.

Running:
```
comparest -c client.json -s server.json
```
will output a markdown report of the changes to the standard output.

Compatibility status can be signaled via the exit code using
`--signal-exit-code`. In case no report is needed, output can be suppressed with
`--silent`. For example:
```
comparest -c client.json -s server.json --signal-exit-code --silent
echo $?
```
If there were changes that the tool determined to be breaking, the exit code
will be 1. If there were some changes the tool couldn't understand, the exit
code will be 2. Otherwise if there were no breaking changes, the exit code will
be 0.

Controlling the Report
----------------------

By default the report includes breaking changes, as well as non-breaking:
changes that would be considered breaking in the opposite direction. To only
include breaking changes in the report use `--only-breaking`. The `--all` option
restores the default behavior.

The report can be formatted in a variety of ways supported by Pandoc. The `-o`
option causes the report to be written to a file. The format of the file is
determined from the extension. The supported extensions include:
 - `.md` for markdown
 - `.html` for an HTML snippet
 - `.rst` for restructured text
 - no extension for a self-contained HTML document with styles

By default the report is split up into parts relating to different paths,
requests, responses, etc, using headers of various levels. Alternatively, the
report can use indended block-quotes to visualize the tree structure of the
report. The header style is enabled with `--header-style`, and the block-quote
style is enabled with `--folding-block-quotes-style`.
