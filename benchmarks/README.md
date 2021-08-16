# OpenApi comparison benchmarks

This is a nix expression that automatically downloads, builds and runs multiple OpenAPI comparison tools on the same set of inputs to compare how well they behave.

To run the benchmarks run the following command in this directory:

```bash
nix-build
```

## The tools

| Tool                                                                       | Output file name                            |
| -------------------------------------------------------------------------- | ------------------------------------------- |
| [Typeable CompaREST](https://github.com/typeable/compaREST)                | `typeable.md`                               |
| [Atlassian OpenApi Diff](https://bitbucket.org/atlassian/openapi-diff)     | `atlassian.json`, `atlassian.error.txt`     |
| [OpenApi Tools OpenApi Diff](https://github.com/OpenAPITools/openapi-diff) | `openapitools.md`, `openapitools.error.txt` |
| [Tufin OpenApi Diff](https://github.com/tufin/oasdiff)                     | `oasdiff.txt`, `oasdiff.error.txt`          |
| [Bump](https://bump.sh)                                                    | `bump.txt`                                  |

## Running with Bump.sh

Bump.sh is a closed-source tool that is only available via REST API. To include bump.sh in the benchmark results you have to supply your credentials as arguments:

```bash
nix-build --argstr bumpToken <TOKEN> --argstr bumpDocumentation <DOCUMENTATION IDENTIFIER>
```
