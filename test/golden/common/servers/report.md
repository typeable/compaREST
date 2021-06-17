# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 3                                        | 1                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **GET** /pets

### Server `http://missing.url`

The server was removed.

### Server `http://{x}variable.path/{y}/{openVariable1}/{openVariable2}`

Enum value `a` has been removed.

A variable has been changed from being open to being closed.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **GET** /pets

### Server `http://{x}variable.path/{y}/{openVariable1}/{openVariable2}`

Enum value `bbb` has been removed.
