# Summary

| [❌ Breaking changes](#breaking-changes) | [⚠️ Non-breaking changes](#non-breaking-changes) |
|------------------------------------------|--------------------------------------------------|
| 3                                        | 1                                                |

# <span id="breaking-changes"></span>❌ Breaking changes

## **GET** /pets

### Server `http://missing.url`

The server was removed.

### Server `http://{x}variable.path/{y}/{openVariable1}/{openVariable2}`

1.  Enum value `a` has been removed.

2.  A variable has been changed from being open to being closed.

# <span id="non-breaking-changes"></span>⚠️ Non-breaking changes

## **GET** /pets

### Server `http://{x}variable.path/{y}/{openVariable1}/{openVariable2}`

Enum value `bbb` has been added.
