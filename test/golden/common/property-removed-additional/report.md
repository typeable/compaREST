# Summary

| [❌ Breaking changes](#breaking-changes) | [⚠️ Non-breaking changes](#non-breaking-changes) |
|------------------------------------------|--------------------------------------------------|
| 3                                        | 3                                                |

# <span id="breaking-changes"></span>❌ Breaking changes

## **POST** /test

### ⬅️☁️ JSON Response – 200

#### `$.property2`

1.  Values are no longer limited to the following types:

    -   Number

2.  The property was previously explicitly defined. It is now implicitly
    described by the catch-all "additional properties" case.

3.  The property may not be present.

# <span id="non-breaking-changes"></span>⚠️ Non-breaking changes

## **POST** /test

### ➡️☁️ JSON Request

#### `$.property2`

1.  Values are no longer limited to the following types:

    -   Number

2.  The property was previously explicitly defined. It is now implicitly
    described by the catch-all "additional properties" case.

3.  The property may not be present.
