# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 3                                        | 3                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱⬅️ JSON Response – 200

#### `$.property2`

1.  The following types were removed:

    -   Number

2.  The property was previously explicitly defined. It is now implicitly
    described by the catch-all "additional properties" case.

3.  The property may not be present.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$.property2`

1.  The following types were removed:

    -   Number

2.  The property was previously explicitly defined. It is now implicitly
    described by the catch-all "additional properties" case.

3.  The property may not be present.
