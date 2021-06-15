# Summary

| [⚠️ Breaking changes](#breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------|
| 2                                        | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### JSON Request

#### `$(Object)`

Expected the property `property2` to be allowed, but it wasn't.

### JSON Response – 200

#### `$(Object)`

Didn't expect the property `property2` to be required, but it was.
