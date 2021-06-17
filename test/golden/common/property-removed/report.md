# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 2                                        | 2                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$(Object)`

Property `property2` has been removed.

### 📱⬅️ JSON Response – 200

#### `$(Object)`

Property `property2` has become required.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$(Object)`

Property `property2` has become required.

### 📱⬅️ JSON Response – 200

#### `$(Object)`

Property `property2` has been removed.
