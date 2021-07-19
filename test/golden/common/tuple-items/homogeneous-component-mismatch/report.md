# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 4                                        | 4                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$(Array)`

The array is no longer explicitly defined as a tuple.

#### `$[0](String)`

The type has been removed.

### 📱⬅️ JSON Response – 200

#### `$(Array)`

The array is no longer explicitly defined as a tuple.

#### `$[0](Number)`

The type has been added.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$(Array)`

The array is no longer explicitly defined as a tuple.

#### `$[0](Number)`

The type has been added.

### 📱⬅️ JSON Response – 200

#### `$(Array)`

The array is no longer explicitly defined as a tuple.

#### `$[0](String)`

The type has been removed.
