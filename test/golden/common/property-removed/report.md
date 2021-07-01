# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 2                                        | 2                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$.property2`

The property has been removed.

### 📱⬅️ JSON Response – 200

#### `$.property2`

The property may not be present.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$.property2`

The property may not be present.

### 📱⬅️ JSON Response – 200

#### `$.property2`

The property has been removed.
