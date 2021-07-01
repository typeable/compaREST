# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 3                                        | 3                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱⬅️ JSON Response – 200

#### `$.property2`

Type was required to be Number.

#### Object

1.  Property `property2` may not be present.

2.  Property `property2` was handled by a specific "properties" clause.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$.property2`

Type was required to be Number.

#### Object

1.  Property `property2` may not be present.

2.  Property `property2` was handled by a specific "properties" clause.
