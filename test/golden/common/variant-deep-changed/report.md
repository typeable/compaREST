# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 2                                        | 2                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### In cases where `$.desc.name` is `"B"`.

##### `$.prop_B(Number)`

The value has been removed.

### 📱⬅️ JSON Response – 200

#### In cases where `$.desc.name` is `"B"`.

##### `$.prop_B(String)`

The value has been added.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### In cases where `$.desc.name` is `"B"`.

##### `$.prop_B(String)`

The value has been added.

### 📱⬅️ JSON Response – 200

#### In cases where `$.desc.name` is `"B"`.

##### `$.prop_B(Number)`

The value has been removed.
