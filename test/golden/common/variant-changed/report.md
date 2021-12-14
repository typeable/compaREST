# Summary

| [❌ Breaking changes](#breaking-changes) | [⚠️ Non-breaking changes](#non-breaking-changes) |
|------------------------------------------|--------------------------------------------------|
| 2                                        | 2                                                |

# <span id="breaking-changes"></span>❌ Breaking changes

## **POST** /test

### ➡️☁️ JSON Request

#### In cases where `$.tag` is `"B"`

##### `$.prop_B(Number)`

The type has been removed.

### ⬅️☁️ JSON Response – 200

#### In cases where `$.tag` is `"B"`

##### `$.prop_B(String)`

The type has been added.

# <span id="non-breaking-changes"></span>⚠️ Non-breaking changes

## **POST** /test

### ➡️☁️ JSON Request

#### In cases where `$.tag` is `"B"`

##### `$.prop_B(String)`

The type has been added.

### ⬅️☁️ JSON Response – 200

#### In cases where `$.tag` is `"B"`

##### `$.prop_B(Number)`

The type has been removed.
