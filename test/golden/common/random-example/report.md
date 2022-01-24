# Summary

| [❌ Breaking changes](#breaking-changes) | [⚠️ Non-breaking changes](#non-breaking-changes) |
|------------------------------------------|--------------------------------------------------|
| 5                                        | 6                                                |

# <span id="breaking-changes"></span>❌ Breaking changes

## **GET** /pets

### ⬅️☁️ JSON Response – 200

#### `$[*].name(String)`

1.  Maximum length of the string changed from 10 to 15.

2.  Minimum length of the string changed from 3 to 1.

## **POST** /pets

### ➡️☁️ JSON Request

#### `$.weight`

1.  Values are now limited to the following types:

    -   Number

2.  The property was previously implicitly described by the catch-all
    "additional properties" case. It is now explicitly defined.

#### `$.weight(Number)`

Value is now a multiple of 1.0.

# <span id="non-breaking-changes"></span>⚠️ Non-breaking changes

## **GET** /pets

### Parameter limit

#### JSON Schema

##### `$(Number)`

Upper bound changed from 20.0 inclusive to 30.0 inclusive.

### ⬅️☁️ JSON Response – 200

#### `$[*].weight`

1.  Values are now limited to the following types:

    -   Number

2.  The property was previously implicitly described by the catch-all
    "additional properties" case. It is now explicitly defined.

#### `$[*].weight(Number)`

Value is now a multiple of 1.0.

## **POST** /pets

### ➡️☁️ JSON Request

#### `$.name(String)`

1.  Maximum length of the string changed from 10 to 15.

2.  Minimum length of the string changed from 3 to 1.
