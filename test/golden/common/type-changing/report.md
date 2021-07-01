# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 6                                        | 6                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test1

### 📱➡️ JSON Request

Type is now required to be Array.

## **POST** /test2

### 📱➡️ JSON Request

Type is now required to be Number, Array, or Object.

## **POST** /test3

### 📱➡️ JSON Request

#### `$(Array)`

The type has been removed.

## **POST** /test4

### 📱➡️ JSON Request

#### `$(Object)`

The type has been removed.

## **POST** /test5

### 📱➡️ JSON Request

Type is now required to be Number.

#### `$(Number)`

Value is now a multiple of 1.0.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test1

### 📱⬅️ JSON Response – 200

Type is now required to be Array.

## **POST** /test2

### 📱⬅️ JSON Response – 200

Type is now required to be Number, Array, or Object.

## **POST** /test3

### 📱⬅️ JSON Response – 200

#### `$(Array)`

The type has been removed.

## **POST** /test4

### 📱⬅️ JSON Response – 200

#### `$(Object)`

The type has been removed.

## **POST** /test5

### 📱⬅️ JSON Response – 200

Type is now required to be Number.

#### `$(Number)`

Value is now a multiple of 1.0.
