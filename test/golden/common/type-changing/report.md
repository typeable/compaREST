# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 6                                        | 6                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test1

### 📱➡️ JSON Request

Values are now limited to the following types:

-   Array

## **POST** /test2

### 📱➡️ JSON Request

Values are now limited to the following types:

-   Number

-   Array

-   Object

## **POST** /test3

### 📱➡️ JSON Request

#### `$(Array)`

The value has been removed.

## **POST** /test4

### 📱➡️ JSON Request

#### `$(Object)`

The value has been removed.

## **POST** /test5

### 📱➡️ JSON Request

Values are now limited to the following types:

-   Number

#### `$(Number)`

Value is now a multiple of 1.0.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test1

### 📱⬅️ JSON Response – 200

Values are now limited to the following types:

-   Array

## **POST** /test2

### 📱⬅️ JSON Response – 200

Values are now limited to the following types:

-   Number

-   Array

-   Object

## **POST** /test3

### 📱⬅️ JSON Response – 200

#### `$(Array)`

The value has been removed.

## **POST** /test4

### 📱⬅️ JSON Response – 200

#### `$(Object)`

The value has been removed.

## **POST** /test5

### 📱⬅️ JSON Response – 200

Values are now limited to the following types:

-   Number

#### `$(Number)`

Value is now a multiple of 1.0.
