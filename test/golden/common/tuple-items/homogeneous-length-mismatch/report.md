# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 5                                        | 5                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test1

### 📱⬅️ JSON Response – 200

#### `$(Array)`

Tuple specification has been removed

## **POST** /test2

### 📱⬅️ JSON Response – 200

#### `$(Array)`

Tuple specification has been removed

## **POST** /test3

### 📱➡️ JSON Request

#### `$(Array)`

Minimum length of the array changed from 2 to 3.

### 📱⬅️ JSON Response – 200

#### `$(Array)`

1.  Tuple length changed from 3 to 2.

2.  The array is no longer explicitly defined as a tuple.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test1

### 📱➡️ JSON Request

#### `$(Array)`

Tuple specification has been removed

## **POST** /test2

### 📱➡️ JSON Request

#### `$(Array)`

Tuple specification has been removed

## **POST** /test3

### 📱➡️ JSON Request

#### `$(Array)`

1.  Tuple length changed from 3 to 2.

2.  The array is no longer explicitly defined as a tuple.

### 📱⬅️ JSON Response – 200

#### `$(Array)`

Minimum length of the array changed from 2 to 3.
