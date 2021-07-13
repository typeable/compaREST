# Summary

| [⚠️ Breaking changes](#breaking-changes) | [🙆 Non-breaking changes](#non-breaking-changes) | 🤷 Unsupported feature changes |
|------------------------------------------|-------------------------------------------------|-------------------------------|
| 2                                        | 2                                               | 0                             |

# <span id="breaking-changes"></span>⚠️ Breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$|PInProperty "tag" PHere|CByEnumValue (fromList [String "B"])|.prop_B(Number)`

The type has been removed.

### 📱⬅️ JSON Response – 200

#### `$|PInProperty "tag" PHere|CByEnumValue (fromList [String "B"])|.prop_B(String)`

The type has been added.

# <span id="non-breaking-changes"></span>🙆 Non-breaking changes

## **POST** /test

### 📱➡️ JSON Request

#### `$|PInProperty "tag" PHere|CByEnumValue (fromList [String "B"])|.prop_B(String)`

The type has been added.

### 📱⬅️ JSON Response – 200

#### `$|PInProperty "tag" PHere|CByEnumValue (fromList [String "B"])|.prop_B(Number)`

The type has been removed.
