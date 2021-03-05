# Golden tests

## The basics

The `test/golden` directory contains a tree with golden tests. A test is a any nested directory with no folders nested inside it (i. e. tests can only be leaves in the file system):

```
id
├── a.yaml
├── b.yaml
└── report.yaml
```

All of the files and their meaning are specified in code:


```haskell
tests :: IO TestTree
tests =
  goldenInputsTreeUniform
    "Golden Reports"
    "test/golden/common"
    "report.yaml"
    ("a.yaml", "b.yaml")
    Yaml.decodeFileThrow
    (uncurry reportCompat)
```

This test would read the files `a.yaml` and `b.yaml` using the `Yaml.decodeFileThrow` function, and pass the resulting tuple to `uncurry reportCompat`. The resul will be compared to `report.yaml`.

## Supported feature tests

If a test starts with either `x ` or `v `, the test is assumed to test the support of some OpenApi functionality. If the test begins `x ` it means that the feature is expected to be not supported, and the result will only be checked to have an "unsupported" flag set. If it starts with `v ` the test is expected to be supported and the result is compared to some file on disk.

The result will be reflected in the generated compatibility matrix.

If a test does not begin with either `x ` or `v `, then it is presumed to be a normal test and will not be reflected in the resulting compatibility matrix.
