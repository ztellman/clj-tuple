Often the lists we create have only a few elements in them.  This library exposes a collection type, `tuple`, which is optimized for lists with six or fewer elements.

Compared lists and vectors, 2-tuple is ~2-3x faster to create, destructure, calculate a hash, check for equality, and look up in a normal Java hash-map.  Some of these changes are amplified at larger sizes; a 5-tuple is ~30x faster to create than a five element vector.

## Usage

```clj
[clj-tuple "0.1.0"]
```

```clj
clj-tuple> (tuple 1 2 3)
(1 2 3)
```

## license

Copyright © 2013 Zachary Tellman

Distributed under the MIT License