Often the lists we create have only a few elements in them.  This library exposes a collection type, `tuple`, which is optimized for these cases.

Compared lists and vectors, a two element tuple is ~2-3x faster to create, destructure, calculate a hash, check for equality, and look up in a normal Java hash-map.  Some of these gains are amplified at larger sizes; a five element tuple is ~30x faster to create than a vector.  However, tuples larger than ~8 elements will perform similarly to Clojure's persistent lists.

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