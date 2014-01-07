Often the lists we create have only a few elements in them.  This library provides a collection type, `tuple`, which is optimized for these cases.  

A tuple behaves exactly like a Clojure vector.  However, compared lists and vectors, a two element tuple is ~2-3x faster to create, destructure, calculate a hash, check for equality, and look up in a normal Java hash-map.  Some of these gains are amplified at larger sizes; a five element tuple is ~30x faster to create than a vector.  Tuples larger than six elements, however, simply turn into standard vectors.

### usage

[![Build Status](https://travis-ci.org/ztellman/clj-tuple.png?branch=master)](https://travis-ci.org/ztellman/clj-tuple)

```clj
[clj-tuple "0.1.4"]
```

```clj
clj-tuple> (tuple 1 2 3)
[1 2 3]
```

### license

Copyright © 2013 Zachary Tellman

Distributed under the MIT License
