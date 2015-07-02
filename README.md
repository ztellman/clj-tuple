This library provides efficient implementations for small maps and vectors, discussed more in [this post](http://blog.factual.com/using-clojure-to-generate-java-to-reimplement-clojure).  These implementations will eventually be merged into Clojure proper, but for now can be used by replacing `vector` with `clj-tuple/vector` and `hash-map` with `clj-tuple/hash-map`.

### usage

[![Build Status](https://travis-ci.org/ztellman/clj-tuple.png?branch=master)](https://travis-ci.org/ztellman/clj-tuple)

```clj
[clj-tuple "0.2.2"]
```

```clj
clj-tuple> (vector 1 2 3)
[1 2 3]
clj-tuple> (hash-map 1 2 3 4)
{1 2, 3 4}
```

### license

Copyright © 2015 Zachary Tellman

Distributed under the MIT License
