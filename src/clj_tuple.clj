(ns clj-tuple
  (:import
    [clojure.lang
     PersistentUnrolledVector
     PersistentUnrolledMap])
  (:refer-clojure :exclude [vector hash-map]))

(defn hash-map
  ([]
     (PersistentUnrolledMap/create))
  ([k1 v1]
     (PersistentUnrolledMap/create k1 v1))
  ([k1 v1 k2 v2]
     (PersistentUnrolledMap/create k1 v1 k2 v2))
  ([k1 v1 k2 v2 k3 v3]
     (PersistentUnrolledMap/create k1 v1 k2 v2 k3 v3))
  ([k1 v1 k2 v2 k3 v3 k4 v4]
     (PersistentUnrolledMap/create k1 v1 k2 v2 k3 v3 k4 v4))
  ([k1 v1 k2 v2 k3 v3 k4 v4 k5 v5]
     (PersistentUnrolledMap/create k1 v1 k2 v2 k3 v3 k4 v4 k5 v5))
  ([k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6]
     (PersistentUnrolledMap/create k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6))
  ([k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 & rst]
     (let [m (transient (PersistentUnrolledMap/create k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6))]
       (loop [m m, s rst]
         (if (empty? s)
           (persistent! m)
           (let [k (first s)
                 s (rest s)]
             (when (empty? s)
               (throw
                 (IllegalArgumentException.
                   "Cannot pass an odd number of arguments into 'map'.")))
            (recur (assoc! m k (first s)) (rest s))))))))

(defn vector
  "Returns a collection which behaves like a vector, but is highly efficient for index lookups,
   hash calculations, equality checks, and reduction.  If there are more than six elements,
   returns a normal vector."
  ([]
     (PersistentUnrolledVector/create))
  ([x]
     (PersistentUnrolledVector/create x))
  ([x y]
     (PersistentUnrolledVector/create x y))
  ([x y z]
     (PersistentUnrolledVector/create x y z))
  ([x y z w]
     (PersistentUnrolledVector/create x y z w))
  ([x y z w u]
     (PersistentUnrolledVector/create x y z w u))
  ([x y z w u v]
     (PersistentUnrolledVector/create x y z w u v))
  ([x y z w u v & rst]
     (let [r (transient (PersistentUnrolledVector/create x y z w u v))]
       (loop [r r, s rst]
         (if (empty? s)
           (persistent! r)
           (recur (conj! r (first s)) (rest s)))))))

(def tuple vector)
