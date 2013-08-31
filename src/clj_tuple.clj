(ns clj-tuple
  (:require
    [clojure.core.protocols :as p])
  (:import
    [clojure.lang
     Util
     IMapEntry]
    [java.util
     Map$Entry
     Iterator
     Collection]))

(set! *unchecked-math* true)

;;; utility functions appropriated from potemkin

(defn- walk
  "Like `clojure.walk/walk`, but preserves metadata."
  [inner outer form]
  (let [x (cond
            (list? form) (outer (apply list (map inner form)))
            (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
            (seq? form) (outer (doall (map inner form)))
            (coll? form) (outer (into (empty form) (map inner form)))
            :else (outer form))]
    (if (instance? clojure.lang.IObj x)
      (with-meta x (meta form))
      x)))

(defn- postwalk
  "Like `clojure.walk/postwalk`, but preserves metadata."
  [f form]
  (walk (partial postwalk f) f form))

(def ^:private gensym-regex #"(_|[a-zA-Z0-9\-\'\*]+)#?_+(\d+_*#?)+(auto__)?$")
(def ^:private unified-gensym-regex #"([a-zA-Z0-9\-\'\*]+)#__\d+__auto__$")

(defn- unified-gensym? [s]
  (and
    (symbol? s)
    (re-find unified-gensym-regex (str s))))

(defn- un-gensym [s]
  (second (re-find gensym-regex (str s))))

(defn- unify-gensyms
  "All gensyms defined using two hash symbols are unified to the same
   value, even if they were defined within different syntax-quote scopes."
  [body]
  (let [gensym* (memoize gensym)]
    (postwalk
      #(if (unified-gensym? %)
         (symbol (str (gensym* (str (un-gensym %) "__")) "__auto__"))
         %)
      body)))

;;;

(defn map-entry [k v]
  (reify
    IMapEntry
    Map$Entry
    
    (key [_] k)
    (getKey [_] k)

     (val [_] v)
    (getValue [_] v)
    
    (hashCode [_]
      (bit-xor (Util/hash k) (Util/hash v)))
    (equals [_ x]
      (and (instance? Map$Entry x)
        (Util/equals k (.getKey ^Map$Entry x))
        (Util/equals v (.getValue ^Map$Entry x))))))

(declare conj-tuple)

(defn- throw-arity [actual]
  (throw
    (RuntimeException.
      (str "Wrong number of args (" actual ")"))))

(defmacro ^:private def-tuple [name dec-name cardinality]
  (let [fields (map
                 #(symbol (str "e" %))
                 (range cardinality))
        other (with-meta `x## {:tag (str name)})
        lookup (fn this
                 ([idx]
                    (this idx `(throw (IndexOutOfBoundsException. (str ~idx)))))
                 ([idx default]
                    `(let [idx# ~idx]
                       (case idx#
                         ~@(mapcat
                             (fn [n field]
                               `(~n ~field))
                             (range)
                             fields)
                         ~default))))]
    (unify-gensyms
      `(do

         (deftype ~name [~@fields mta##]

           clojure.lang.IObj
           (meta [_] mta##)
           (withMeta [_ m#] (new ~name ~@fields m#))
           
           java.util.concurrent.Callable
           (call [this##]
             (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))
           
           java.lang.Runnable
           (run [this##]
             (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))

           clojure.lang.ILookup
           (valAt [_ k##] ~(lookup `(int k##) nil))
           (valAt [_ k## not-found##] ~(lookup `(int k##) `not-found##))
       
           clojure.lang.IFn
           ~@(map
             (fn [n]
               `(~'invoke [this# ~@(repeat n '_)]
                  (throw-arity ~n)))
             (remove #{1} (range 0 21)))
           
           (invoke [_ idx##]
             ~(lookup `(int idx##)))
       
           (applyTo [this## args##]
             (let [cnt# (count args##)]
               (if (= 1 cnt#)
                 ~(lookup `(int (first args##)))
                 (throw-arity cnt#))))

           ~@(when (= 2 cardinality)
               `(IMapEntry
                  Map$Entry
                  
                  (key [_] ~(first fields))
                  (getKey [_] ~(first fields))
                  
                  (val [_] ~(second fields))
                  (getValue [_] ~(second fields))))

           clojure.lang.Associative
           clojure.lang.IPersistentVector
           (count [_] ~cardinality)
           (length [_] ~cardinality)
           
           (containsKey [_ k##]
             ~(condp = cardinality
                0 false
                1 `(= 0 k##)
                `(and (number? k##)
                   (<= 0 k## ~(dec cardinality)))))
           (entryAt [_ k##]
             (let [v# ~(lookup `(int k##))]
               (map-entry k## v#)))
           (assoc [this# k# v##]
             (case (int k#)
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) `mta##)))
                   (range)
                   fields)
               ~cardinality (.cons this# v##)
               (throw (IndexOutOfBoundsException. (str k#)))))
           (assocN [this# k# v##]
             (case k#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) `mta##)))
                   (range)
                   fields)
               ~cardinality (.cons this# v##)
               (throw (IndexOutOfBoundsException. (str k#)))))

           java.util.Collection
           (isEmpty [_] ~(zero? cardinality))
           (iterator [_]
             (let [^Collection l# (list ~@fields)]
               (.iterator l#)))
           (toArray [_]
             (let [ary## (object-array ~cardinality)]
               ~@(map
                   (fn [idx field] `(aset ary## ~idx ~field))
                   (range)
                   fields)
               ary##))
           (size [_] ~cardinality)

           clojure.lang.IPersistentCollection
           clojure.lang.Indexed
           clojure.lang.Sequential
           clojure.lang.ISeq
           clojure.lang.Seqable

           (first [_]
             ~(first fields))
           (next [this##]
             ~(when (> cardinality 1)
                `(new ~dec-name ~@(rest fields) nil)))
           (more [this##]
             (if-let [rst# (next this##)]
               rst#
               '()))
           (cons [this# k#]
              (conj-tuple this# k#))
           (peek [_]
             ~(last fields))
           (pop [_]
             ~(when (> cardinality 1)
                `(new ~dec-name ~@(butlast fields) nil)))
           (rseq [_]
             (new ~name ~@(reverse fields) nil))
           (seq [this##]
             ~(when-not (zero? cardinality)
                `this##))

           (nth [_ idx## not-found##]
             ~(lookup `idx## `not-found##))
           (nth [_ idx##]
             ~(lookup `idx##))
           
           (equiv [this# x##]
             (if (instance? ~name x##)
               ~(if (zero? cardinality)
                  true
                  `(and
                     ~@(map
                         (fn [f]
                           `(Util/equiv ~f (. ~other ~f)))
                         fields)))
               (and (== ~cardinality (count x##))
                 (Util/equiv x## this#))))
           
           (equals [this# x##]
             (if (instance? ~name x##)
               ~(if (zero? cardinality)
                  true
                  `(and
                     ~@(map
                         (fn [f]
                           `(Util/equals ~f (. ~other ~f)))
                         fields)))
               (and (== ~cardinality (count x##))
                 (Util/equals x## this#))))

           Comparable
           (compareTo [this# x##]
             (if (instance? ~name x##)
               ~(condp = cardinality
                  0 0
                  1 `(compare ~(first fields) (. ~other ~(first fields)))
                  (reduce
                    (fn [form field]
                      `(let [cmp# (compare ~field (. ~other ~field))]
                         (if (== 0 cmp#)
                           ~form
                           cmp#)))
                    0
                    (reverse fields)))
               (let [cnt# (count x##)]
                 (if (== ~cardinality cnt#)
                   (- (compare x## this#))
                   (- ~cardinality cnt#)))))
           
           (hashCode [_]
             ~(if (zero? cardinality)
                1
                `(unchecked-int
                   ~(reduce
                      (fn
                        ([form]
                           form)
                        ([form x]
                           `(+ (* 31 ~form) (Util/hash ~x))))
                      1
                      fields))))
           
           clojure.lang.IHashEq
           (hasheq [_]
             ~(if (zero? cardinality)
                1
                `(unchecked-int
                   ~(reduce
                      (fn
                        ([form]
                           form)
                        ([form x]
                           `(+ (* 31 ~form) (Util/hasheq ~x))))
                      1
                      fields))))

           p/InternalReduce
           (internal-reduce [_ f## start##]
             ~(if (zero? cardinality)
                `(f## start##)
                (reduce
                  (fn [form field]
                    `(f## ~form ~field))
                  `start##
                  fields)))

           p/CollReduce
           (coll-reduce [_ f##]
             ~(if (zero? cardinality)
                `(f##)
                (reduce
                  (fn [form field]
                    `(f## ~form ~field))
                  (first fields)
                  (rest fields))))
           (coll-reduce [_ f## start##]
             ~(if (zero? cardinality)
                `(f## start##)
                (reduce
                  (fn [form field]
                    `(f## ~form ~field))
                  `start##
                  fields)))

           (toString [_]
             (str "[" ~@(->> fields (map (fn [f] `(pr-str ~f))) (interpose " ")) "]")))

         (defmethod print-method ~name [o# ^java.io.Writer w#]
           (.write w# (str o#)))))))

(def-tuple Tuple0 nil 0)
(def-tuple Tuple1 Tuple0 1)
(def-tuple Tuple2 Tuple1 2)
(def-tuple Tuple3 Tuple2 3)
(def-tuple Tuple4 Tuple3 4)
(def-tuple Tuple5 Tuple4 5)
(def-tuple Tuple6 Tuple5 6)

(eval
  (unify-gensyms
    `(defn- conj-tuple [t## x##]
       (let [^clojure.lang.Counted t## t##]
         (case (.count t##)
           ~@(mapcat
               (fn [idx]
                 (let [nm (symbol (str "Tuple" (inc idx)))]
                   `(~idx
                      (new ~nm
                        ~@(map
                            (fn [n] `(. ~(with-meta `t## {:tag (str "Tuple" idx)}) ~(symbol (str "e" n))))
                            (range idx))
                        x##
                        nil))))
               (range 6))
           6 (conj (clojure.lang.PersistentVector/create ^clojure.lang.ISeq t##) x##)
           )))))

(defn tuple
  "Returns a tuple which behaves like a vector, but is highly efficient for index lookups, hash
   calculations, equality checks, and reduction.  If there are more than six elements, returns a
   normal vector."
  ([]
     (Tuple0. nil))
  ([x]
     (Tuple1. x nil))
  ([x y]
     (Tuple2. x y nil))
  ([x y z]
     (Tuple3. x y z nil))
  ([x y z w]
     (Tuple4. x y z w nil))
  ([x y z w u]
     (Tuple5. x y z w u nil))
  ([x y z w u v]
     (Tuple6. x y z w u v nil))
  ([x y z w u v & rst]
     (let [r (-> []
               transient
               (conj! x)
               (conj! y)
               (conj! z)
               (conj! w)
               (conj! u)
               (conj! v))]
       (loop [r r, s rst]
         (if (empty? s)
           (persistent! r)
           (recur (conj! r (first s)) (rest s)))))))
