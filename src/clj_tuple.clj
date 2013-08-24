(ns clj-tuple
  (:import
    [clojure.lang
     Util]
    [java.util
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

(defmacro ^:private def-tuple [name dec-name cardinality]
  (let [fields (map
                 #(symbol (str "e" %))
                 (range cardinality))
        other (with-meta `x## {:tag (str name)})]
    (unify-gensyms
      `(deftype ~name [~@fields ~(with-meta 'rst {:unsynchronized-mutable true})]
         clojure.lang.IPersistentCollection
         clojure.lang.Indexed
         clojure.lang.Sequential
         clojure.lang.ISeq
         clojure.lang.Seqable
         java.util.RandomAccess

         (first [_#]
           ~(first fields))
         (next [_#]
           ~(when (> cardinality 1)
              `(let [rst# ~'rst]
                 (if (nil? rst#)
                   (set! ~'rst (new ~dec-name ~@(rest fields) nil))
                   rst#))))
         (more [this##]
           (if-let [rst# (seq (next this##))]
             rst#
             '()))
         (cons [_# x#]
           (list x# ~@fields))
         (seq [this##]
           ~(when-not (zero? cardinality)
              `this##))
         
         (nth [_# idx# not-found#]
           (case idx#
             ~@(mapcat
                 (fn [idx field]
                   `(~idx ~field))
                 (range)
                 fields)
             not-found#))
         (nth [_# idx#]
           (case idx#
             ~@(mapcat
                 (fn [idx field]
                   `(~idx ~field))
                 (range)
                 fields)
             (throw (IndexOutOfBoundsException. (str idx#)))))
         
         (count [_] ~cardinality)
         
         (equiv [this# x##]
           (if (identical?
                 (if (nil? this#) nil (.getClass this#))
                 (if (nil? this#) nil (.getClass x##)))
             ~(if (zero? cardinality)
                true
                `(and
                   ~@(map
                       (fn [f]
                         `(Util/equiv ~f (. ~other ~f)))
                       fields)))
             (and (== ~cardinality (count x##))
               ~@(map
                   (fn [idx f]
                     `(Util/equiv ~f (nth x## ~idx)))
                   (range)
                   fields))))
         (equals [this# x##]
           (if (identical?
                 (if (nil? this#) nil (.getClass this#))
                 (if (nil? this#) nil (.getClass x##)))
             (and
               ~@(map
                   (fn [f]
                     `(Util/equals ~f (. ~other ~f)))
                   fields))
             (and (== ~cardinality (count x##))
               ~@(map
                   (fn [idx f]
                     `(Util/equals ~f (nth x## ~idx)))
                   (range)
                   fields))))
         (hashCode [_]
           ~(if (zero? cardinality)
              1
              (reduce
                (fn
                  ([form]
                     form)
                  ([form x]
                     `(+ (* 31 ~form) (Util/hash ~x))))
                1
                fields)))

         ~@(let [{:keys [major minor]} *clojure-version*]
             (when-not (and (= 1 major) (< minor 4))
               `(clojure.lang.IHashEq
                  (hasheq [_]
                    ~(if (zero? cardinality)
                       1
                       (reduce
                         (fn
                           ([form]
                              form)
                           ([form x]
                              `(+ (* 31 ~form) (Util/hasheq ~x))))
                         1
                         fields))))))))))

(defmacro ^:private def-tuple-n [name dec-name cardinality]
  (let [fields (map
                 #(symbol (str "e" %))
                 (range cardinality))
        other (with-meta `x## {:tag (str name)})]
    (unify-gensyms
      `(deftype ~name [~@fields ~(with-meta 'remainder {:tag "Collection"})]
         clojure.lang.IPersistentCollection
         clojure.lang.Indexed
         clojure.lang.Sequential
         clojure.lang.ISeq
         java.util.RandomAccess

         (first [_#]
           ~(first fields))
         (next [_#]
           (let [remainder# (rest ~'remainder)]
             (if (empty? remainder#)
               (new ~dec-name ~@(rest fields) (first ~'remainder) nil)
               (new ~name ~@(rest fields) (first ~'remainder) remainder#))))
         (more [this##]
           (next this##))
         (cons [_# x#]
           (list* x# ~@fields ~'remainder))
         (seq [this##]
           this##)
         
         (nth [_# idx# not-found#]
           (case idx#
             ~@(mapcat
                 (fn [idx field]
                   `(~idx ~field))
                 (range)
                 fields)
             (nth ~'remainder (- idx# ~cardinality) not-found#)))
         (nth [_# idx#]
           (case idx#
             ~@(mapcat
                 (fn [idx field]
                   `(~idx ~field))
                 (range)
                 fields)
             (try
               (nth ~'remainder (- idx# ~cardinality))
               (catch IndexOutOfBounds))
             (throw (IndexOutOfBoundsException. (str idx#)))))
         
         (count [_] (+ ~cardinality (count ~'remainder)))
         
         (equiv [this# x##]
           (if (identical?
                 (if (nil? this#) nil (.getClass this#))
                 (if (nil? this#) nil (.getClass x##)))
             (and
               ~@(map
                   (fn [f]
                     `(Util/equiv ~f (. ~other ~f)))
                   fields)
               (Util/equiv ~'remainder (.remainder ~other)))
             (and (== (count this#) (count x##))
               ~@(map
                   (fn [idx f]
                     `(Util/equiv ~f (nth x## ~idx)))
                   (range)
                   fields))))
         (equals [this# x##]
           (if (identical?
                 (if (nil? this#) nil (.getClass this#))
                 (if (nil? this#) nil (.getClass x##)))
             (and
               ~@(map
                   (fn [f]
                     `(Util/equals ~f (. ~other ~f)))
                   fields)
               (Util/equals ~'remainder (.remainder ~other)))
             (and (== (count this#) (count x##))
               ~@(map
                   (fn [idx f]
                     `(Util/equals ~f (nth x## ~idx)))
                   (range)
                   fields))))
         (hashCode [_]
           (let [seed# ~(reduce
                          (fn
                            ([form]
                               form)
                            ([form x]
                               `(+ (* 31 ~form) (Util/hash ~x))))
                          1
                          fields)
                 ^Iterator it# (.iterator ~'remainder)]
             (loop [hash# seed#]
               (if (.hasNext it#)
                 (recur (+ (* 31 hash#) (Util/hash (.next it#))))
                 (unchecked-int hash#)))))
         
         ~@(let [{:keys [major minor]} *clojure-version*]
             (when-not (and (= 1 major) (< minor 4))
               `(clojure.lang.IHashEq
                  (hasheq [_]
                    (let [seed# ~(reduce
                                   (fn
                                     ([form]
                                        form)
                                     ([form x]
                                        `(+ (* 31 ~form) (Util/hasheq ~x))))
                                   1
                                   fields)
                          ^Iterator it# (.iterator ~'remainder)]
                      (loop [hash# seed#]
                        (if (.hasNext it#)
                          (recur (+ (* 31 hash#) (Util/hasheq (.next it#))))
                          (unchecked-int hash#))))))))))))

(def-tuple Tuple0 nil 0)
(def-tuple Tuple1 Tuple0 1)
(def-tuple Tuple2 Tuple1 2)
(def-tuple Tuple3 Tuple2 3)
(def-tuple Tuple4 Tuple3 4)
(def-tuple-n TupleN Tuple4 4)

(defn tuple
  "Returns tuple structures that cannot be changed (even via immutable operators), which are highly efficient
   for index lookups, hash calculation, and equality checks.  If the arity is greater than 4, defaults to
   returning a list."
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
  ([x y z w & rst]
     (TupleN. x y z w rst)))
