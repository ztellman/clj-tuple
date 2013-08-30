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

(declare conj-tuple tuple)

(defn throw-arity [actual]
  `(throw
     (RuntimeException.
       ~(str "Wrong number of args (" actual ")"))))

(defmacro ^:private def-tuple [name dec-name cardinality]
  (let [fields (map
                 #(symbol (str "e" %))
                 (range cardinality))
        other (with-meta `x## {:tag (str name)})]
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
       
           clojure.lang.IFn
           ~@(map
             (fn [n]
               `(~'invoke [this# ~@(repeat n '_)]
                  ~(throw-arity n)))
             (remove #{1} (range 0 21)))
           
           (invoke [_ idx#]
             (case (int idx#)
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               (throw (IndexOutOfBoundsException. (str idx#)))))
       
           (applyTo [this## args#]
             (let [cnt# (count args#)]
               (case cnt#
                 1 (case (int (first args#))
                     ~@(mapcat
                         (fn [idx field]
                           `(~idx ~field))
                         (range)
                         fields)
                     (throw (IndexOutOfBoundsException. (str (first args#)))))
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
             (let [v# (case (int k##)
                        ~@(mapcat
                            (fn [idx field]
                              `(~idx ~field))
                            (range)
                            fields)
                        (throw (IndexOutOfBoundsException. (str k##))))]
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

           (nth [_ idx# not-found#]
             (case idx#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               not-found#))
           (nth [_ idx#]
             (case idx#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               (throw (IndexOutOfBoundsException. (str idx#)))))
           
           (equiv [this# x##]
             (if (identical?
                   (.getClass this#)
                   (if (nil? x##) nil (.getClass x##)))
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
                   (.getClass this#)
                   (if (nil? x##) nil (.getClass x##)))
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

(defmacro ^:private def-tuple-n [name dec-name cardinality]
  (let [fields (map
                 #(symbol (str "e" %))
                 (range cardinality))
        other (with-meta `x## {:tag (str name)})]
    (unify-gensyms
      `(do

         (deftype ~name [~@fields ~(with-meta 'remainder {:tag "java.util.Collection"}) mta##]
           
           clojure.lang.IObj
           (meta [_] mta##)
           (withMeta [_ m#] (new ~name ~@fields ~'remainder m#))
           
           java.util.concurrent.Callable
           (call [this##]
             (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))
           
           java.lang.Runnable
           (run [this##]
             (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))
       
           clojure.lang.IFn
           ~@(map
             (fn [n]
               `(~'invoke [this# ~@(repeat n '_)]
                  ~(throw-arity n)))
             (remove #{1} (range 0 21)))
           
           (invoke [_ idx#]
             (case (int idx#)
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               (try
                 (nth ~'remainder (- idx# ~cardinality))
                 (catch IndexOutOfBoundsException _#
                   (throw (IndexOutOfBoundsException. (str idx#)))))))
       
           (applyTo [this## args#]
             (let [cnt# (count args#)]
               (case cnt#
                 1 (let [idx# (int (first args#))]
                     (case idx#
                       ~@(mapcat
                           (fn [idx field]
                             `(~idx ~field))
                           (range)
                           fields)
                       (try
                         (nth ~'remainder (- idx# ~cardinality))
                         (catch IndexOutOfBoundsException _#
                           (throw (IndexOutOfBoundsException. (str idx#)))))))
                 (throw-arity cnt#))))

           clojure.lang.Associative
           clojure.lang.IPersistentVector
           (count [_] (+ ~cardinality (count ~'remainder)))
           (length [this#] (.count this#))
           
           (containsKey [_ k##]
             (and (number? k##)
               (or
                 (<= 0 k## ~(dec cardinality))
                 (<= ~cardinality k## (count ~'remainder)))))
           (entryAt [_ k#]
             (let [v# (case (int k#)
                        ~@(mapcat
                            (fn [idx field]
                              `(~idx ~field))
                            (range)
                            fields)
                        (try
                          (nth ~'remainder (- k# ~cardinality))
                          (catch IndexOutOfBoundsException _#
                            (throw (IndexOutOfBoundsException. (str k#))))))]
               (map-entry k# v#)))
           (assoc [this# k# v##]
             (case (int k#)
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) ~'remainder `mta##)))
                   (range)
                   fields)
               (try
                 (new ~name ~@fields (assoc (vec ~'remainder) (- (unchecked-long k#) ~cardinality) v##) mta##)
                 (catch IndexOutOfBoundsException _#
                   (throw (IndexOutOfBoundsException. (str k#)))))))
           (assocN [this# k# v##]
             (case k#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) ~'remainder `mta##)))
                   (range)
                   fields)
               (try
                 (new ~name ~@fields (assoc (vec ~'remainder) (- (unchecked-long k#) ~cardinality) v##) mta##)
                 (catch IndexOutOfBoundsException _#
                   (throw (IndexOutOfBoundsException. (str k#)))))))

           java.util.Collection
           (isEmpty [_] false)
           (iterator [_]
             (let [^Collection l# (list* ~@fields ~'remainder)]
               (.iterator l#)))
           (toArray [this#]
             (let [ary## (object-array (count this#))]
               ~@(map
                   (fn [idx field] `(aset ary## ~idx ~field))
                   (range)
                   fields)
               (loop [idx# ~cardinality, s# ~'remainder]
                 (when-not (empty? s#)
                   (aset ary## idx# (first s#))
                   (recur (inc idx#) (rest s#))))
               ary##))
           (size [this#] (.count this#))

           clojure.lang.IPersistentCollection
           clojure.lang.Indexed
           clojure.lang.Sequential
           clojure.lang.ISeq
           clojure.lang.Seqable

           (first [_]
             ~(first fields))
           (next [this##]
             (let [rst# (rest ~'remainder)]
               (if (empty? rst#)
                 (new ~dec-name ~@(rest fields) (first ~'remainder) nil)
                 (new ~name ~@(rest fields) (first ~'remainder) rst# nil))))
           (more [this##]
             (if-let [rst# (next this##)]
               rst#
               '()))
           (cons [this# k#]
             (new ~name ~@fields (conj (vec ~'remainder) k#) nil))
           (peek [_]
             (last ~'remainder))
           (pop [_]
             (let [rst# (pop (vec ~'remainder))]
               (if (empty? rst#)
                 (new ~dec-name ~@fields nil)
                 (new ~name ~@fields (pop (vec ~'remainder)) nil))))
           (rseq [_]
             (apply tuple (reverse (list* ~@fields ~'remainder))))
           (seq [this#]
             this#)

           (nth [_ idx# not-found#]
             (case idx#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               (nth ~'remainder (- idx# ~cardinality) not-found#)))
           (nth [_ idx#]
             (case idx#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx ~field))
                   (range)
                   fields)
               (try
                 (nth ~'remainder (- idx# ~cardinality))
                 (catch IndexOutOfBoundsException _#
                   (throw (IndexOutOfBoundsException. (str idx#)))))))
           
           (equiv [this# x##]
             (if (identical?
                   (.getClass this#)
                   (if (nil? x##) nil (.getClass x##)))
               ~(if (zero? cardinality)
                  true
                  `(and
                     ~@(map
                         (fn [f]
                           `(Util/equiv ~f (. ~other ~f)))
                         fields)
                     (Util/equiv ~'remainder (.remainder ~other))))
               (and (== (count this#) (count x##))
                 ~@(map
                     (fn [idx f]
                       `(Util/equiv ~f (nth x## ~idx)))
                     (range)
                     fields)
                 (Util/equiv ~'remainder (drop ~cardinality x##)))))
           
           (equals [this# x##]
             (if (identical?
                   (.getClass this#)
                   (if (nil? x##) nil (.getClass x##)))
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
                     fields)
                 (Util/equals ~'remainder (drop ~cardinality x##)))))

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
           
           clojure.lang.IHashEq
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
                   (unchecked-int hash#)))))

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
           (reduce f##
             ~(reduce
                (fn [form field]
                  `(f## ~form ~field))
                (first fields)
                (rest fields))
             ~'remainder))
         (coll-reduce [_ f## start##]
           (reduce f##
             ~(reduce
                (fn [form field]
                  `(f## ~form ~field))
                `start##
                fields)
             ~'remainder))


         (toString [_]
           (str "["
             ~@(->> fields (map (fn [f] `(pr-str ~f))) (interpose " "))
             (->> ~'remainder
               (map (fn [x#] (pr-str x#)))
               (interpose " ")
               (apply str))
             "]")))))))

(def-tuple Tuple0 nil 0)
(def-tuple Tuple1 Tuple0 1)
(def-tuple Tuple2 Tuple1 2)
(def-tuple Tuple3 Tuple2 3)
(def-tuple Tuple4 Tuple3 4)
(def-tuple Tuple5 Tuple4 5)
(def-tuple Tuple6 Tuple5 6)
(def-tuple-n TupleN Tuple6 6)

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
               (range 6)))))))

(defn tuple
  "Returns a tuple which behaves like a list, but is highly efficient for index lookups, hash
   calculations, equality checks, and reduction.  If there are more than six elements, an
   unbounded tuple is used which is comparable in performance to a normal list."
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
     (TupleN. x y z w u v rst nil)))
