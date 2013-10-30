(ns clj-tuple
  (:require
    [clojure.core.protocols :as p])
  (:import
    [clojure.lang
     Util
     IMapEntry
     MapEntry]
    [java.util
     List
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
         (symbol (str (gensym* (str (un-gensym %) "__")) "__unified__"))
         %)
      body)))

;;;

(declare conj-tuple tuple)

(defn- throw-arity [actual]
  (throw
    (RuntimeException.
      (str "Wrong number of args (" actual ")"))))

;; specific cardinality tuples
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

           clojure.lang.IEditableCollection
           (asTransient [_]
             (-> []
               transient
               ~@(map
                   (fn [x] `(conj! ~x))
                   fields)))

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
             (when-let [v# ~(lookup `(int k##))]
               (MapEntry. k## v#)))
           (assoc [this# k# v##]
             (case (int k#)
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) mta##)))
                   (range)
                   fields)
               ~cardinality (conj-tuple this# v##)
               (throw (IndexOutOfBoundsException. (str k#)))))
           (assocN [this# k# v##]
             (case k#
               ~@(mapcat
                   (fn [idx field]
                     `(~idx (new ~name ~@(-> fields vec (assoc idx `v##)) mta##)))
                   (range)
                   fields)
               ~cardinality (conj-tuple this# v##)
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
           java.util.List

           (empty [_]
             (tuple))
           (first [_]
             ~(first fields))
           (next [this##]
             ~(when (> cardinality 1)
                `(new ~dec-name ~@(rest fields) mta##)))
           (more [this##]
             (if-let [rst# (next this##)]
               rst#
               '()))
           (cons [this# k#]
              (conj-tuple this# k#))
           (peek [_]
             ~(last fields))
           (pop [_]
             ~(if (zero? cardinality)
                `(throw (IllegalArgumentException. "Cannot pop from an empty vector."))
                `(new ~dec-name ~@(butlast fields) mta##)))
           (rseq [_]
             (new ~name ~@(reverse fields) mta##))
           (seq [this##]
             ~(when-not (zero? cardinality)
                `this##))

           (nth [_ idx## not-found##]
             ~(lookup `idx## `not-found##))
           (nth [_ idx##]
             ~(lookup `idx##))
           (get [_ idx##]
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
               (and
                 (sequential? x##)
                 (== ~cardinality (count x##))
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
               (and
                 (sequential? x##)
                 (== ~cardinality (count x##))
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

           ~@(let [reduce-form (fn [val elements]
                                 `(let [x## ~val]
                                    ~(reduce
                                       (fn [form field]
                                         `(let [x## (f## x## ~field)]
                                            (if (reduced? x##)
                                              @x##
                                              ~form)))
                                       `x##
                                       (reverse elements))))]
               `(p/InternalReduce
                  (internal-reduce [_ f## start##]
                    ~(if (zero? cardinality)
                       `start##
                       (reduce-form `start## fields)))
                  
                  p/CollReduce
                  (coll-reduce [_ f##]
                    ~(if (zero? cardinality)
                       `(f##)
                       (reduce-form (first fields) (rest fields))))
                  (coll-reduce [_ f## start##]
                    ~(if (zero? cardinality)
                       `start##
                       (reduce-form `start## fields)))))
           
           (toString [_]
             (str "[" ~@(->> fields (map (fn [f] `(pr-str ~f))) (interpose " ")) "]")))

         (defmethod print-method ~name [o# ^java.io.Writer w#]
           (.write w# (str o#)))

         (defmethod print-method ~name [o# ^java.io.Writer w#]
           (.write w# (str o#)))))))

;; hacky workaround for the fact that PersistentVector isn't an ISeq, so we can't cleanly
;; transition via conj from a tuple to a vector.
(let [lookup (fn this
               ([idx]
                  `(let [idx# ~idx]
                     (if (< -1 idx# (- end## start##))
                       (.nth v## (+ start## idx#))
                       (throw (IndexOutOfBoundsException. (str idx#))))))
               ([idx default]
                  `(let [idx# ~idx
                         default# ~default]
                     (if (< -1 idx# (- end## start##))
                       (.nth v## (+ start## idx#))
                       default#))))]
  (eval
    (unify-gensyms
      `(deftype ~'VectorSeq
        [^long start## ^long end## ^clojure.lang.PersistentVector v## mta##]
       
        clojure.lang.IObj
        (meta [_] mta##)
        (withMeta [_ m#] (new ~'VectorSeq start## end## v## m#))
       
        java.util.concurrent.Callable
        (call [this##]
          (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))
           
        java.lang.Runnable
        (run [this##]
          (.invoke ~(with-meta `this## {:tag "clojure.lang.IFn"})))

        clojure.lang.ILookup
        (valAt [_ k##] ~(lookup `(int k##)))
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

        clojure.lang.IEditableCollection
        (asTransient [_]
          (loop [v# (transient []), idx# start##]
            (if (== end## idx#)
              v#
              (recur (conj! v# (.nth v## idx#)) (inc idx#)))))

        clojure.lang.Associative
        clojure.lang.IPersistentVector
        (count [this#] (- end## start##))
        (length [this#] (- end## start##))
           
        (containsKey [_ k#]
          (< -1 k# (- end## start##)))
        (entryAt [_ k##]
          (when-let [v# ~(lookup `(int k##))]
            (MapEntry. k## v#)))
        (assoc [this# k# v##]
          (.assocN this# (int k#) v##))
        (assocN [this# k# v#]
          (new ~'VectorSeq start## end##
            (.assocN ~(with-meta `v## {:tag "clojure.lang.IPersistentVector"})
              (int (+ start## k#))
              v#)
            mta##))

        java.util.Collection
        (isEmpty [_] (== 0 (- end## start##)))
        (iterator [_]
          (let [^Collection l# (->> v##
                                 seq
                                 (drop start##)
                                 (take (- end## start##)))]
            (.iterator l#)))
        (toArray [_]
          (->> v##
            seq
            (drop start##)
            (take (- end## start##))
            to-array))
        (size [_] (- end## start##))

        clojure.lang.IPersistentCollection
        clojure.lang.Indexed
        clojure.lang.Sequential
        clojure.lang.ISeq
        clojure.lang.Seqable
        java.util.List

        (seq [this#] this#)
        (empty [_]
          (with-meta [] mta##))
        (first [_]
          (when (< 0 (- end## start##))
            (.nth v## start##)))
        (next [this##]
          (when (< 1 (- end## start##))
            (new ~'VectorSeq (inc start##) end## v## mta##)))
        (more [this##]
          (if-let [rst# (next this##)]
            rst#
            '()))
        (cons [this# k#]
          (new ~'VectorSeq start## (inc end##)
            (.assocN ~(with-meta `v## {:tag "clojure.lang.IPersistentVector"}) end## k#)
            mta##))
        (peek [_]
          (when (< 0 (- end## start##))
            (.nth v## end##)))
        (pop [_]
          (if (< 0 (- end## start##))
            (new ~'VectorSeq start## (dec end##) v## mta##)
            (throw (IllegalArgumentException. "Cannot pop from an empty vector."))))

        (nth [_ idx## not-found##]
          ~(lookup `idx## `not-found##))
        (nth [_ idx##]
          ~(lookup `idx##))
        (get [_ idx##]
          ~(lookup `idx##))
           
        (equiv [this# x##]
          (and
            (sequential? x##)
            (== (count this#) (count x##))
            (loop [idx# start##, x# x##]
              (if (== end## idx#)
                true
                (if (and x# (Util/equiv (.nth v## idx#) (first x#)))
                  (recur (inc idx#) (rest x#))
                  false)))))
           
        (equals [this# x##]
          (and
            (sequential? x##)
            (== (count this#) (count x##))
            (loop [idx# start##, x# x##]
              (if (== end## idx#)
                true
                (if (and x# (Util/equals (.nth v## idx#) (first x#)))
                  (recur (inc idx#) (rest x#))
                  false)))))

        Comparable
        (compareTo [this# x##]
          (let [cnt# (count x##)]
            (if (== (- end## start##) cnt#)
              (- (compare x## this#))
              (- (- end## start##) cnt#))))
           
        (hashCode [_]
          (loop [idx# start##, h# 1]
            (if (== end## idx#)
              h#
              (recur (inc idx#) (+ (* 31 h#) (Util/hash (.nth v## idx#)))))))
           
        clojure.lang.IHashEq
        (hasheq [_]
          (loop [idx# start##, h# 1]
            (if (== end## idx#)
              h#
              (recur (inc idx#) (+ (* 31 h#) (Util/hasheq (.nth v## idx#)))))))))))


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
                        (meta t##)))))
               (range 6))
           6 (let [^clojure.lang.Indexed t## t##]
               (VectorSeq. 0 7
                 (-> []
                   transient
                   (conj! (.nth t## 0))
                   (conj! (.nth t## 1))
                   (conj! (.nth t## 2))
                   (conj! (.nth t## 3))
                   (conj! (.nth t## 4))
                   (conj! (.nth t## 5))
                   (conj! (.nth t## 6))
                   x##
                   persistent!)
                 (meta t##)))
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
