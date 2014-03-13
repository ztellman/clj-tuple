(ns clj-tuple-test
  (:require
    [clojure.test :refer :all]
    [clj-tuple :refer :all]
    [criterium.core :as c]
    [collection-check :as check]
    [simple-check.generators :as gen]
    [clojure.pprint :as pprint])
  (:import
    [java.util.concurrent
     ConcurrentHashMap]
    [java.util
     HashMap]))

(deftest test-pprint-able
  (is (= (with-out-str (pprint/pprint [1 2 3]))
         (with-out-str (pprint/pprint (tuple 1 2 3))))))

(deftest test-equivalency
  (check/assert-vector-like 1e4 (tuple) gen/int))

(deftest ^:stress stress-test-equivalency
  (check/assert-vector-like 1e5 (tuple) gen/int))

(defmacro do-benchmark [description bench-form-fn]
  (let [bench-form-fn (eval bench-form-fn)]
    `(do
       ~@(map
           (fn [[type x]]
            `(do
               (println "\n  ** "~description ~type "\n")
               ~(bench-form-fn x)))
           (partition 2
             ["list 1"    '(list 1)
              "vector 1"  '(vector 1)
              "tuple 1"   '(tuple 1)
              "list 2"    '(list 1 2)
              "vector 2"  '(vector 1 2)
              "tuple 2"   '(tuple 1 2)
              "list 3"    '(list 1 2 3)
              "vector 3"  '(vector 1 2 3)
              "tuple 3"   '(tuple 1 2 3)
              "list 5"    '(list 1 2 3 4 5)
              "vector 5"  '(vector 1 2 3 4 5)
              "tuple 5"   '(tuple 1 2 3 4 5)
              "list 7"    '(list 1 2 3 4 5 6 7)
              "vector 7"  '(vector 1 2 3 4 5 6 7)
              "tuple 7"   '(tuple 1 2 3 4 5 6 7)
              ])))))

(deftest ^:benchmark benchmark-construction
  (do-benchmark "create" (fn [x] `(c/quick-bench ~x))))

(deftest ^:benchmark benchmark-addition
  (do-benchmark "apply +" (fn [x] `(c/quick-bench (apply + ~x)))))

(deftest ^:benchmark benchmark-reduce
  (do-benchmark "reduce +" (fn [x] `(c/quick-bench (reduce + ~x)))))

(deftest ^:benchmark benchmark-conj
  (do-benchmark "conj" (fn [x] `(let [x# ~x] (c/quick-bench (conj x# 1))))))

(deftest ^:benchmark benchmark-nth
  (do-benchmark "nth"
    (fn [x]
      `(let [idx# (dec (count ~x))
             x# ~x]
         (c/quick-bench (nth x# idx#))))))

(deftest ^:benchmark benchmark-first
  (do-benchmark "first" (fn [x] `(let [x# ~x] (c/quick-bench (first x#))))))

(deftest ^:benchmark benchmark-apply-variadic
  (do-benchmark "apply variadic"
    (fn [x]
      `(let [f# (fn [& args#] args#)
             x# ~x]
         (c/quick-bench (apply f# x#))))))

(deftest ^:benchmark benchmark-apply-fixed
  (do-benchmark "apply fixed"
    (fn [x]
      (let [cnt (count (eval x))]
        `(let [f# (fn [~@(repeatedly cnt gensym)] )
               x# ~x]
           (c/quick-bench (apply f# x#)))))))

(deftest ^:benchmark benchmark-compare
  (do-benchmark "compare"
    (fn [x]
      `(let [a# ~x
             b# ~x]
         (when-not (list? a#)
           (c/quick-bench (compare a# b#)))))))

(deftest ^:benchmark benchmark-seq
  (do-benchmark "seq"
    (fn [x]
      `(c/quick-bench (seq ~x)))))

(deftest ^:benchmark benchmark-hasheq
  (do-benchmark "hasheq" (fn [x] `(c/quick-bench (hash ~x)))))

(deftest ^:benchmark benchmark-hash
  (do-benchmark "hash" (fn [x] `(c/quick-bench (.hashCode ~(with-meta x {:tag "Object"}))))))

(deftest ^:benchmark benchmark-equality
  (do-benchmark "equals"
    (fn [x]
      `(let [a# ~x
             b# ~x]
         (c/quick-bench
           (.equals ^Object a# b#))))))

(deftest ^:benchmark benchmark-get-hash-map
  (do-benchmark "get hash-map"
    (fn [x]
      `(let [x# ~x
             ^HashMap m# (doto (HashMap.) (.put x# x#))]
         (c/quick-bench (.get m# ~x))))))

(deftest ^:benchmark benchmark-get-concurrent-hash-map
  (do-benchmark "get concurrent hash-map"
    (fn [x]
      `(let [x# ~x
             ^ConcurrentHashMap m# (doto (ConcurrentHashMap.) (.put x# x#))]
         (c/quick-bench (.get m# ~x))))))

(deftest ^:benchmark benchmark-get-persistent-hash-map
  (do-benchmark "get persistent hash-map"
    (fn [x]
      `(let [x# ~x
             m# {x# x#}]
         (c/quick-bench (get m# ~x))))))
