(ns clj-tuple-test
  (:require
    [clojure.test :refer :all]
    [clj-tuple :as t]
    [criterium.core :as c]
    [collection-check :as check]
    [clojure.test.check.generators :as gen]
    [clojure.pprint :as pprint]))

(deftest test-pprint-able
  (is (= (with-out-str (pprint/pprint [1 2 3]))
        (with-out-str (pprint/pprint (t/vector 1 2 3)))))
  (is (= (with-out-str (pprint/pprint {1 2 3 4}))
        (with-out-str (pprint/pprint (t/hash-map 1 2 3 4))))))

(deftest test-equivalency
  (check/assert-vector-like 1e3 (t/vector) gen/int)
  (check/assert-map-like 1e3 (t/hash-map) gen/int gen/int))
