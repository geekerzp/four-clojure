(ns four-clojure.core)

;; Drop Every Nth Element
;; http://www.4clojure.com/problem/41
(fn [coll n]
  (mapcat #(take (dec n) %) (partition-all n coll)))

;; Split a sequence
;; http://www.4clojure.com/problem/49
(fn [n coll]
  [(take n coll) (drop n coll)])

(juxt take drop)
