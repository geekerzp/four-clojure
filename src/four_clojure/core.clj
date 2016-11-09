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

;; Advanced Destructuring
;; https://www.4clojure.com/problem/51
[1 2 3 4 5]

;; A Half-Truth
;; https://www.4clojure.com/problem/83
(fn [& args]
  (and (boolean (some true? args)) (not-every? true? args)))
