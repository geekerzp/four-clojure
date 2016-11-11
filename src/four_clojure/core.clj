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

;; Map Construction
;; https://www.4clojure.com/problem/61
(def map-construction
 (fn [a b]
   (apply assoc {} (interleave a b))))

;; Set Intersection
;; https://www.4clojure.com/problem/81
(def set-intersection
  (fn [a b]
    (set (filter a b))))

;; Comparisons
;; https://www.4clojure.com/problem/166
(def comparisons
  (fn [op a b]
    (cond
      (op a b) :lt
      (op b a) :gt
      :else :eq)))

;; Re-implement Iterate
;; http://www.4clojure.com/problem/62
(def re-implement-iterate
  (fn it [f x]
    (cons x (lazy-seq (it f (f x))))))

;; Re-implement Map
;; http://www.4clojure.com/problem/118
(def re-implement-map
  (fn mp [f s]
    (if (empty? s)
      nil
      (lazy-seq (cons (f (first s)) (mp f (rest s)))))))

;; Simple closures
;; http://www.4clojure.com/problem/107
(def simple-closures
  (fn [n]
    (fn [x]
      (int (Math/pow x n)))))

;; Greatest Common Divisor
;; http://www.4clojure.com/problem/66
(def greatest-common-divisor
  (fn euclid [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

;; Product Digits
;; http://www.4clojure.com/problem/99
(def product-digits
 (fn [a b]
   (->> (* a b)
        str
        (map str)
        (map #(Integer/parseInt %)))))
