(ns four-clojure.core)

;; Drop Every Nth Element
;; http://www.4clojure.com/problem/41
(def drop-every-nth-element
 (fn [coll n]
   (mapcat #(take (dec n) %) (partition-all n coll))))

;; Split a sequence
;; http://www.4clojure.com/problem/49
(def split-a-sequence
 (fn [n coll]
   [(take n coll) (drop n coll)]))

(juxt take drop)

;; Advanced Destructuring
;; https://www.4clojure.com/problem/
(def advanced-destructuring
  [1 2 3 4 5])

;; A Half-Truth
;; https://www.4clojure.com/problem/83
(def a-half-truth
 (fn [& args]
   (and (boolean (some true? args)) (not-every? true? args))))

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

;; Cartesian Product
;; http://www.4clojure.com/problem/90
(def cartesian-product
 (fn [s1 s2]
   (into #{} (for [s1 s1
                   s2 s2]
               [s1 s2])))
  )

;; Group a Sequence
;; http://www.4clojure.com/problem/63
(def group-a-sequence
 (fn [f s]
   (apply merge-with concat (map #(hash-map (f %) [%]) s)))
  )

;; Symmetic Difference
;; http://www.4clojure.com/problem/63
(def symmetic-difference
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a)))
  )

;; dot product
;; http://www.4clojure.com/problem/143
(def dot-product
  #(reduce + (map * %1 %2))
  )

;; Read a binary number
;; http://www.4clojure.com/problem/122
(def read-a-binary-number
  #(Integer/valueOf % 2)
  )

;; Through the Looking Class
;; http://www.4clojure.com/problem/126
Class

;; Infix Calculator
;; http://www.4clojure.com/problem/135
(def infix-calculator
  (fn [& expr]
    (loop [[x & xs] expr acc 0 op +]
      (cond
        (nil? x) acc
        (number? x) (recur xs (op acc x) nil)
        :else (recur xs acc x))))
  )

;; Indexing Sequences
;; http://www.4clojure.com/problem/157
(def indexing-sequences
  ;; #(map vector % (range))
  #(map-indexed vector %)
  )

;; Pascal's Triangle
;; http://www.4clojure.com/problem/97
(def pascal-triangle
  (fn [n]
    (nth (iterate #(map + (concat [0] %) (concat % [0])) [1]) (dec n)))
  )
