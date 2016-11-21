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

;; To Tree, or not to Tree
;; http://www.4clojure.com/problem/95
(def to-tree-or-not-to-tree
  (fn [r]
    (every?
     #(or
       (nil? %)
       (and (sequential? %) (= 3 (count %))))
     (tree-seq sequential? rest r)))
  )

;; Sum of square of digits
;; http://www.4clojure.com/problem/120
(def sum-of-sequare-of-digits
  (fn [s]
    (letfn [(digitize [n]
              (loop [n n
                     ret ()]
                (if (< n 1)
                  ret
                  (recur (quot n 10) (conj ret (rem n 10))))))
            (sum-of-sequare [s]
              (reduce + (map #(* % %) s)))]
      (count (filter (fn [[k v]] (< k v))
                     (zipmap s (map (comp sum-of-sequare digitize) s))))))
  )

;; Least Common Multiple
;; http://www.4clojure.com/problem/100
(def least-common-multiple
  (fn lcm
    ([x y]
     (letfn [(gcd [a b]
               (cond
                 (zero? b) a
                 (> a b) (gcd b (mod a b))
                 :else (gcd a (mod b a))))]
       (/ (* x y) (gcd x y))))
    ([x y & rest] (apply lcm (lcm x y) rest)))
  )

;; Pascal's Trapezoid
;; http://www.4clojure.com/problem/147
(def passcal-trapzoid
  (fn [s]
    (iterate #(map + (concat [0N] %) (concat % [0N])) s))
  )

;; Beauty is Symmetry
;; http://www.4clojure.com/problem/96
(def beauty-is-symmetry
  (fn [[_ l r]]
    (letfn [(mirror?
              [[lx ll lr :as l] [rx rl rr :as r]]
              (or (not (or l r))
                  (and (= lx rx)
                       (and (mirror? ll rr)
                            (mirror? lr rl)))))]
      (mirror? l r)))
  )

;; Recognize Playing Cards
;; http://www.4clojure.com/problem/128
(def recegnize-playing-cards
  (fn [s]
    (let [suit {\H :heart, \C :club, \D :diamond, \S :spades}
          rank {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5,
                \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]
      {:suit (suit (first s)), :rank (rank (second s))}))
  )

;; Split by Type
;; http://www.4clojure.com/problem/50
(def split-by-type
  #(vals (group-by type %))
  )

;; Trees into tables
;; http://www.4clojure.com/problem/146
(def trees-into-tables
  (fn [t]
    (into {} (for [[k v] t
                   [vk vv] v]
               [[k vk] vv])))
  )

;; Pairwise Disjoint Sets
;; http://www.4clojure.com/problem/153
(def parwise-disjoint-sets
  #(apply distinct? (mapcat seq %))
  )

;; Flipping out
;; http://www.4clojure.com/problem/46
(def flipping-out
  (fn [f]
    (fn [& args]
      (apply f (reverse args))))
  )

;; Rotate Sequence
;; http://www.4clojure.com/problem/44
(def rotate-sequence
  (fn [n coll]
    (let [part (split-at (mod n (count coll)) coll)]
      (concat (second part) (first part))))
  )

;; Count Occurrences
;; http://www.4clojure.com/problem/55
(def count-occurrences
  #(into {}
         (map (fn [[k v]] [k (count v)])
              (group-by identity %)))
  )
