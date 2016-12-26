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

;; Reverse Interleave
;; http://www.4clojure.com/problem/43
(def reverse-interleave
  (fn [coll n]
    (apply map list (partition n n coll)))
  )

;; Function Composition
;; http://www.4clojure.com/problem/58
(def function-composition
  (fn [& fs]
    (reduce (fn [f g]
              #(f (apply g %&))) fs))
  )

;; Find Distinct Items
;; http://www.4clojure.com/problem/56
(def find-distinct-items
  #(reduce (fn [acc n]
             (if (some #{n} acc)
               acc
               (conj acc n)))
           [] %)
  )

;; Partition a Sequence
;; http://www.4clojure.com/problem/54
(def partition-sequence
  (fn [n s]
    (loop [m 0
           ret '()]
      (if (> (+ m n) (count s))
        (reverse ret)
        (recur (+ n m) (conj ret (take n (drop m s)))))))
  )

;; Word Sorting
;; http://www.4clojure.com/problem/70
(def word-sorting
  (fn [s]
    (let [words (re-seq #"\w+" s)]
      (sort-by clojure.string/lower-case words)))
  )

;; Juxtaposition
;; http://www.4clojure.com/problem/59
(def juxtaposition
  (fn [& fs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fs)))
  )

;; Prime Numbers
;; http://www.4clojure.com/problem/67
(def prime-numbers
  (fn [x]
    (take x
          (remove
           (fn [n]
             (some #(= 0 (mod n %))
                   (range 2 (inc (int (Math/sqrt n))))))
           (iterate inc 2))))
  )

;; Black Box Testing
;; http://www.4clojure.com/problem/65
(def black-box-testing
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (reversible? base) :vector
        (= base '()) :list)))
  )

;; Filter Perfect Squares
;; http://www.4clojure.com/problem/74
(def filter-perfect-squares
  (fn [s]
    (->> s
         (#(clojure.string/split % #","))
         (map read-string)
         (filter #(== (Math/sqrt %) (int (Math/sqrt %))))
         (clojure.string/join ","))
    )
  )

;; Perfect Numbers
;; http://www.4clojure.com/problem/80
(def perfect-numbers
  (fn [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n)))))
  )

;; intoCamelCase
;; http://www.4clojure.com/problem/102
(def into-camel-case
  (fn [s]
    (clojure.string/join
     (let [words (clojure.string/split s #"-")]
       (cons (first words) (map #(clojure.string/capitalize %) (rest words))))))
  )

;; Anagram Finder
;; http://www.4clojure.com/problem/77
(def anagram-finder
  (fn [s]
    (into #{}
          (map set
               (filter #(> (count %) 1)
                       (vals (group-by sort s))))))
  )

;; Merge with a Function
;; http://www.4clojure.com/problem/69
(def merge-with-function
  (fn [f & maps]
    (reduce
     (fn [m1 m2]
       (reduce
        (fn [m [k v]]
          (assoc m k (if (m1 k) (f (m1 k) v) v)))
        m1 m2))
     (first maps) (rest maps)))
  )

;; Happy numbers
;; http://www.4clojure.com/problem/86
(def happy-numbers
  (fn [n]
    (letfn [(digitize [n]
              (loop [n n
                     ret ()]
                (if (< n 1)
                  ret
                  (recur (quot n 10) (conj ret (rem n 10))))))
            (change [n]
              (reduce + (map #(* % %) (digitize n))))]
      (loop [n n
             ret #{}]
        (cond
          (= 1 n) true
          (ret n) false
          :else (recur (change n) (conj ret n))))))
  )

;; Sequence Reductions
;; http://www.4clojure.com/problem/60
(def sequence-reductions
  (fn my-reductions
    ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (my-reductions f (first s) (rest s))
        (list (f)))))
    ([f init coll]
     (cons init
           (lazy-seq
            (when-let [s (seq coll)]
              (my-reductions f (f init (first s)) (rest s)))))))
  )

;; Power Set
;; http://www.4clojure.com/problem/85
(def power-set
  (fn [s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) (list #{}) s)))
  )

;; Intro to Trampoline
;; http://www.4clojure.com/problem/76
(def intro-to-trampoline
  [1 3 5 7 9 11])

;; Longest Increasing Sub-Seq
;; http://www.4clojure.com/problem/53
(def longest-increasing-subseq
  (fn [coll]
    (->> (map vector coll (range))
         (partition-by #(apply - %))
         (map #(map first %))
         (filter #(> (count %) 1))
         (sort-by (comp - count))
         first
         vec))
  )

;; Euler's Totient Function
;; http://www.4clojure.com/problem/75
(def totient-function
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b]
                (cond
                  (zero? b) a
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))]
        (count (filter #{1} (map (partial gcd n) (range 1 n)))))))
  )

;; Identify keys and values
;; http://www.4clojure.com/problem/105
(def identity-keys-and-values
  (fn [s]
    (loop [s s ret {}]
      (if (empty? s)
        ret
        (recur (second (split-with number? (rest s)))
               (assoc ret (first s) (first (split-with number? (rest s))))))))
  )

;; Reimplement Trampoline
;; http://www.4clojure.com/problem/78
(def reimplement-trampoline
  (fn my-trampoline
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (my-trampoline #(apply f args))))
  )

;; The Balance of N
;; http://www.4clojure.com/problem/115
(def balance-number
  (fn [n]
    (let [s (str n), l (count s)]
      (= (reduce + (map int (take (Math/ceil (/ l 2)) s)))
         (reduce + (map int (drop (Math/floor (/ l 2)) s))))))
  )

;; Equivalence Classes
;; http://www.4clojure.com/problem/98
(def equivalence-classes
  (fn [f s]
    (set (map set (vals (group-by f s)))))
  )

;; Digits and bases
;; http://www.4clojure.com/problem/137
(def digits-and-bases
  (fn [n b]
    (if (zero? n)
      '(0)
      (loop [n n ret '()]
        (if (< n 1)
          ret
          (recur (quot n b) (conj ret (rem n b)))))))
  )

;; Oscilrate
;; http://www.4clojure.com/problem/144
(def oscilrate
  #(reductions (fn [v f] (f v)) %1 (cycle %&))
  )

;; Decurry
;; http://www.4clojure.com/problem/158
(def decurry
  (fn [f]
    (fn [& args]
      (reduce #(%1 %2) f args)))
  )

;; Lazy Searching
;; http://www.4clojure.com/problem/108
(def lazy-searching
  (fn [& xs]
    (if (apply = (map first xs))
      (ffirst xs)
      (let [smallest (apply min (map first xs))]
        (recur (map #(if (= smallest (first %)) (drop 1 %) %) xs)))))
  )

;; Partially Flatten a Sequence
;; http://www.4clojure.com/problem/93
(def partially-flatten-sequence
  (fn [x]
    (filter #(every? (complement sequential?) %)
            (filter sequential?
                    (rest (tree-seq sequential? seq x)))))
  )

;; Global take-while
;; http://www.4clojure.com/problem/114
(def global-take-while
  (fn _ [n p [x & xs]]
    (let [n (if (p x) (dec n) n)]
      (if (zero? n)
        '()
        (lazy-seq (cons x (_ n p xs))))))
  )

;; Insert between two items
;; http://www.4clojure.com/problem/132
(def insert-between-two-items
  (fn [pred val coll]
    (->> (partition 2 1 coll)
         (mapcat (fn [[x y]] (if (pred x y) [val y] [y])))
         (cons (first coll))
         (filter (complement nil?))
         ))
  )

;; Sequence of pronunciations
;; http://www.4clojure.com/problem/110
(def sequence-of-pronunciations
  (fn [s]
    (rest
     (iterate
      #(flatten
        (map (fn [v] (vector (count v) (first v)))
             (partition-by identity %))) s)))
  )

;; Write Roman Numerals
;; http://www.4clojure.com/problem/104
(def write-roman-numberals
  (fn [n]
    (let [rmap (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                           90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [s "" n n]
        (if (zero? n)
          s
          (let [[arabic roman] (last (filter #(>= n (first %)) rmap))]
            (recur (str s roman) (- n arabic)))))))
  )

;; Generating k-combinations
;; http://www.4clojure.com/problem/103
(def generating-k-combinations
  (fn [k s]
    (set (filter #(= k (count %))
                 (reduce #(concat %1 (map (fn [x] (set (conj x %2))) %1))
                         #{#{}} s))))
  )

;; Intervals
;; http://www.4clojure.com/problem/171
(def intervals
  (fn [c]
    (->> (map list (sort (set c)) (range))
         (partition-by #(apply - %))
         (map #(list (ffirst %) (first (last %))))))
  )

;; Sequs Horribilis
;; http://www.4clojure.com/problem/112
(def sequs-horribilis
  (fn _ [n [x & xs]]
    (cond
      (nil? x) '()
      (coll? x) (let [s (_ n x)]
                  (cons s (_ (- n (reduce + (flatten s))) xs)))
      (<= x n) (cons x (_ (- n x) xs))
      :e '())))

;; Prime Sandwich
;; http://www.4clojure.com/problem/116
(def prime-sandwich
  (fn [n]
    (letfn [(prime? [x]
              (not-any? #(zero? (mod x %))
                        (range 2 (inc (int (Math/sqrt x))))))
            (pre-pri [x]
              (if (prime? x)
                x
                (pre-pri (dec x))))
            (next-pri [x]
              (if (prime? x)
                x
                (next-pri (inc x))))]
      (and
       (> n 2)
       (prime? n)
       (= n (/ (+ (pre-pri (dec n)) (next-pri (inc n))) 2)))))
  )

;; Universal Computation Engine
;; http://www.4clojure.com/problem/121
(def universal-computation-engine
  (fn [s]
    (fn [m]
      (letfn [(calc [[op & args]]
                (apply ({'+ + '- - '* * '/ /} op)
                       (map #(if (coll? %) (calc %) (m % %)) args)))]
        (calc s))))
  )

;; The Big Divide
;; http://www.4clojure.com/problem/148
(def big-divide
  (fn [n a b]
    (let [g #(quot (- n 1N) %)
          f #(/ (* % (inc (g %)) (g %)) 2)
          x (f a)
          y (f b)
          z (f (* a b))]
      (- (+ x y) z)))
  )

;; Balancing Brackets
;; http://www.4clojure.com/problem/177
(def balancing-brackets
  (fn [s]
    (empty?
     (reduce
      #(cond
         (#{\( \[ \{} %2) (cons %2 %)
         (= \) %2) (if (= \( (first %)) (rest %) (concat % (list false)))
         (= \] %2) (if (= \[ (first %)) (rest %) (concat % (list false)))
         (= \} %2) (if (= \{ (first %)) (rest %) (concat % (list false)))
         :else (do (println %2) %))
      ()
      s)))
  )

;; Sum Some Set Subsets
;; http://www.4clojure.com/problem/131
(def sum-some-set-subsets
  (letfn [(power-set [s]
            (reduce
             (fn [r e] (into r (map #(conj % e) r)))
             #{#{}} s))
          (sans-null-set [s]
            (disj (power-set s) #{}))]
    (fn [& xs]
      (->> xs
           (map (comp set (partial map #(reduce + %)) sans-null-set))
           (apply clojure.set/intersection)
           not-empty
           boolean)
      ))
  )

;; Palindromic Numbers
;; http://www.4clojure.com/problem/150
(def palindromic-numbers
  (fn [x]
    (letfn [(n2d [n]
              (loop [n n ret '()]
                (if (zero? n) ret
                    (recur (quot n 10) (conj ret (rem n 10))))))

            (d2n [xs]
              (apply + (map * (reverse xs) (iterate (partial * 10) 1))))

            (count-up [x]
              (+ x (nth (iterate (partial * 10) 1) (/ (count (n2d x)) 2))))

            (pnumber [x]
              (let [s (n2d x)
                    n (count s)
                    [c _] (split-at (/ n 2) s)]
                (d2n
                 (cond
                   (= s (reverse s)) s
                   (odd? n) (concat c (rest (reverse c)))
                   (even? n) (concat c (reverse c))))))

            (pnumber-seq [x]
              (lazy-seq
               (cons (pnumber x) (pnumber-seq (count-up x)))))]
      (drop-while #(< % x) (pnumber-seq x))))
  )

;; Tricky card games
;; http://www.4clojure.com/problem/141
(def tricky-card-games
  (fn [trump]
    (fn [trick]
      (let [lead-suit (if trump trump (:suit (first trick)))]
        (last (sort-by :rank (filter #(= lead-suit (:suit %)) trick))))))
  )

;; Infinite Matrix
;; http://www.4clojure.com/problem/168
(def infinite-matrix
  (fn infinite-matrix
    ([f]
     (infinite-matrix f 0 0))
    ([f m n]
     (letfn [(inner [i j]
               (lazy-seq (cons (f i j) (inner i (inc j)))))
             (outer [i]
               (lazy-seq (cons (inner i n) (outer (inc i)))))]
       (outer m)))
    ([f m n s t]
     (take s (map #(take t %) (infinite-matrix f m n)))))
  )

;; Parentheses... Again
;; http://www.4clojure.com/problem/195
(def parentheses-again
  (fn parentheses
    ([n] (parentheses n n))
    ([left right]
     (if (and (zero? left) (zero? right))
       #{""}
       (clojure.set/union
        (when (> left 0)
          (set (map #(str "(" % ) (parentheses (dec left) right))))
        (when (> right left)
          (set (map #(str ")" % ) (parentheses left (dec right)))))))))
  )

;; Read Roman numerals
;; http://www.4clojure.com/problem/92
(def read-roman-numerals
  (fn [s]
    (let [roman {"M" 1000
                 "CM" 900
                 "D"  500
                 "CD" 400
                 "C"  100
                 "XC"  90
                 "L"   50
                 "XL"  40
                 "X"   10
                 "IX"   9
                 "V"    5
                 "IV"   4
                 "I"    1}]
      (reduce +
              (map roman
                   (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s)))))
  )

;; Analyze a Tic-Tac-Toe Board
;; http://www.4clojure.com/problem/73
(def analyze-tic-tac-toe-board
  (fn [[[a b c :as f] [d e f :as s] [g h i :as t]]]
    (some {[:o :o :o] :o [:x :x :x] :x}
          [f s t [a d g] [b e h] [c f i] [a e i] [c e g]]))
  )

;; Triangle Minimal Path
;; http://www.4clojure.com/problem/79
(def triangle-minimal-path
  (fn [s]
    (first
     (reduce
      #(map + (map min (butlast %1) (rest %1)) %2)
      (reverse s))))
  )

;; Transitive Closure
;; http://www.4clojure.com/problem/84
(def transitive-closure
  (fn [s]
    (letfn [(infer [s]
              (set
               (concat s (for [[a b] s
                               [d e] s
                               :when (= b d)]
                           [a e]))))]
      (loop [s1 s s2 (infer s)]
        (if (= s1 s2) s1
            (recur s2 (infer s2))))))
  )

;; Word Chains
;; http://www.4clojure.com/problem/82
(def word-chains
  (fn [s]
    (letfn [(diff-one-char? [w1 w2]
              (cond
                (= (count w1) (count w2)) (= 1 (count
                                                (filter false?
                                                        (map #(= % %2) w1 w2))))
                :else (let [max-w (max-key count w1 w2)
                            min-w (min-key count w1 w2)]
                        (some #(= (seq min-w) %)
                              (map #(concat (take % max-w) (drop (inc %) max-w))
                                   (range (count max-w)))))))

            (chain? [ws]
              (every? true?
                      (map (partial apply diff-one-char?) (partition 2 1 ws))))

            (permutations [s]
              (lazy-seq
               (if (seq (rest s))
                 (apply concat (for [x s]
                                 (map #(cons x %) (permutations (remove #{x} s)))))
                 [s])))]
      (boolean (some chain? (permutations s)))))
  )

;; Graph Connectivity
;; http://www.4clojure.com/problem/91
(def graph-connectivity
  (fn [s]
    (apply = (vals (reduce
                    (fn [g [a b]]
                      (let [r (clojure.set/union (g a #{a}) (g b #{b}))]
                        (reduce #(assoc % %2 r) g r)))
                    {} s))))
  )

;; Game of Life
;; http://www.4clojure.com/problem/94
(def game-of-life
  (fn [b]
    (letfn [(n [r c]
              (for [i (range -1 2)
                    j (range -1 2)
                    :when (not= 0 i j)]
                (get-in b [(+ r i) (+ c j)])))]
      (let [r (count b) c (count (first b))]
        (->>(for [i (range r)
                  j (range c)]
              (let [l (count (filter #(= \# %) (n i j)))]
                (cond
                  (and (= l 2)
                       (= \# (get-in b [i j]))) \#
                  (= l 3) \#
                  :else \space)))
            (partition c)
            (map #(apply str %))))))
  )

;; Number Maze
;; http://www.4clojure.com/problem/106
(def number-maze
  (fn [s e]
    (letfn [(f [xs]
              (lazy-cat [xs]
                        (f (clojure.set/union
                            (set (map #(+ % 2) xs))
                            (set (map #(* % 2) xs))
                            (set (map #(/ % 2) (filter even? xs)))))))]
      (->> (f #{s})
           (take-while #(not (contains? % e)))
           count
           inc)))
  )

;; Levenshtein Distance
;; http://www.4clojure.com/problem/101
(def levenshtien-distance
  (fn lev [[h & t :as a] [f & r :as b]]
    (cond (nil? h) (count b)
          (nil? f) (count a)
          (= f h) (recur t r)
          :else (min (inc (lev t r)) ;; replace
                     (inc (lev a r)) ;; insert
                     (inc (lev t b)) ;; delete
                     )))
  )

;; Graph Tour
;; http://www.4clojure.com/problem/89
(def graph-tour
  (fn [g]
    (letfn [(d [g]
              (apply merge-with + {} (for [[a b] g
                                           :when (not= a b)]
                                       {a 1 b 1})))]
      (and
       (not (empty? (d g)))
       (->> (vals (d g)) (filter odd?) count (>= 2)))))
  )

;; Win at Tic-Tac-Toe
;; http://www.4clojure.com/problem/119
(def win-at-tic-tac-toe
  (fn [p b]
    (let [free (for [x (range 3)
                     y (range 3)
                     :when (= :e (get-in b [x y]))]
                 [x y])
          lines #(concat % (apply map vector %)
                         [(map get % [0 1 2])]
                         [(map get % [2 1 0])])
          win? (fn [[x y]]
                 (let [nb (assoc-in b [x y] p)]
                   (some #(= [p p p] %) (lines nb))))]
      (set (filter win? free))))
  )

;; Making Data Dance
;; http://www.4clojure.com/problem/113
(def making-data-dance
  (fn [& xs]
    (reify
      clojure.lang.Seqable
      (toString [_] (apply str (interpose ", " (sort xs))))
      (seq [_] (seq (distinct xs)))))
  )

;; For Science!
;; http://www.4clojure.com/problem/117
(def for-science
  (let [size (fn [maze]
               (let [y (count maze)
                     x (count (first maze))]
                 [y x]))

        find-mouse (fn [maze]
                     (first
                      (let [[ym xm] (size maze)]
                        (for [y (range ym)
                              x (range xm)
                              :when (= \M (get-in maze [y x]))]
                          [y x]))))

        neighbors [[-1 0]
                   [1 0]
                   [0 -1]
                   [0 1]]

        moves (fn [pos maze]
                (for [neighbor neighbors
                      :let [pos (mapv + pos neighbor)]
                      :when (and (every? (complement neg?) pos)
                                 (every? identity (map < pos (size maze)))
                                 (not= \# (get-in maze pos)))]
                  pos))]
    (fn [maze]
      (loop [ps [(find-mouse maze)]
             seen #{}]
        (cond
          (some #(= \C (get-in maze %)) ps) true
          (empty? ps) false
          :else (recur (clojure.set/difference (set (mapcat #(moves % maze) ps)) seen)
                       (into seen ps))))))
  )

;; Best Hand
;; http://www.4clojure.com/problem/178
(def best-hand
  (fn [cards]
    (let [anl (fn [[s r]]
                {:suit ((zipmap "DHCS" [:diamond :heart :club :spade]) s)
                 :rank ((zipmap "23456789TJQKA" (range)) r)})
          cs (map anl cards)
          suits (sort (map :suit cs))
          ranks (sort (map :rank cs))
          minr (first ranks)
          rnkcount (reduce #(max % (count %2)) 0 (partition-by identity ranks))
          sf? (and (apply = suits)
                   (= ranks (range minr (+ minr 5))))
          fk? (= 4
                 (apply max
                        (map count
                             (vals (group-by identity ranks)))))
          fh? (and (= (count (distinct ranks)) 2)
                   (or (apply = (take 2 ranks))
                       (apply = (take 3 ranks))))
          fl? (apply = suits)
          st? (or (= ranks (range minr (+ minr 5)))
                  (= ranks (concat (range 4) (list 12))))
          tk? (= 3 rnkcount)
          tp? (= 2 (count (filter #(= 2 (count %))
                                  (partition-by identity ranks))))
          pr? (= 2 rnkcount)]
      (cond
        sf? :straight-flush
        fk? :four-of-a-kind
        fh? :full-house
        fl? :flush
        st? :straight
        tk? :three-of-a-kind
        tp? :two-pair
        pr? :pair
        :else :high-card)))
  )

;; Gus' Quinundrum
;; http://www.4clojure.com/problem/125
(def gus-quinundrum
  (fn []
    (let [s "(fn [] (let [s %s] (format s (pr-str s))))"]
      (format s (pr-str s))))
  )

;; Crossword puzzle
;; http://www.4clojure.com/problem/111
(def crossword-puzzle
  (fn [word board]
    (->> board
         (apply map str)
         (concat board)
         (map #(clojure.string/replace % #"#|_| " {"#" "|" "_" "." " " ""}))
         (map #(re-matches (re-pattern %) word))
         ((complement every?) nil?)))
  )

;; Analyze Reversi
;; http://www.4clojure.com/problem/124
(def analyze-reversi
  (fn [board color]
    (into {} (for [y (range 4)
                   x (range 4)
                   move (for [dx [-1 0 1]
                              dy [-1 0 1]
                              l [3 4]
                              :when (not= 0 dx dy)]
                          (map (juxt #(* dx %) #(* dy %)) (range l)))
                   :let [line (map #(map + [x y] %) move)]
                   :when (every? (partial every? #(<= 0 % 3)) line)
                   :let [from (first line)
                         to (last line)
                         over (rest (butlast line))
                         other-color ({'b 'w 'w 'b} color)]
                   :when (and (= (get-in board from) color)
                              (= (get-in board to) 'e)
                              (every? #(= other-color (get-in board %)) over))]
               [to (set over)])))
  )

;; Squares Squared
;; http://www.4clojure.com/problem/138
(def squares-squared
  (fn [start end]
    (let [direction-map {[1 1] [1 -1]
                         [1 -1][-1 -1]
                         [-1 -1][-1 1]
                         [-1 1] [1 1]}

          digit-chars (mapcat (comp seq str)
                              (take-while #(>= end %)
                                          (iterate #(* % %) start)))

          path-length (first (drop-while #(> (count digit-chars) %)
                                         (map #(* % %) (iterate inc 1))))

          move (fn [{:keys [position direction path]}]
                 (let [keep-direction? (boolean
                                        (some #(= % (mapv +
                                                          position
                                                          (direction-map direction)))
                                              path))
                       next-direction (if keep-direction?
                                        direction
                                        (direction-map direction))] ; turn right
                   {:path (conj path position)
                    :position (mapv + position next-direction)
                    :direction next-direction}))

          coords (first (filter #(= path-length (count %))
                                (map :path
                                     (iterate move
                                              {:path []
                                               :position [0 0]
                                               :direction [-1 1]}))))

          adjust-coords (fn [coords]
                          (let [dy (- (apply min (map first coords)))
                                dx (- (apply min (map last coords)))]
                            (map #(mapv + % [dy dx]) coords)))

          coords-with-char (into {}
                                 (map vector
                                      (adjust-coords coords)
                                      (concat digit-chars (repeat \*))))

          m (count (distinct (map first (keys coords-with-char))))

          template (vec (repeat m
                                (vec (repeat m \space))))]
      (mapv #(apply str %)
            (reduce (fn [t [k v]]
                      (assoc-in t k v))
                    template
                    coords-with-char))))
  )

;; Tree reparenting
;; http://www.4clojure.com/problem/130
(def tree-reparenting
  (fn [parent tree]
    (loop [t tree acc nil]
      (let [[f r]
            ((juxt filter remove) #(some #{parent} (flatten %)) t)]
        (if-let [h (first f)]
          (recur h (concat r (if acc (list acc) acc)))
          (concat r (if acc (list acc) acc))))))
  )

;; Language of a DFA
;; http://www.4clojure.com/problem/164
(def DFA
  (fn language [{:keys [states alphabet start accepts transitions]}]
    (letfn [(alpha-for [[word state]]
              (let [trans (map (fn [[letter to-state]]
                                 (vector (str word letter) to-state))
                               (seq (transitions state)))
                    complete-words (filter (fn [[word to-state]]
                                             (accepts to-state))
                                           trans)]
                (concat (keys (into {} complete-words))
                        (lazy-seq (mapcat alpha-for trans)))))]
      (alpha-for ["" start])))
  )

;; Love Triangle
;; http://www.4clojure.com/problem/127
(def love-triangle
  (fn [rocks]
    (let [lengthen-row
          (fn [row max-size]
            (let [row (map #(if (= \1 %) 1 0) row)
                  width (count row)]
              (if (< width max-size)
                (into [] (concat (repeat (- max-size width) 0) row))
                (into [] row))))

          matrix-base
          (map #(Integer/toBinaryString %) rocks)

          matrix-width
          (apply max (map count matrix-base))

          matrix
          (into [] (map #(lengthen-row % matrix-width) matrix-base))

          row-down
          (fn [row level]
            (get matrix (+ row level)))

          row-up
          (fn [row level]
            (get matrix (- row level)))

          row-left
          (fn [level idx]
            (take level (iterate dec idx)))

          row-right
          (fn [level idx]
            (take level (iterate inc idx)))

          grow-base
          (fn [h-fn v-fn]
            (fn [row idx]
              (loop [acc 0 level 0]
                (let [row-data (h-fn row level)]
                  (if (every? (fn [x] (= 1 x))
                              (map #(get row-data %) (v-fn (inc level) idx)))
                    (recur (+ acc (inc level)) (inc level))
                    [acc level])))))

          grow-bottom-left
          (grow-base row-down row-left)

          grow-bottom-right
          (grow-base row-down row-right)

          grow-top-left
          (grow-base row-up row-left)

          grow-top-right
          (grow-base row-up row-right)

          combine-triangles
          (fn [t1 t2]
            (if (= (second t1) (second t2))
              (- (+ (first t1) (first t2)) (second t1))
              0))

          grow-triangle-down
          (fn [t1 grow-top-fn row idx]
            (let [level (second t1)
                  t2 (grow-top-fn (+ row (* 2 (- level 1))) idx)]
              (combine-triangles t1 t2)))

          score-position
          (fn [row idx]
            (let [bot-left (apply grow-bottom-left [row idx])
                  bot-right (apply grow-bottom-right [row idx])
                  bot-middle (combine-triangles bot-left bot-right)
                  top-left (apply grow-top-left [row idx])
                  top-right (apply grow-top-right [row idx])
                  top-middle (combine-triangles top-left top-right)
                  bot-ldown (grow-triangle-down bot-left grow-top-left row idx)
                  bot-rdown (grow-triangle-down bot-right grow-top-right row idx)]
              (apply max (concat (map first [bot-left bot-right top-left top-right])
                                 [bot-middle top-middle bot-ldown bot-rdown]))))

          score
          (apply max (for [row (range (count matrix))
                           col (range matrix-width)]
                       (score-position row col)))]
      (when (> score 2)
        score)))
  )

;; Veitch, Please!
;; http://www.4clojure.com/problem/140
(def veitch-please
  (fn [m]
    (let [sd clojure.set/difference
          si clojure.set/intersection
          su clojure.set/union
          w (count (first m))
          g (group-by
             (fn [r]
               (count (filter #(#{'A 'B 'C 'D} %) r)))
             m)
          pv (for [i (range w) j (g i) k (g (inc i))
                   :when (contains?
                          #{#{'A 'a}, #{'B 'b}, #{'C 'c}, #{'D 'd}}
                          (sd (su j k) (si j k)))]
               [#{j k} (si j k)])
          p2 (set (map last pv))]
      (if (empty? p2)
        (disj m #{'A 'd})
        (recur (su (sd m (apply su (map first pv))) p2)))))
  )
