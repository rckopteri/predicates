(ns predicates)

(defn sum-f [f g x] (+ (f x) (g x)))

(defn greater-than [n]
  (fn [k] (> k n)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [x] (if (contains? a-set x) true false)))

(defn pred-and [pred1 pred2]
  (fn [x]
    (if (and (pred1 x) (pred2 x)) true false)))

(defn pred-or [pred1 pred2]
  (fn [x]
    (if (or (pred1 x) (pred2 x)) true false)))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (or
    (nil? string)
    (every? whitespace? string)))

(defn has-award? [book award]
  (if (some #(= award %) (:awards book)) true false))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [x]
            (contains? (:awards book) x)) awards))

(defn my-some [pred a-seq]
  (first (filter
           (fn [value] value)
           (map pred a-seq))))

(defn my-every? [pred a-seq]
  (let [not-pred (complement pred)]
    (if (= a-seq (filter pred a-seq)) true false)))

(defn prime? [n]
  (let [my-list (range 1 (+ 1 n))
        my-map (filter #(= 0 (rem n %)) my-list)]
    (if (= 2 (count my-map))
      true
      false)))
;^^
