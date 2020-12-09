(ns aoc9.core
  (:gen-class))


;; read-input
(defn read-input []
  (mapv identity 
    (line-seq (java.io.BufferedReader. *in*))))


;; parse input
(defn parse-input [lines]
  (->> lines
    (mapv #(Long/parseLong %))))


;; find `invalid` number in input
(defn check-valid [num nums]
  (some (fn [x] (= x true)) 
    (for [x nums
          y nums]
      (if (= (+ x y) num)
        true
        false))))

(defn check-subsequences [preamble-len numbers]
  (map-indexed
    (fn [idx num] [num (check-valid num (subvec numbers idx (+ idx preamble-len)))]) 
    (drop preamble-len numbers)))

(defn find-invalid [numbers]
  (->> numbers
    (check-subsequences 25)
    (filter (fn [[x v]] (= v nil)))
    (map (fn [[k _]] k))
    (first)))


;; find subvector which sums to invalid number
(defn find-sum-subvec
  "find a subsequence of contiguous numbers that sum to `x`"
  [vec x]
  (loop [s 0 e 1]
    (let [subseq (subvec vec s e)
          sum (reduce + subseq)]
      (cond 
        (= sum x) subseq
        (< sum x) (recur s (+ e 1))
        :else (recur (+ 1 s) (+ 2 s))))))

(defn minl "this is because clojure apparently doesn't mave `min` for longs" [longs]
  (reduce (fn [x y] (if (<= x y) x y)) longs))

(defn maxl "this is becuase clojure apparently doesn't have `max` for longs" [longs]
  (reduce (fn [x y] (if (>= x y) x y)) longs))

(defn sum-min-max [vec]
  (+ (minl vec) (maxl vec)))


;; entrypoint
(defn -main
  "Advent of code - day 9"
  [& args]
  (let [numbers (parse-input (read-input))
        invalid (find-invalid numbers)]
    (->> (find-sum-subvec numbers invalid)
      (sum-min-max)
      (prn))))
