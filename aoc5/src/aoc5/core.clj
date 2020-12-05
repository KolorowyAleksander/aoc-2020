(ns aoc5.core
  (:require [clojure.math.numeric-tower :as math :refer [expt]])
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))

(defn binary-search-seat [row-letters]
  (->> 
    row-letters
    (map-indexed (fn [idx l] [l (/ (expt 2 (count row-letters)) (expt 2 (+ idx 1)))]))
    (reduce (fn [acc [l val]]
     (if (or (= l \L) (= l \F))
       acc
       (+ acc val))) 0)
  ))

(defn find-seat [lines]
  (let [[row-letters col-letters] (split-at 7 lines)]
    [(binary-search-seat row-letters) (binary-search-seat col-letters)]))

(defn count-seat-id [[row column]]
  (+ (* row 8) column))

(defn -main
  "Advent of code - day 5"
  [& args]
  (->> 
    (read-input)
    (map find-seat)
    (map count-seat-id)
    (sort)
    (reduce (fn [acc item] (if (= (+ acc 2) item) (println acc) item))))
  )
