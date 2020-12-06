(ns aoc6.core
  (:require [clojure.set :as s])
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))

(defn reduce-split [[current acc] line]
  (if (= line "")
    ['(), (conj acc current)]
    [(conj current line), acc]))


(defn parse-input [lines]
  (let [[last acc] (reduce reduce-split ['(), '()] lines)]
    (conj acc last)))

(defn count-any
  "count the letters on the [vote-list] where any of the entries contains the letter"
  [vote-list]
  (->> 
    vote-list
    (str/join)
    (set)
    (count)))

(defn count-all 
  "count the letters on the [vote-list] where ALL entries contain the letter"
  [vote-list]
  (let [c (count vote-list)]
    (->> 
      vote-list
      (str/join)
      (frequencies)
      (seq)
      (filter (fn [[letter freq]] (= freq (count vote-list)))) 
      (count)))) 

(defn -main
  "Advent of code - day 6"
  [& args]
  (->> 
    (read-input)
    (parse-input)
    (map count-all)
    (reduce +)
    (println))
  )
