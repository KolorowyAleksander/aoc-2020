(ns aoc7.core
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require  [defun.core :refer [defun]])
  (:gen-class))

(defn read-input []
  (line-seq
   (java.io.BufferedReader. *in*)))

(def split-expr #"(, | bags?|\.| contain )")

;; first part: dfs on a tree searching for a value
(defun find-gold 
  "find how many bags can contain a \"shiny gold\" bag"
  ([tree-map] 
    (->> (seq (dissoc tree-map "shiny gold"))
      (map (fn [[k _]] [k (find-gold tree-map k)]))
      (filter (fn [[_ v]] (= v true)))
      (count)))
  ([_ "no other"] false)
  ([_ "shiny gold"] true)
  ([tree-map key]
      (reduce (fn [acc [k _]] (or acc (find-gold tree-map k))) false (get tree-map key))))

;; second part: tree reduction
(defn count-nests 
  "count how many objects the \"shiny gold\" bag contains"
  ([tree-map] (count-nests tree-map "shiny gold"))
  ([tree-map key]
    (let [items (get tree-map key)]
      (if (= items nil)
        1
        (reduce (fn [acc [k num]] (+ (* (count-nests tree-map k) num) num acc)) 0 items)))))

;; parsing input into a tree-map
(defn parse-bag [bag] 
  (let [raw (str/trim bag)]
   (match raw 
     "no other" [raw 0]
     :else (let [[number text] (str/split raw #" " 2)]
       [text (Integer/parseInt number)]))))

(defn parse-line [line]
  (let [[first & rest] (str/split line split-expr)]
    (conj (map parse-bag (remove empty? rest)) first)))

(defn parse-input [lines]
  (->> lines 
    (map parse-line)
    (reduce (fn [acc [x & xs]] (assoc acc x xs)) '{})))

(defn -main
  "Advent of code - day 7"
  [& args]
  (->>
   (read-input)
   (parse-input)
   (find-gold)
   (prn)))
