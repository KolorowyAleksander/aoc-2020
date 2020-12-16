(ns aoc16.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:require [clojure.core.match :refer [match]])
  (:gen-class))


;; read-input
(defn read-input []
  (slurp *in*))


;; parse input
(defn parse-field [line]
  (let [splits (str/split line #"(: | or |-)")]
    [(first splits) (into [] (partition 2 (mapv (fn [itm] (Integer/parseInt itm)) (drop 1 splits))))]))

(defn parse-ticket [line] 
  (mapv (fn [itm] (Integer/parseInt itm)) (str/split line #",")))

(defn parse-input [lines]
  (let [[fields ticket tickets] (str/split lines #"(\n\nyour ticket:\n|\n\nnearby tickets:\n)")]
    [(reduce 
       (fn [acc itm] (let [[key val] (parse-field itm)] (assoc acc key val)))
       {}
       (str/split-lines fields)) 
     (parse-ticket ticket)
     (mapv parse-ticket (str/split-lines tickets))]))


;; part 1 of the challenge
(defn check-intervals "0 if valid, value if invalid" [intervals value]
  (if (some identity (map (fn [[fst snd] ] (<= fst value snd)) intervals))
    0
    value))

(defn sum-invalid-tickets [[fields _ tickets]]
  (let [intervals (reduce (fn [acc [_ val]] (conj acc (first val) (second val))) [] (seq fields))]
    (reduce (fn [acc itm] (+ acc (check-intervals intervals itm))) 0 (flatten tickets))))

;; entrypoint
(defn -main
  "Advent of code - day 16"
  [& args]
  (->> (read-input)
    (parse-input)
    (sum-invalid-tickets)
    (pp/pprint)))
