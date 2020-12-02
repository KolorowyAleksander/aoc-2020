(ns aoc1.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))

(defn parse-input [number-strings]
  (map #(Integer/parseInt %) number-strings))

(defn build-tuples [arity numbers]
  (combo/combinations numbers arity))

(defn is-2020 [vector]
  (= (reduce + vector) 2020))

(defn find-tuple [number-tuples]
  (first (filter is-2020 number-tuples)))

(defn parse-option [args]
  (if (= (first args) "--triples") 3 2))

(defn -main
  "Advent of code - day 1"
  [& args]

  (->> (read-input)
    (parse-input)
    (build-tuples (parse-option args))
    (find-tuple)
    (reduce *)
    (println)))
