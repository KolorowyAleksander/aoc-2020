(ns aoc2.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))

(defn split-input [line]
  (let [[x first-tail] (str/split line #"-" 2)] 
    (let [[y second-tail] (str/split first-tail #" " 2)]
      (let [[letter pass] (str/split second-tail #": " 2)]
        [(Integer/parseInt x) (Integer/parseInt y) (first letter) pass]))))

(defn parse-input [number-strings]
  (map split-input number-strings))

(defn check-password [[x y letter pass]]
  (<= x (get (frequencies pass) letter 0) y))

(defn binary-xor [a b]
  (or
   (and (not a) b)
   (and a (not b))))

(defn check-password-2 [[x y letter pass]]
  (binary-xor  
    (= (nth pass (- x 1)) letter)
    (= (nth pass (- y 1)) letter)))

(defn -main
  "Advent of code - day 2"
  [& args]

  (->> (read-input)
    (parse-input)
    (filter check-password-2)
    (count)
    (println)))
