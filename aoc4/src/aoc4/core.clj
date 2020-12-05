(ns aoc4.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as p])
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))

(defn reduce-split [acc line]
  (if (= line "")
    (conj acc "")
    (let [[x & xs] acc]
      (if (= x "")
        (conj xs line)
        (conj xs (str/join " " [x line]))))))

(defn parse-input [lines]
  (->>
    (reduce reduce-split '() lines)
    (map (fn [line] (str/trim line)))
    (map (fn [line] (str/split line #" ")))
    (map #(reduce (fn [acc pair] (let [[k v] (str/split pair #":")] (assoc acc k v))) {} %1))))

(defn between [s a b]
  (and 
    (>= (compare s a) 0) 
    (<= (compare s b) 0)))

(defn match [s reg]
  (not= (re-matches reg s) nil))

(def field-validators 
  { "byr" (fn [val] (between val "1920" "2002"))
    "iyr" (fn [val] (between val "2010" "2020"))
    "eyr" (fn [val] (between val "2020" "2030"))
    "hgt" (fn [val] (and 
                      (match val #"^(\d{3}cm)|(\d{2}in)")
                      (or (between val "150cm" "193cm") (between val "59in" "76in"))))
    "hcl" (fn [val] (match val #"^#[0-9a-f]{6}$"))
    "ecl" (fn [val] (match val #"^(amb|blu|brn|gry|grn|hzl|oth)$"))
    "pid" (fn [val] (match val #"^[0-9]{9}$"))})

(defn validate-field [field value validator]
  (if (= value nil) 
    false
    (validator value)))

(defn is-valid [passport]
  (->> 
    (seq field-validators)
    (map (fn [[field validator]] (validate-field field (get passport field) validator)))
    (reduce (fn [a b] (and a b)))))

(defn count-valid [passports]
  (reduce (fn [acc passport] (if (is-valid passport) (+ acc 1) acc)) 0 passports))

(defn -main
  "Advent of code - day 4"
  [& args]
  (->> 
    (read-input)
    (parse-input)
    (count-valid)
    (p/pprint)))
