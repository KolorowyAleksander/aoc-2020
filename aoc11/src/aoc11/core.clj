(ns aoc11.core
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [aoc11.arr :as arr])
  (:gen-class))


;; read-input
(defn read-input []
  (mapv identity 
    (line-seq (java.io.BufferedReader. *in*))))


;; parse input
(defn parse-input [lines]
  (let [rows (count lines)
        cols (count (get lines 0))]
    (arr/->Array (into [] (str/join lines)) 
                 rows 
                 cols)))


;; change state
(defn get-arr [arr rows cols row col]
  (if (or (< row 0) (< col 0) (> row (- rows 1)) (> col (- cols 1)))
    nil
    (get arr (+ (* cols row) col))))

(defn adjacent [arr rows cols idx]
  (let [x (quot idx cols)
        y (mod idx cols)]
    ; (print x y " ")
    (->> 
      (for [i [(- x 1) x (+ x 1)] j [(- y 1) y (+ y 1)]] [i j]) 
      (filter (fn [z] (not= z [x y])))
      (map (fn [[i j]] (get-arr arr rows cols i j))))))

(defn look [arr rows cols x y dirx diry]
  ; (printf "dirx: %d, diry: %d\n" dirx diry)
  (loop [xi (+ x dirx)
         yi (+ y diry)]
    ; (printf "xi: %d, yi: %d, dirx: %d, diry: %d\n" xi yi dirx diry)
    (let [itm (get-arr arr rows cols xi yi)]
      (match itm
        nil nil
        \.  (recur (+ xi dirx) (+ yi diry))
        :else itm))))

(defn visible [arr rows cols idx]
  (let [x (quot idx cols)
        y (mod idx cols)]
    (->> 
      (for [[i j] [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]] [i j]) 
      (map (fn [[i j]] (look arr rows cols x y i j))))))

(defn calculate-state [array]
  (let [arr (:arr array) cols (:cols array) rows (:rows array)]
    (arr/->Array 
      (into []
        (map-indexed 
          (fn [idx itm] 
            (let [adj (visible arr rows cols idx)]
              ; (printf "%d %c %s\n" idx itm (into [] adj))
              (if (and (= itm \L) (= 0 (count (filter (fn [it] (= it \#)) adj))))
                \#
                (if (and (= itm \#) (>= (count (filter (fn [it] (= it \#)) adj)) 5))
                  \L
                  itm))))
          arr))
      rows
      cols)))

(defn stabilize [array]
  (let [first array
        second (calculate-state first)]
    (loop [prev first
           curr second]
      ; (arr/print curr)
      (if (= (:arr prev) (:arr curr))
        curr
        (recur curr (calculate-state curr))))))

(defn count-occupied [array]
  (reduce (fn [acc itm] (+ acc (if (= itm \#) 1 0))) 0 (:arr array)))

;; entrypoint
(defn -main
  "Advent of code - day 11"
  [& args]
  (->> (read-input)
    (parse-input)
    ; (calculate-state)
    ; (calculate-state)
    ; (calculate-state)
    (stabilize)
    (count-occupied)
    (prn)))
