(ns aoc12.core
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))


;; read-input
(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))


;; parse input
(defn parse-input [lines]
  (map 
    (fn [line] 
      (let [first (str/join (take 1 line)) second (str/join (drop 1 line))]
        [first (Integer/parseInt second)]))
    lines))


;; execute provided actions
(defn rotate [direction d val]
  (match d
    "L" (mod (- direction val) 360)
    "R" (mod (+ direction val) 360)
    :else  (throw (Exception. (format "INVALID ROTATION in rotate: %s_%s" d val)))))

(defn forward [[x y] direction val]
  (match direction
    0   [(+ x val) y]
    90  [x (+ y val)]
    180 [(- x val) y]
    270 [x (- y val)]
    :else  (throw (Exception. (format "INVALID DIRECTION in forward: %s_%s" direction val)))))

(defn execute-action [coords direction action]
  (let [[x y] coords
        [act val] action]
    (match action
      ["N" _] [[(+ x val) y] direction]
      ["S" _] [[(- x val) y] direction]
      ["E" _] [[x (+ y val)] direction]
      ["W" _] [[x (- y val)] direction]
      ["L" _] [[x y] (rotate direction "L" val)]
      ["R" _] [[x y] (rotate direction "R" val)]
      ["F" _] [(forward coords direction val) direction]
      :else  (throw (Exception. (format "INVALID ACTION in execute-action: %s_%s" act val))))))

; Action N means to move north by the given value.
; Action S means to move south by the given value.
; Action E means to move east by the given value.
; Action W means to move west by the given value.
; Action L means to turn left the given number of degrees.
; Action R means to turn right the given number of degrees.
; Action F means to move forward by the given value in the direction the ship is currently facing.
; Rotations can only be performed in (90 180 270) degrees
(defn execute [actions]
  (loop [coords [0 0]
         direction 90
         actions actions
         acc []]
    (if (empty? actions)
      acc
      (let [f (first actions)
            [new_coords new_direction] (execute-action coords direction f)]
        (recur 
          new_coords 
          new_direction 
          (drop 1 actions)
          (conj acc [new_coords new_direction]))))))

(defn calculate [[[x y] _]]
  (+ (math/abs x) (math/abs y)))

;; entrypoint
(defn -main
  "Advent of code - day 12"
  [& args]
  (->> (read-input)
    (parse-input)
    (execute)
    (last)
    (calculate)
    (prn)))
