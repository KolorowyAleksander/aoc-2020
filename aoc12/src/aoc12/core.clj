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
(defn rotate [[xw yw] direction val]
  (let [angle-deg (match direction
                    "R" (* -1 val)
                    "L" val
                    :else  (throw (Exception. (format "INVALID ROTATION in rotate: %s_%s" direction val))))
        angle (* angle-deg (/ Math/PI 180))]
    [
      (- 
        (Math/round (* (Math/cos angle) xw)) 
        (Math/round (* (Math/sin angle) yw)))
      (+
        (Math/round (* (Math/sin angle) xw))
        (Math/round (* (Math/cos angle) yw)))]))

(defn forward [[x y] [xw yw] val]
  [(+ x (* xw val)) (+ y (* yw val))])

(defn execute-action [coords waypoint action]
  (let [[x y]     coords
        [xw yw]   waypoint
        [act val] action]
    (match action
      ["N" _] [coords [xw (+ yw val)]]
      ["S" _] [coords [xw (- yw val)]]
      ["E" _] [coords [(+ xw val) yw]]
      ["W" _] [coords [(- xw val) yw]]
      ["L" _] [coords (rotate waypoint "L" val)]
      ["R" _] [coords (rotate waypoint "R" val)]
      ["F" _] [(forward coords waypoint val) waypoint]
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
         waypoint [10 1]
         actions actions
         acc []]
    (if (empty? actions)
      acc
      (let [f (first actions)
            [new_coords new_waypoint] (execute-action coords waypoint f)]
        (recur 
          new_coords 
          new_waypoint 
          (drop 1 actions)
          (conj acc [new_coords new_waypoint]))))))

(defn calculate [[[x y] _]]
  (+ (math/abs x) (math/abs y)))

(defn onetwothree [seq]
  (doseq [x seq] (prn x)))

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
    ; (onetwothree)))
