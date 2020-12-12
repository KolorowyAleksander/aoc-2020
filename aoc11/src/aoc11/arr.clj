(ns aoc11.arr
  (:require [clojure.string :as str])
  (:refer-clojure 
    :rename {get get-int}
    :exclude [set split print]))

(defprotocol Arr
  (get [this row col])
  (set [this row col val])
  (split [this])
  (print [this])
  (duppa [this fun]))

(defrecord Array [arr rows cols]
  Arr
  (get [this row col] (get-int (:arr this) (+ (* (:cols this) row) col)))
  (set [this row col val] 
    (->Array 
      (assoc (:arr this) (+ (* (:cols this) row) col) val)
      (:rows  this)
      (:cols this)))
  (split [this] (partition (:rows this) (:arr this)))
  (print [this] (doseq [seqq (split this)] (println (str/join seqq)))
    (println)
    this)
  (duppa [this fun] (->Array
                      (map fun (:arr this))
                      (:rows this)
                      (:cols this))))
