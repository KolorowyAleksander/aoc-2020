(ns aoc3.core
  (:gen-class))

(defn read-input []
  (line-seq (java.io.BufferedReader. *in*)))
  
(defn parse-input [lines]
  [(reverse (reduce conj '() lines)) (count lines) (count (first lines ))])

(defn walk [step-down step-right [input-sequence depth width]]
  (loop [d 0
         w 0
         count 0
         seq input-sequence]
    (if (and (>= d depth))
      count
      (let [line (nth seq d) character (nth (nth seq d) w)]
        (printf "\t%d\t%d\t%d\t%c\t%s\n" count d w character line)
        (recur 
          (+ d step-down)
          (mod (+ w step-right) width)
          (+ count (if (= character \#) 1 0))
          seq)))))

(defn -main
  "Advent of code - day 3"
  [& args]
  (->> (read-input)
    (parse-input)
    (walk 2 1)
    (println)))
