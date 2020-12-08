(ns aoc8.core
  (:require [clojure.string :as str])
  (:require [clojure.core.match :refer [match]])
  (:gen-class))


;; read-input
(defn read-input []
  (mapv identity 
    (line-seq (java.io.BufferedReader. *in*))))


;; parse input
(defn parse-instruction [line]
  (let [[inst number] (str/split line #" ")]
    (let [[sign n] (split-at 1 number)]
      [(match inst
        "acc" :acc
        "nop" :nop
        "jmp" :jmp)
       (match (str/join sign)
        "+" :add
        "-" :sub)
       (Integer/parseInt (str/join n))])))

(defn parse-input [lines]
  (->> lines
    (mapv parse-instruction)))

;; execution
(defn execute-instruction [instruction context]
  (let [[inst sign val] instruction
        [n pc acc exec] context
        new-exec (conj exec pc)]
    ; (printf "%s %s %d  %d %d\n" inst sign val pc acc)
    (match [inst sign]
      [:nop _]    [(+ pc 1) acc new-exec]
      [:acc :add] [(+ pc 1) (+ acc val) new-exec]
      [:acc :sub] [(+ pc 1) (- acc val) new-exec]
      [:jmp :add] [(mod (+ pc val) n) acc new-exec]
      [:jmp :sub] [(mod (- pc val) n) acc new-exec])))

(defn loop-termination "detect infinite execution" [context]
  (let [[n pc acc exec] context]
    (contains? exec pc)))
  
(defn end-termination "detect normal execution" [context]
  (let [[n pc acc exec] context]
    (= pc (- n 1))))

(defn execute-program 
  "execute the program instructions until the end-condition happens, either
   termination by executing the last instruction in the program, or an
   attempt to execute the same instruction a second time"
  [program]
  (let [n (count program)]
    ; (printf "inst sign v  p a, program length: %d\n" n)
    (loop [pc 0 
           acc 0
           exec #{}]
      (if (loop-termination [n pc acc exec])
        [acc, :loop-termination]
        (let [should-terminate (end-termination [n pc acc exec])
              [pc acc new-exec] (execute-instruction (nth program pc) [n pc acc exec])]
          (if should-terminate
            [acc :end-termination]
            (recur pc acc new-exec)))))))


;; find candidates for corrupted instruction
(defn find-candidates [program]
  (->> program
    (map-indexed (fn [idx itm] [idx itm]))
    (filter (fn [[line-no [inst sign val]]] (or (= inst :nop) (= inst :jmp))))))

(defn modify-program [program candidate]
  (let [[lineno [inst sign val]] candidate]
    (match inst
      :jmp (assoc program lineno [:nop sign val])
      :nop (assoc program lineno[:jmp sign val]))))

(defn execute-candidates [program candidates]
  (map 
    (fn [candidate]
      [candidate (execute-program (modify-program program candidate))]) 
    candidates))

(defn find-terminating-program 
  "generate programs that can be candidates for a properly terminating execution and execute them,
   looking for the one that terminates normally"
  [program]
  (->> program
    (find-candidates)
    (execute-candidates program)
    (filter (fn [[candidate [acc term]]] (= term :end-termination)))
    (first)))

;; entrypoint
(defn -main
  "Advent of code - day 8"
  [& args]
  (->>
   (read-input)
   (parse-input)
   (find-terminating-program)
   (prn)))
