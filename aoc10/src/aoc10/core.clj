(ns aoc10.core
  (:gen-class))


;; read-input
(defn read-input []
  (mapv identity 
    (line-seq (java.io.BufferedReader. *in*))))


;; parse input
(defn parse-input [lines]
  (->> lines
    (mapv #(Integer/parseInt %))))


;; find the sequence of adapters
(defn find-sequence [numbers]
  (let [numbers-ext (-> numbers
                     (conj 0)
                     (conj (+ 3 (reduce max numbers)))
                     (sort))]
    (->> numbers-ext
      (drop 1)
      (into [])
      (reduce-kv
        (fn [acc k v] (conj acc [k (- v (nth numbers-ext k))]))
        []))))

(defn count-steps [number-pairs]
  (->> number-pairs 
    (map (fn [[k v]] v))
    (frequencies)))

(defn count-result [map]
  (* (get map 1) (get map 3)))

(defn count-steps-multiplied [numbers]
  (->> numbers
    (find-sequence)
    (count-steps)
    (count-result)))


;; find all possible sequences
(defn find-adj [numbers-ext x]
  (filter (fn [y] (and (> y x) (<= y (+ x 3)))) numbers-ext))

(defn adj-matrix [numbers-ext]
  (let [adj-list (for [x numbers-ext]
                   (if (= x last)
                     [x []]
                     [x (find-adj numbers-ext x)]))]
    (reduce (fn [acc [x xs]] (assoc acc x xs)) {} adj-list)))

(defn reduce-graph [graph]
  (get
    (->> graph 
      (keys)
      (sort)
      (reverse)
      (reduce 
        (fn 
          [acc key]
          (let [vert (get graph key)
                val (if (empty? vert) 1 (reduce (fn [acc2 key2] (+ acc2 (get acc key2))) 0 vert))]
            (assoc acc key val)))
        {}))
    0))

(defn count-permutations [numbers]
  (let [last (+ 3 (reduce max numbers))
        numbers-ext (-> numbers
                      (conj 0)
                      (conj last)
                      (sort))]
    (->> 
      (adj-matrix numbers-ext)
      (reduce-graph))))

;; entrypoint
(defn -main
  "Advent of code - day 10"
  [& args]
  (->> (read-input)
    (parse-input)
    ; (count-steps-multiplied)
    (count-permutations)
    (prn)))
