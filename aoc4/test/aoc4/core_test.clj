(ns aoc4.core-test
  (:require [clojure.test :refer :all]
            [aoc4.core :refer :all]))

(def value "173cm")

(deftest a-test
  (testing "Beetween centimeteres"
    (is ((get field-validators "hgt") value) true)))
