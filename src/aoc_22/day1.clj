(ns aoc-22.day1
  (:require [clojure.string :as str]))

(def input (slurp "./input/day1.txt"))

;; part1
(->>
 (str/split input #"\n\n")
 (map str/split-lines)
 (map #(apply + (map read-string %)))
 (apply max))

;; part2
(->>
 (str/split input #"\n\n")
 (map str/split-lines)
 (map #(apply + (map read-string %)))
 sort
 (take-last 3)
 (apply +))
