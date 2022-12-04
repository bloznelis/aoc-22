(ns aoc-22.day3
  (:require [clojure.string :as str])
  (:require [clojure.set :as s]))

(defn split-at-middle [col] (split-at (/ (count col) 2) col))
(defn to-priority [char] (let [ascii (int char)] (if (> ascii 96) (- ascii 96) (- ascii 38))))

(defn prioritize [cols]
  (->> (map set cols)
       (apply s/intersection)
       first
       to-priority))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map #(prioritize (split-at-middle %)))
       (apply +)))

(defn part-2 [input]
  (->> (str/split-lines input)
       (partition 3)
       (map prioritize)
       (apply +)))

(comment

  (def rucksack-input "vJrwpWtwJgWrhcsFMMfFFhFp")

  (partition 3 2 rucksack-input)
;; => ((\v \J \r) (\r \w \p) (\p \W \t) (\t \w \J) (\J \g \W) (\W \r \h) (\h \c \s) (\s \F \M) (\M \M \f) (\f \F \F) (\F \h \F))

  (def test-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
  (def real-input (slurp "/home/lukas/code/clojure/aoc-22/input/day3.txt"))

  (part-1 real-input)
  (part-2 real-input)

;;
  )
