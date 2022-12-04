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
