(ns aoc-22.day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as s]))

(defn to-inclusive-ranges [[a1 a2 b1 b2]]
  [(range a1 (inc a2)) (range b1, (inc b2))])

(defn fully-overlapping? [[section1 section2]]
  (#(or (s/subset? %1 %2) (s/superset? %1 %2)) section1 section2))

(defn overlaps? [[section1 section2]]
  (seq (s/intersection section1 section2)))

(defn handle-line [fn line]
  (->>
   (str/split line #",")
   (mapcat #(str/split % #"-"))
   (map read-string)
   (to-inclusive-ranges)
   (map set)
   fn))

(defn p1 [input]
  (->>
   (str/split-lines input)
   (map #(handle-line fully-overlapping? %))
   (filter identity)
   count))

(defn p2 [input]
  (->>
   (str/split-lines input)
   (map #(handle-line overlaps? %))
   (filter identity)
   count))
