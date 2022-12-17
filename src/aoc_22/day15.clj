(ns aoc-22.day15
  (:require [clojure.string :as str]))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def test-line "Sensor at x=2, y=18: closest beacon is at x=-2, y=15")
(def real-input (slurp "/home/lukas/code/clojure/aoc-22/input/day15.txt"))

(defn manhatan-dist [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn as-entry [sensor beacon]
  {(vec sensor) {:dist (manhatan-dist sensor beacon) :beacon beacon}})

(defn parse-line [line]
  (->> line
       (re-seq #"-?[0-9]+")
       (map read-string)
       (partition 2)
       (apply as-entry)))

(defn prepare-sensor-table [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce conj)))

(defn find-max-x [sensor-table]
  (->> sensor-table
       (sort-by #(+ (first (first %)) (:dist (second %))) >)
       first
       (apply (fn [[x, _] {dist :dist}] (+ x dist)))))

(defn find-min-x [sensor-table]
  (->> sensor-table
       (sort-by #(- (first (first %)) (:dist (second %))) <)
       first
       (apply (fn [[x, _] {dist :dist}] (- x dist)))))

(defn coords-to-check [max-x min-x y]
  (for [x (range (dec min-x) (inc max-x))]
    [x y]))

(defn in-range-of-sensor [coords [sensor {sensor-range :dist}]]
  (let [dist (manhatan-dist coords sensor)]
    (<= dist sensor-range)))

(vec {:a {:b :c}})

(defn in-range-of-any-sensor [coords sensor-table]
  (loop [[head & tail] sensor-table]
    (if (nil? head)
      false
      (if (in-range-of-sensor coords head) true (recur tail)))))

;;don't forget existing beacons
(defn compute [input at-y]
  (let [sensor-table (vec (prepare-sensor-table input))
        beacons-cnt (count (into #{} (filter (fn [[_ y]] (= y at-y)) (map :beacon (vals sensor-table)))))
        coords (coords-to-check (find-max-x sensor-table) (find-min-x sensor-table) at-y)
        cannot-exist-count (reduce (fn [cnt coord] (if (in-range-of-any-sensor coord sensor-table) (inc cnt) cnt)) 0 coords)]

    (- cannot-exist-count beacons-cnt)))

(comment
  (compute real-input 2000000)
;; => 4879972
  (coords-to-check (find-max-x (prepare-sensor-table test-input)) (find-min-x (prepare-sensor-table test-input)) 10)

  (find-max-x (prepare-sensor-table test-input))
  (first (vec (prepare-sensor-table test-input)))
  (parse-line test-line)
  (prepare-sensor-table test-input)

  (manhatan-dist [8 7] [2 10]))
