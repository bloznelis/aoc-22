(ns aoc-22.day15
  (:require [clojure.string :as str]
            [clojure.set :as s]))

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
  (<= (manhatan-dist coords sensor) sensor-range))

(defn in-range-of-any-sensor [coords sensor-table]
  (loop [[head & tail] sensor-table]
    (if (nil? head)
      false
      (if (in-range-of-sensor coords head) true (recur tail)))))

(defn line-intersection-point-x [x1 y1 x2 y2 x3 y3 x4 y4]
  (let [top (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4)) (* (- x1 x2) (- (* x3 y4) (* y3 x4))))
        bot (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (/ top bot)))

(defn line-intersection-point-y [x1 y1 x2 y2 x3 y3 x4 y4]
  (let [top (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4)) (* (- y1 y2) (- (* x3 y4) (* y3 x4))))
        bot (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (/ top bot)))

(defn line-intersection-point [[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
  (try
    [(line-intersection-point-x x1 y1 x2 y2 x3 y3 x4 y4) (line-intersection-point-y x1 y1 x2 y2 x3 y3 x4 y4)]
    (catch Exception _ :parallel)))

(defn get-lines [[[sensor-x sensor-y] {dist :dist}]]
  (let [north [sensor-x (- sensor-y (inc dist))]
        east [(+ sensor-x (inc dist)) sensor-y]
        south [sensor-x (+ sensor-y (inc dist))]
        west [(- sensor-x (inc dist)) sensor-y]]

    [[north east] [east south] [north west] [west south]]))

(defn intersection [[point1 point2] all-lines]
  (into #{} (mapcat (fn [[point3 point4]]
                      (let [result (line-intersection-point point1 point2 point3 point4)]
                        (case result
                          :parallel #{}
                          (if (and
                               (integer? (first result))
                               (integer? (second result))
                               (<= 0 (first result))
                               (<= 0 (second result))
                               (> 4000001 (first result))
                               (> 4000001 (second result)))
                            #{result}
                            #{})))) all-lines)))

(defn line-intersections [lines]
  (loop [[head & tail] lines
         intersections #{}]
    (if (nil? head)
      intersections
      (recur tail (s/union intersections (intersection head lines))))))

(defn p1 [input at-y]
  (let [sensor-table (vec (prepare-sensor-table input))
        beacons-cnt (count (into #{} (filter (fn [[_ y]] (= y at-y)) (map :beacon (vals sensor-table)))))
        coords (coords-to-check (find-max-x sensor-table) (find-min-x sensor-table) at-y)
        cannot-exist-count (reduce (fn [cnt coord] (if (in-range-of-any-sensor coord sensor-table) (inc cnt) cnt)) 0 coords)]

    (- cannot-exist-count beacons-cnt)))

(defn p2 [input]
  (let [sensor-table (vec (prepare-sensor-table input))
        coords-to-check (line-intersections (mapcat get-lines sensor-table))
        [secret-beacon-x secret-beacon-y] (loop [[head & tail] coords-to-check]
                                            (if (not (in-range-of-any-sensor head sensor-table)) head (recur tail)))]

    (+ (* secret-beacon-x 4000000) secret-beacon-y)))
