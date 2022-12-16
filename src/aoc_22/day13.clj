(ns aoc-22.day13
  (:require [clojure.string :as str]
            [aoc-22.utils :as utils]))

(defn colls? [& colls] (every? true? (map coll? colls)))
(defn ints? [& ints] (every? true? (map int? ints)))

(defn compare-lists [a b]
  (cond
    (and (empty? a) (empty? b)) :continue
    (empty? a) true
    (empty? b) false
    (= (first a) (first b)) (compare-values (rest a) (rest b))
    :else (compare-values (first a) (first b))))

(defn compare-ints [a b]
  (if (= a b) :continue (< a b)))

(defn compare-values [a b]
  (cond
    (colls? a b) (compare-lists a b)
    (ints? a b) (compare-ints a b)
    (int? a) (compare-lists (list a) b)
    (int? b) (compare-lists a (list b))))

(defn compare-signals [left-signal right-signal]
  (if (and (empty? left-signal) (empty? right-signal)) true
      (let [result (compare-values left-signal right-signal)]
        (case result
          :continue (recur (rest left-signal) (rest right-signal))
          result))))

(defn read-input [input]
  (->> (utils/parse-blocks input)
       (mapcat #(str/split % #"\n"))
       (map read-string)))

(defn p1 [input]
  (->> (read-input input)
       (partition 2)
       (map #(apply compare-signals %))
       (map-indexed #(if %2 (inc %1) nil))
       (filter some?)
       (apply +)))

(defn p2 [input]
  (->> (read-input input)
       (#(conj % [[2]] [[6]]))
       (sort compare-signals)
       (map-indexed #(vector (inc %1) %2))
       (filter #(or (= (second %) [[6]]) (= (second %) [[2]])))
       (map first)
       (apply *)))
