(ns aoc-22.day8
  (:require [clojure.string :as str]))

(defn get-m [x y matrix]
  (nth (nth matrix y nil) x nil))

(defn north-line [x y matrix]
  (->> (range 0 y)
       (map #(get-m x % matrix))))

(defn east-line [x y matrix]
  (->> (range (inc x) (count (first matrix)))
       (map #(get-m % y matrix))))

(defn south-line [x y matrix]
  (->> (range (inc y) (count matrix))
       (map #(get-m x % matrix))))

(defn west-line [x y matrix]
  (->> (range 0 x)
       (map #(get-m % y matrix))))

(defn visible? [x y matrix line-fn]
  (every? #(> (get-m x y matrix) %) (line-fn x y matrix)))

(defn visible-north? [x y matrix] (visible? x y matrix north-line))
(defn visible-south? [x y matrix] (visible? x y matrix south-line))
(defn visible-west? [x y matrix] (visible? x y matrix west-line))
(defn visible-east? [x y matrix] (visible? x y matrix east-line))

(defn score-calc [current-tree tree-path]
  (loop [[head & tail] tree-path
         acc 0]
    (cond
      (nil? head) acc
      (< head current-tree) (recur tail (inc acc))
      :else (inc acc))))

(defn north-score [x y matrix] (score-calc (get-m x y matrix) (reverse (north-line x y matrix))))
(defn west-score [x y matrix] (score-calc (get-m x y matrix) (reverse (west-line x y matrix))))
(defn south-score [x y matrix] (score-calc (get-m x y matrix) (south-line x y matrix)))
(defn east-score [x y matrix] (score-calc (get-m x y matrix) (east-line x y matrix)))

(defn scenic-score [x y matrix]
  (apply * (map #(% x y matrix) [north-score south-score west-score east-score])))

(defn find-visible [matrix]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (for [x (range 1 (dec max-x))
          y (range 1 (dec max-y))
          :let [current-tree (get-m x y matrix)
                visibles (map #(% x y matrix) [visible-north? visible-east? visible-south? visible-west?])]]
      (if (some true? visibles)
        current-tree
        nil))))

(defn find-best-scenic [matrix]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (->> matrix
         (scenic-score x y)
         (for [x (range 1 (dec max-x))
               y (range 1 (dec max-y))])
         (apply max))))

(defn read-matrix [input]
  (->> (str/split-lines input)
       (mapv #(str/split % #""))
       (mapv #(mapv read-string %))))

(defn p1 [input]
  (let [matrix (read-matrix input)
        max-y (count matrix)
        max-x (count (first matrix))
        inner-visible (count (remove nil? (find-visible matrix)))
        outer-visible (+ (* 2 max-x) (* 2 (- max-y 2)))]
    (+ inner-visible outer-visible)))

(defn p2 [input]
  (find-best-scenic (read-matrix input)))
