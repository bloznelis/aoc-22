(ns aoc-22.utils
  (:require [clojure.string :as str]))

(defn to-decimal [binary-seq]
  (->>  (reverse binary-seq)
        (map-indexed #(* %2 (Math/pow 2 %1)))
        (apply +)))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-ints [raw-input]
  (->>
   (str/split-lines raw-input)
   (map #(Integer/parseInt %))))

(defn tokenize [raw-input]
  (->>
   (str/split-lines raw-input)
   (mapv #(mapv read-string (str/split % #"")))))

(defn parse-blocks [raw-input]
  (str/split raw-input #"\n\n"))

(defn median [points]
  (let [sorted-points (sort points)
        size (count sorted-points)
        mid-point (/ size 2)]
    (if (odd? size)
      (nth sorted-points mid-point)
      (/ (+ (nth sorted-points mid-point) (nth sorted-points (dec mid-point))) 2))))

(defn num-list [raw-input]
  (->>
   (str/split raw-input #",")
   (mapv #(read-string %))))

(defn update-existing
  "Updates a value in a map given a key and a function, if and only if the key
  exists in the map."
  ([m k f]
   (if-let [kv (find m k)] (assoc m k (f (val kv))) m)))
