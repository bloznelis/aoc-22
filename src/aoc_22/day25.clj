(ns aoc-22.day25
  (:require [clojure.string :as str]))

(defn decode [snafu]
  (->> (str/split snafu #"")
       reverse
       (map #(case %
               "=" -2
               "-" -1
               (read-string %)))
       (map-indexed #(* %2 (Math/pow 5 %1)))
       (apply +)))

(defn encode [decimal]
  (loop [decimal (long decimal)
         acc ""]
    (if (zero? decimal)
      acc
      (let [char (case (mod (+ decimal 2) 5)
                   0 \=
                   1 \-
                   2 \0
                   3 \1
                   4 \2)]

        (recur (long (/ (+ decimal 2) 5)) (str char acc))))))

(defn solve [input]
  (->> (str/split-lines input)
       (map decode)
       (apply +)
       encode))
