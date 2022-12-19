(ns aoc-22.day18
  (:require [clojure.string :as str]))

(def mods [[1 0 0]
           [-1 0 0]
           [0 1 0]
           [0 -1 0]
           [0 0 1]
           [0 0 -1]])

(defn comp [[x1 y1 z1] all-blocks xfn zfn yfn]
  (empty? (filter (fn [[x2 y2 z2]] (and (xfn x1 x2) (zfn z1 z2) (yfn y2 y1))) all-blocks)))

(def high-y? #(comp %1 %2 = = >))
(def low-y? #(comp %1 %2 = = <))
(def high-x? #(comp %1 %2  > = =))
(def low-x? #(comp %1 %2  < = =))
(def high-z? #(comp %1 %2  = > =))
(def low-z? #(comp %1 %2  = < =))

(defn outside? [air all-blocks]
  (reduce (fn [acc f] (or acc (f air all-blocks))) false [high-y? low-y? high-x? low-x? high-z? low-z?]))

(defn reachable-by-water? [[x y z :as air] all-blocks]
  (if (outside? air all-blocks)
    true
    (let [adjacent-air (->> (map (fn [[x-mod y-mod z-mod]] [(+ x x-mod) (+ y y-mod) (+ z z-mod)]) mods)
                            (filter #(not (contains? all-blocks %))))]
      (if (empty? adjacent-air)
        false
        (loop [[head & tail] adjacent-air
               visited #{air}]
          (if (nil? head)
            false
            (if (contains? visited head)
              false
              (if (outside? head all-blocks) true (recur tail (conj visited head))))))))))

(defn count-surrounding-air [[x y z] all-blocks]
  (->> (map (fn [[x-mod y-mod z-mod]] [(+ x x-mod) (+ y y-mod) (+ z z-mod)]) mods)
       (filter #(not (contains? all-blocks %)))
       (filter #(reachable-by-water? % all-blocks))
       (count)))

(defn count-sides [[x y z] all-blocks]
  (->> (map (fn [[x-mod y-mod z-mod]] [(+ x x-mod) (+ y y-mod) (+ z z-mod)]) mods)
       (filter #(contains? all-blocks %))
       count
       (- 6)))

(defn check [all-blocks count-fn]
  (loop [[block & tail] all-blocks
         acc 0]
    (if (nil? block)
      acc
      (recur tail (+ acc (count-fn block all-blocks))))))

(defn solve [input count-fn]
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (map #(map read-string %))
       (into #{})
       (#(check % count-fn))))

(defn p1 [input] (solve input count-sides))
(defn p2 [input] (solve input count-surrounding-air))
