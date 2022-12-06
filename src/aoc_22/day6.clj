(ns aoc-22.day6)

(defn find-mark [signal threshold]
  (loop [remaining signal
         acc '()
         cnt 0]
    (cond
      (> threshold (count acc)) (recur (rest remaining) (conj acc (first remaining)) (inc cnt))
      (= threshold (count acc) (count (set acc))) cnt
      :else (recur (rest remaining) (conj (drop-last acc) (first remaining)) (inc cnt)))))

(defn p1 [input] (find-mark input 4))
(defn p2 [input] (find-mark input 14))
