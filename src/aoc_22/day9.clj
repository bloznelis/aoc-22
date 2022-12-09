(ns aoc-22.day9
  (:require [clojure.string :as str]))

(defn expand-instructions [instructions]
  (mapcat #(repeat (read-string (second %)) (first %)) instructions))

(defn straight-knot-move [[head-x head-y] [tail-x tail-y]]
  (cond
    (> head-x tail-x) [inc identity]
    (< head-x tail-x) [dec identity]
    (> head-y tail-y) [identity inc]
    (< head-y tail-y) [identity dec]
    :else [identity identity]))

(defn apply-modifiers [state knot-idx [mod-x mod-y]]
  (let [[knot-x knot-y] (nth (:knots state) knot-idx)]
    (update state :knots #(assoc % knot-idx [(mod-x knot-x) (mod-y knot-y)]))))

(defn move-head [state instruction]
  (apply-modifiers state 0 (case instruction
                             "R" [inc identity]
                             "D" [identity dec]
                             "U" [identity inc]
                             "L" [dec identity])))

(defn move-knot [state knot-idx]
  (let [[head-x head-y :as head] (nth (:knots state) (dec knot-idx))
        [knot-x knot-y :as knot] (nth (:knots state) knot-idx)
        x-diff (abs (- head-x knot-x))
        y-diff (abs (- head-y knot-y))]

    (->> (cond
           (and (>= 1 x-diff) (>= 1 y-diff)) [identity identity]
           (or (zero? x-diff) (zero? y-diff)) (straight-knot-move head knot)
           :else [(if (> head-x knot-x) inc dec) (if (> head-y knot-y) inc dec)])
         (apply-modifiers state knot-idx))))

(defn register-tail [state]
  (update state :tail-visits #(conj % (last (:knots state)))))

(defn process-instruction [state instruction]
  (-> (reduce move-knot (move-head state instruction) (take (dec (count (:knots state))) (iterate inc 1)))
      register-tail))

(defn solve [input initial-state]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       expand-instructions
       (reduce process-instruction initial-state)
       :tail-visits
       count))

(defn init [knots-count] {:knots (into [] (repeat knots-count [0 0])) :tail-visits #{[0 0]}})

(defn p1 [input]
  (solve input (init 2)))

(defn p2 [input]
  (solve input (init 10)))
