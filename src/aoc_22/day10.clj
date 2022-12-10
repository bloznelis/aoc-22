(ns aoc-22.day10
  (:require [clojure.string :as str]))

(defn handle-line [state line]
  (let [[cmd value] (str/split line #" ")
        [prev-cycle  prev-register] (last state)
        {prev-after :after} prev-register]
    (case cmd
      "noop" (conj state [(inc prev-cycle) {:during prev-after, :after prev-after}])
      "addx" (reduce conj state [[(inc prev-cycle) {:during prev-after, :after prev-after}]
                                 [(+ 2 prev-cycle) {:during prev-after, :after (+ prev-after (read-string value))}]]))))

(defn compute-states [input]
  (->> input
       str/split-lines
       (reduce handle-line [[0 {:during 1, :after 1}]])
       (into {})))

(defn draw-pixel [cycle states]
  (let [X (get-in states [cycle :during])
        crt-pos (mod (dec cycle) 40)]
    (if (or (= (dec X) crt-pos) (= (inc X) crt-pos) (= X crt-pos))
      "#"
      ".")))

(defn draw-crt-row [row states]
  (map #(draw-pixel % states) row))

(defn p1 [input]
  (let [states (compute-states input)]
    (->> (iterate #(+ 40 %) 20)
         (take 6)
         (map #(* % (get-in states [% :during])))
         (apply +))))

(defn p2 [input]
  (let [states (compute-states input)]
    (->> (partition 40 (range 1 241))
         (map #(str/join (draw-crt-row % states)))
         (str/join "\n")
         print)))
