(ns aoc-22.day2
  (:require [clojure.string :as str])
  (:require [clojure.set :as s]))

(def beats
  {:scissors :paper, :paper :rock, :rock :scissors})

(def shape-score-table
  {:rock 1, :paper 2, :scissors 3})

(def outcome-score-table
  {:loser 0, :draw 3, :winner 6})

(def parser-rules-p1
  {"A" :rock, "B" :paper, "C" :scissors, "X" :rock, "Y" :paper, "Z" :scissors})

(def parser-rules-p2
  {"A" :rock, "B" :paper, "C" :scissors, "X" :lose, "Y" :draw, "Z" :win})

(defn calc-outcome [elf-shape my-shape]
  (if (= elf-shape my-shape)
    :draw
    (if (= (elf-shape beats) my-shape) :loser :winner)))

(defn calc-score [[elf-shape my-shape]]
  (let [shape-score (shape-score-table my-shape)
        result-score (outcome-score-table (calc-outcome elf-shape my-shape))]

    (+ shape-score result-score)))

(defn decide-sign [elf-sign desired-outcome]
  (case desired-outcome
    :lose (elf-sign beats)
    :win (elf-sign (s/map-invert beats))
    :draw elf-sign))

(defn read-input [input rules]
  (->> (str/split-lines input)
       (map #(->>
              (str/split % #" ")
              (map rules)))))

(defn part1 [input]
  (->> (read-input input parser-rules-p1)
       (map calc-score)
       (apply +)))

(defn part2 [input]
  (->> (read-input input parser-rules-p2)
       (map (fn [[elf-shape desired-outcome]] [elf-shape (decide-sign elf-shape desired-outcome)]))
       (map calc-score)
       (apply +)))
