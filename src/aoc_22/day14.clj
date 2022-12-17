(ns aoc-22.day14
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (partition 2 1 (partition 2 (map read-string (re-seq #"[0-9]+" line)))))

(defn interpolate-line [[[start-x start-y] [end-x end-y]]]
  (let [[start-x end-x] (if (< start-x end-x) [start-x end-x] [end-x start-x])
        [start-y end-y] (if (< start-y end-y) [start-y end-y] [end-y start-y])]

    (for [x (range start-x (inc end-x))
          y (range start-y (inc end-y))]
      [x y])))

(defn read-rock [input]
  (->> (str/split-lines input)
       (mapcat #(mapcat interpolate-line (parse-line %)))
       (into #{})))

(defn find-lowest-rock-y [rock]
  (apply max (map second rock)))

;; Part 1

(defn try-move-sand [occupied [sand-x sand-y]]
  (cond
    (not (contains? occupied [sand-x (inc sand-y)])) [sand-x (inc sand-y)]
    (not (contains? occupied [(dec sand-x) (inc sand-y)])) [(dec sand-x) (inc sand-y)]
    (not (contains? occupied [(inc sand-x) (inc sand-y)])) [(inc sand-x) (inc sand-y)]
    :else :stop))

(defn drop-sand-unit [state]
  (loop [sand-at [500 0]]
    (let [move-result (try-move-sand (:occupied state) sand-at)]
      (cond
        (= move-result :stop) (-> (update state :occupied conj sand-at)
                                  (update :sand-units-count inc))
        (> (second move-result) (:lowest-rock-y state)) :endless
        :else (recur move-result)))))

(defn run [state]
  (let [state-after-drop (drop-sand-unit state)]
    (case state-after-drop
      :endless (:sand-units-count state)
      (recur state-after-drop))))

(defn p1 [input]
  (let [rock (disj (read-rock input))
        state {:occupied rock
               :lowest-rock-y (find-lowest-rock-y rock)
               :sand-units-count 0}]
    (run state)))

;; Part 2

(defn free? [occupied floor-y [_ y :as coords]]
  (not (or (= floor-y y) (contains? occupied coords))))

(defn try-move-sand-2 [occupied floor-y [sand-x sand-y]]
  (cond
    (free? occupied floor-y [sand-x (inc sand-y)]) [sand-x (inc sand-y)]
    (free? occupied floor-y [(dec sand-x) (inc sand-y)]) [(dec sand-x) (inc sand-y)]
    (free? occupied floor-y  [(inc sand-x) (inc sand-y)]) [(inc sand-x) (inc sand-y)]
    (contains? occupied [sand-x sand-y]) :clogged
    :else :stop))

(defn drop-sand-unit-2 [state]
  (loop [sand-at [500 0]]
    (let [move-result (try-move-sand-2 (:occupied state) (:floor-y state) sand-at)]
      (cond
        (= move-result :clogged) :clogged
        (= move-result :stop) (-> (update state :occupied conj sand-at)
                                  (update :sand-units-count inc))
        :else (recur move-result)))))

(defn run-2 [state]
  (let [state-after-drop (drop-sand-unit-2 state)]
    (case state-after-drop
      :clogged (:sand-units-count state)
      (recur state-after-drop))))

(defn p2 [input]
  (let [rock (disj (read-rock input))
        state {:occupied rock
               :floor-y (+ (find-lowest-rock-y rock) 2)
               :sand-units-count 0}]
    (run-2 state)))
