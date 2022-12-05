(ns aoc-22.day5
  (:require [aoc-22.utils :as utils])
  (:require [clojure.string :as str]))

(def crane-version (atom :9000))

(def box-indexes
  (map vector (iterate inc 1) (iterate #(+ % 4) 1)))

(defn parse-stacks-line [line initial-stacks]
  (loop [idx-seq box-indexes
         stacks initial-stacks]
    (let [[stack-idx str-idx] (first idx-seq)
          box (get line str-idx)]
      (cond
        (nil? box) stacks
        (= \space box) (recur (drop 1 idx-seq) stacks)
        :else (recur (drop 1 idx-seq) (update stacks stack-idx conj box))))))

(defn parse-stacks [lines]
  (loop [current-lines (reverse (drop-last (str/split-lines lines)))
         state {}]
    (if (empty? current-lines)
      state
      (recur (rest current-lines) (parse-stacks-line (first current-lines) state)))))

(defn parse-rule-line [line]
  (let [[amount from to] (map read-string (re-seq #"[0-9]+" line))]
    {:amount amount, :from from, :to to}))

(defn parse-rules [block]
  (map parse-rule-line (str/split-lines block)))

(defn apply-rule [stacks rule]
  (let [{amount :amount, origin :from, target :to} rule
        boxes (take amount (stacks origin))]

    (-> stacks
        (update origin #(drop amount %))
        (update target #(concat (if (= @crane-version :9000) (reverse boxes) boxes) %)))))

(defn apply-rules [[stacks rules]]
  (loop [current-stacks stacks
         current-rules rules]
    (if (empty? current-rules)
      current-stacks
      (recur (apply-rule current-stacks (first current-rules)) (rest current-rules)))))

(defn solve [input]
  (->> (utils/parse-blocks input)
       (#(list (parse-stacks (first %)) (parse-rules (second %))))
       apply-rules
       (into (sorted-map))
       vals
       (map first)
       (str/join)))

(defn p1 [input]
  (reset! crane-version :9000)
  (solve input))

(defn p2 [input]
  (reset! crane-version :9001)
  (solve input))
