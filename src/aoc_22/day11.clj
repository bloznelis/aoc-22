(ns aoc-22.day11
  (:require [aoc-22.utils :as utils]
            [clojure.string :as str]
            [clojure.math :refer [floor]]))

(def test-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(def input-block "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3")

(re-find #"[0-9]+" "Monkey 11:")
(map read-string (re-seq #"[0-9]+" "Starting items: 79, 98"))

(drop 4 (str/split "Operation: new = old * 17" #" "))

(zero? (mod 4  4))

(defn read-block [block]
  (let [lines (str/split-lines block)
        [monkey items op test if-true if-false] lines
        monkey-idx (read-string (re-find #"[0-9]+" monkey))
        divisible-by (read-string (re-find #"[0-9]+" test))
        starting-items (mapv read-string (re-seq #"[0-9]+" items))
        [operation arg] (drop 6 (str/split op #" "))
        if-true-target (read-string (re-find #"[0-9]+" if-true))
        if-false-target (read-string (re-find #"[0-9]+" if-false))]
    {:monkey monkey-idx
     :items starting-items
     :operation-fn #((case operation
                       "*" *
                       "+" +) % (read-string arg))
     :test-fn #(zero? (mod % divisible-by))
     :if-true-target if-true-target
     :if-false-target if-false-target}))

(read-block input-block)

(def state (into {} (map #(vector (:monkey %) %) (map read-block (utils/parse-blocks test-input)))))

(defn single-item [monkey-idx state]
  (let [monkey (state monkey-idx)
        item (first (:items monkey))
        worry-level ((:operation-fn monkey) item)
        worry-after (floor (/ worry-level 3))
        throw-target (if ((monkey :test-fn) worry-after) (:if-true-target monkey) (:if-false-target monkey))]

    (-> state
        (update-in [throw-target :items] conj worry-after)
        (update-in
         [(:monkey monkey) :items] #(into [] (rest %))))))

(rest [1 2 3])

(into [] (rest [1 2 3]))

(single-item 0 state)

(defn turn [state monkey-idx]
  (loop [current-state state]
    (if (empty? (:items (current-state monkey-idx)))
      state
      (recur  (single-item monkey-idx current-state)))))

(defn one-round [state]
  (reduce turn state (range (count state))))

((:operation-fn (state 1)) 4)

state

(into {} (map #(vector (:monkey %) %) (map read-block (utils/parse-blocks test-input))))

(one-round (into {} (map #(vector (:monkey %) %) (map read-block (utils/parse-blocks test-input)))))
