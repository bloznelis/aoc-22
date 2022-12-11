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
                       "+" +) % (case arg "old" % (read-string arg)))
     :test-fn #(zero? (mod % divisible-by))
     :if-true-target if-true-target
     :if-false-target if-false-target
     :thrown-cnt 0}))

(defn single-item [monkey-idx state]
  (let [monkey (state monkey-idx)
        item (first (:items monkey))
        worry-level ((:operation-fn monkey) item)
        worry-after (floor (/ worry-level 3))
        throw-target (if ((monkey :test-fn) worry-after) (:if-true-target monkey) (:if-false-target monkey))]

    (-> state
        (update-in [throw-target :items] conj worry-after)
        (update-in
         [(:monkey monkey) :items] #(into [] (rest %)))

        (update-in [(:monkey monkey) :thrown-cnt] inc))))

(defn turn [state monkey-idx]
  (loop [current-state state]
    (println "helo")
    (if (empty? (:items (current-state monkey-idx)))
      current-state
      (recur (single-item monkey-idx current-state)))))

(defn one-round [state]
  (reduce turn state (range (count state))))

(comment
  (def real-input (slurp "/home/lukas/code/aoc-22/input/day11.txt"))
  ((:operation-fn (state 1)) 4)

  (range 1)

  state

  (into {} (map #(vector (:monkey %) %) (map read-block (utils/parse-blocks test-input))))

  (defn run-rounds [amount state]
    (if (zero? amount)
      state
      (run-rounds (dec amount) (one-round state))))

  (->> real-input
       utils/parse-blocks
       (map read-block)
       (map #(vector (:monkey %) %))
       (into {})
       (run-rounds 20)
       vals
       (map :thrown-cnt)
       sort
       (take-last 2)
       (apply *)))
