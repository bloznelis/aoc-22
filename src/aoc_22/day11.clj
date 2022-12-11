(ns aoc-22.day11
  (:require [aoc-22.utils :as utils]
            [clojure.string :as str]
            [clojure.math :refer [floor]]))

(def part (atom :p1))

(defn extract-num [string]
  (read-string (re-find #"[0-9]+" string)))

(defn read-block [block]
  (let [[monkey items op test if-true if-false] (str/split-lines block)
        [operation arg] (drop 6 (str/split op #" "))]
    {:monkey (extract-num monkey)
     :items (mapv read-string (re-seq #"[0-9]+" items))
     :operation-fn #((case operation
                       "*" *
                       "+" +) % (case arg "old" % (read-string arg)))
     :test-fn #(zero? (extract-num test))
     :if-true-target (extract-num if-true)
     :if-false-target (extract-num if-false)
     :thrown-cnt 0}))

(defn throw-item [monkey-idx state]
  (let [monkey (state monkey-idx)
        item (first (:items monkey))
        worry-level ((:operation-fn monkey) item)
        worry-after (if (= :p1 @part) (floor (/ worry-level 3)) (mod worry-level 9699690)) ;;magic number
        throw-target (if ((monkey :test-fn) worry-after) (:if-true-target monkey) (:if-false-target monkey))]

    (-> state
        (update-in [throw-target :items] conj worry-after)
        (update-in [(:monkey monkey) :items] #(into [] (rest %)))
        (update-in [(:monkey monkey) :thrown-cnt] inc))))

(defn turn [state monkey-idx]
  (loop [current-state state]
    (if (empty? (:items (current-state monkey-idx)))
      current-state
      (recur (throw-item monkey-idx current-state)))))

(defn one-round [state]
  (reduce turn state (range (count state))))

(defn run-rounds [amount state]
  (if (zero? amount)
    state
    (recur (dec amount) (one-round state))))

(defn solve [input rounds]
  (->> input
       utils/parse-blocks
       (map read-block)
       (map #(vector (:monkey %) %))
       (into {})
       (run-rounds rounds)
       vals
       (map :thrown-cnt)
       sort
       (take-last 2)
       (apply *)))

(defn p1 [input]
  (reset! part :p1)
  (solve input 20))

(defn p2 [input]
  (reset! part :p2)
  (solve input 10000))
