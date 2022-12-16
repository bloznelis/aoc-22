(ns aoc-22.day12
  (:require [clojure.string :as str]))

(defn to-matrix-map [input]
  (->> (str/split-lines input)
       (map-indexed (fn [column-idx row] (->> (str/split row #"")
                                              (map-indexed (fn [row-idx [char & _]] {[row-idx column-idx] char}))
                                              (apply conj))))
       (apply conj)))

(defn valid-neighbours [[x y] matrix]
  (for [[x-mul y-mul] [[0 1] [1 0] [-1 0] [0 -1]]
        :let [neigh-coords [(+ x x-mul) (+ y y-mul)]
              neigh (matrix neigh-coords)]
        :when (and (some? neigh) (<= (- (int (matrix [x y])) (int neigh)) 1))]
    neigh-coords))

(defn find-start-end [matrix]
  (let [[start _] (first (filterv #(= (second %) \S) matrix))
        lows (into #{} (map first (filterv #(= (second %) \a) matrix)))
        [end _] (first (filter #(= (second %) \E) matrix))
        replaced (-> (assoc matrix start \a)
                     (assoc end \z))]

    {:lows lows :start start :best-signal-at end :height-map replaced}))

(defn attach-neighbours [matrix]
  (->> matrix
       (map (fn [[coords height]] {coords {:height height, :neighbours (valid-neighbours coords matrix)}}))
       (into {})))

(defn make-state [input]
  (let [{lows :lows start :start, best-signal-at :best-signal-at, height-map :height-map} (find-start-end (to-matrix-map input))
        matrix (attach-neighbours height-map)]

    {:unvisited (into #{} (keys matrix))
     :distances (into {} (map #(vector % (if (= % best-signal-at) 0 Long/MAX_VALUE)) (keys matrix)))
     :origin best-signal-at
     :start start
     :matrix matrix
     :lows lows}))

(defn update-distances [old-distances new-distances]
  (reduce (fn [distances [coords distance]]
            (if (> (distances coords) distance)
              (assoc distances coords distance)
              distances)) old-distances new-distances))

(defn lookup-neighs [state current-node]
  (let [current-distance (get-in state [:distances current-node])
        neighs (get-in state [:matrix current-node :neighbours])
        neigh-distances (into {} (map #(vector % (inc current-distance)) neighs))]

    (-> state
        (update :distances update-distances neigh-distances)
        (update :unvisited disj current-node))))

;; Priority queue would be nice here
(defn get-closest-unvisited [state]
  (first (first (sort-by second (map #(vector % (get-in state [:distances %])) (:unvisited state))))))

(defn search [state]
  (let [current-node (get-closest-unvisited state)]

    (if (or (and (= (:part state) :1) (= (:start state) current-node)) (and (= (:part state) :2) (contains? (:lows state) current-node)))
      (get-in state [:distances current-node])
      (recur (lookup-neighs state current-node)))))

(defn p1 [input] (search (assoc (make-state input) :part :1)))
(defn p2 [input] (search (assoc (make-state input) :part :2)))
