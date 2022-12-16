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
        :when (and (some? neigh) (> (- (int (matrix [x y])) (int neigh)) -1))]
    neigh-coords))

(defn find-start-end [matrix]
  (let [[start _] (first (filterv #(= (second %) \S) matrix))
        [end _] (first (filter #(= (second %) \E) matrix))
        replaced (-> (assoc matrix start \a)
                     (assoc end \z))]

    {:start start :end end :height-map replaced}))

(defn attach-neighbours [matrix]
  (->> matrix
       (map (fn [[coords height]] {coords {:height height, :neighbours (valid-neighbours coords matrix)}}))
       (into {})))

(defn make-priority-queue-2 [m]
  (into (sorted-set-by compare) m))

(defn make-priority-queue-3 [results]
(into (sorted-map-by (fn [key1 key2]
                         (compare [(get results key1) key1]
                                  [(get results key2) key2]
                                  )))
        results))

(defn prepare-state [origin target matrix]
  {:visited []
   :unvisited (make-priority-queue-3 (map #(vector (if (= % origin) 0 Long/MAX_VALUE) %) (keys matrix)))
   :distances (into {} (map #(vector % (if (= % origin) 0 Long/MAX_VALUE)) (keys matrix)))
   :origin origin
   :target target
   :matrix matrix})

(defn make-state [input]
  (let [{start :start, end :end, height-map :height-map} (find-start-end (to-matrix-map input))]

    (prepare-state start end height-map)))

(defn lookup-neighs [state neighs current-distance]
  (let [neigh-distances (into {} (map #(vector % (inc current-distance)) neighs))
        unvisited-update (map #((second %) (first %)) neigh-distances)]

    (-> state
        (update :distances conj neigh-distances)
        (update :unvisited #(apply conj %) unvisited-update)
        (update :)

        )))

(defn search [state]
  (loop [current-node (first (:unvisited state))
         neighs (get-in state [:matrix current-node :neighbours])]
    (if (= current-node (:target state))
      (get-in state [:distances (:target state)])
      ())))

(comment
  (def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

  (conj {:A 10, :B 4} {:C 2, :A 2})

  (def real-input (slurp "/home/lukas/code/clojure/aoc-22/input/day12.txt"))

  (make-state test-input)

  (attach-neighbours (:height-map (find-start-end (to-matrix-map test-input))))

  (valid-neighbours [0 0] (to-matrix-map test-input))
  (int ((to-matrix-map test-input) [0 1]))

  (- (int \a) (int \b))
  (char 83)
  (int (read-string "a"))

  (into (sorted-map-by))

  (def priority-queue
    (make-priority-queue-3 {:A 2, :B 3, :C 1}))

  priority-queue
;; => #{[2 :C] [5 :A] [10 :B]}
  (first priority-queue)
;; => [2 :C]
  (rest priority-queue)
;; => ([5 :A] [10 :B])
  (assoc priority-queue :B 1 )
  (make-priority-queue-3 (apply conj priority-queue [[:C 1] [:B 1]]))

;; => #{[2 :C] [4 :X] [5 :A] [10 :B] [11 :D]}
  (read-string "[[1],[2,3,4]]")
;; => [[1] [2 3 4]]
;;
  (map vector [1 2 3] [4 5 6])

  (defn compare [[a b]]
    (cond
      (and (int? a) (int? b)) :ints
      (and (coll? a) (coll? b)) :cols))

  (def origin-coords [0 0])

  (def visited-nodes '())
  (def unvisited-nodes (make-priority-queue [[0 [0 0]] [1 [0 1]]]))

  (def origin-distances {origin-coords 0})
  (def graph {origin-coords {:neighbours [{:coords [0 1] :weight 1}, {:coords [1 0] :weight 1}]}})

  (defn get-neighboars [graph coords]
    (get-in graph [coords :neighbours]))

  (get-neighboars graph [0 0])

  (defn get-neighboar-distances [graph distances coords]
    (let [neighbours (get-neighboars graph coords)
          current-dist (distances coords)]

      (map #(conj % {:distance (+ current-dist (:weight %))}) neighbours)))

  (get-neighboar-distances graph origin-distances [0 0])

  (defn update-distances [distances neigbours]
    (apply conj distances (map #(hash-map (:coords %) (:distance %)) neigbours)))

  (update-distances origin-distances (get-neighboar-distances graph origin-distances [0 0]))

;;
  )
