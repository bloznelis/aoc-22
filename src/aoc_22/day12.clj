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

(comment
  (def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

;; do I need to attach neighbours as part of each node information or
  ;; can it be found ad-hoc?

  (attach-neighbours (:height-map (find-start-end (to-matrix-map test-input))))

  (valid-neighbours [0 0] (to-matrix-map test-input))
  (int ((to-matrix-map test-input) [0 1]))

  (- (int \a) (int \b))
  (char 83)
  (int (read-string "a"))

  (defn make-priority-queue-2 [m]
    (into (sorted-set-by compare) m))

  (def priority-queue
    (make-priority-queue-2 [[2 :A] [2 :B] [1 :D]]))

  priority-queue
;; => #{[2 :C] [5 :A] [10 :B]}
  (first priority-queue)
;; => [2 :C]
  (rest priority-queue)
;; => ([5 :A] [10 :B])
  (conj priority-queue [0 :C])
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
