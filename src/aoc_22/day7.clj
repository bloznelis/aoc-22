(ns aoc-22.day7
  (:require [clojure.string :as str]))

(defn drop-file-lines [lines]
  (drop-while #(not (str/starts-with? % "$")) lines))

(defn take-file-lines [lines]
  (take-while #(not (str/starts-with? % "$")) lines))

(defn fill-fs-single-line [pwd fs line]
  (let [[info name] (str/split line #" ")
        node-path (conj pwd name)
        node (case info
               "dir" {:node :dir}
               {:node :file, :size (read-string info)})]

    (-> fs
        (conj {node-path node})
        (update pwd #(update % :child-paths conj node-path)))))

(defn fill-fs [pwd fs-acc lines]
  (loop [[head & tail] lines
         acc fs-acc]
    (if (nil? head) acc (recur tail (fill-fs-single-line pwd acc head)))))

(defn scan-fs [input]
  (loop [[head & tail] (str/split-lines input)
         fs {["/"] {:node :dir}}
         pwd []]
    (cond
      (nil? head) fs
      (str/starts-with? head "$")
      (let [[_ cmd arg] (str/split head #" ")]
        (case cmd
          "cd" (recur tail fs (if (= ".." arg) (pop pwd) (conj pwd arg)))
          "ls" (recur (drop-file-lines tail) (fill-fs pwd fs (take-file-lines tail)) pwd))))))

(def dir-size-cache (atom {})) ;; sue me

(defn calculate-dir-size [fs dir-path]
  (let [children-paths (:child-paths (fs dir-path))
        cached-size (@dir-size-cache dir-path)
        computed-size (->> children-paths
                           (map #(case (:node (fs %))
                                   :dir (calculate-dir-size fs %)
                                   :file (:size (fs %))))
                           (apply +))
        dir-size (or cached-size computed-size)]
    (swap! dir-size-cache conj {dir-path dir-size})
    dir-size))

(defn find-dir-to-delete [to-clean]
  (->> (vals @dir-size-cache)
       (map #(vector % (- % to-clean)))
       (filter #(pos? (second %)))
       (sort-by second)
       first
       first))

(defn p1 [input]
  (let [fs (scan-fs input)]
    (->> fs
         keys
         (map #(calculate-dir-size fs %))
         (filter #(>= 100000 %))
         (apply +))))

(defn p2 [input]
  (let [fs (scan-fs input)
        free (- 70000000 (calculate-dir-size fs ["/"]))
        to-clean (- 30000000 free)]
    (find-dir-to-delete to-clean)))
