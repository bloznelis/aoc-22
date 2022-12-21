(ns aoc-22.day21
  (:require [clojure.string :as str]))

(def eval-table (atom {}))

(defn read-line [line]
  (let [[address a b c] (str/split line #" ")
        addr (keyword (str/join (drop-last address)))]

    (if (nil? c)
      {addr (read-string a)}
      {addr {:operation (eval (symbol b)) :left-addr (keyword a) :right-addr (keyword c)}})))

(defn init [input]
  (reset! eval-table (->> (str/split-lines input)
                          (map read-line)
                          (apply conj))))

(defn eval-expr [address]
  (let [op (address @eval-table)
        evaluated (if (map? (address @eval-table))
                    ((:operation op) (eval-expr (:left-addr op)) (eval-expr (:right-addr op)))
                    op)]
    (swap! eval-table #(conj % {address evaluated}))
    evaluated))

(defn find-dep [table key]
  (->> table
       (filter #(map? (second %)))
       (filter (fn [[_ right]] (or (= key (:left-addr right)) (= key (:right-addr right)))))
       first
       first))

(defn find-dep-path [table key]
  (loop [current-dep key
         acc []]
    (if (nil? current-dep)
      acc
      (recur (find-dep table current-dep) (conj acc current-dep)))))

(defn re-express [[current next] table]
  (let [{op :operation, left :left-addr, right :right-addr} (current table)]
    (cond
      (= left next) (cond
                      (= op /) {left {:operation *, :left-addr current, :right-addr right}}
                      (= op *) {left {:operation /, :left-addr current, :right-addr right}}
                      (= op +) {left {:operation -, :left-addr current, :right-addr right}}
                      (= op -) {left {:operation +, :left-addr current, :right-addr right}})
      (= right next) (cond
                       (= op /) {right {:operation /, :left-addr left, :right-addr current}}
                       (= op *) {right {:operation /, :left-addr current, :right-addr left}}
                       (= op +) {right {:operation -, :left-addr current, :right-addr left}}
                       (= op -) {right {:operation -, :left-addr left, :right-addr current}}))))

(defn p1 [input]
  (init input)
  (eval-expr :root))

(defn p2 [input]
  (let [initial (init input)
        human-expressed (->> :humn
                             (find-dep-path initial)
                             reverse
                             (partition 2 1)
                             (map #(re-express % initial))
                             (reduce conj))]
    (eval-expr :root)
    (swap! eval-table conj human-expressed)

    (swap! eval-table conj {:zero 0})
    (swap! eval-table conj {:lvvf {:operation +, :left-addr :zero, :right-addr :rqgq}})

    (eval-expr :humn)))
