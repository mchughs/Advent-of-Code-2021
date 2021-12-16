(ns day15.solution 
  (:require [clojure.string :as str]))

;; Using https://gist.github.com/loganlinn/5437067 for Dijstra's implementation

(def ^:private inf (Long/MAX_VALUE))

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
     (fn [c [nbr nbr-cost]]
       (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
     costs
     (neighbors g curr unvisited))))

(defn dijkstra
  "Returns a mapping of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
  [g src & {:keys [target]}]
  (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
         curr src
         unvisited (disj (apply hash-set (keys g)) src)]        
    (if (or (empty? unvisited) (= inf (costs curr)))
      costs
      (let [costs' (update-costs g costs curr unvisited)
            curr' (first (sort-by costs' unvisited))]
        (if (= target curr)
          (costs' target)
          (recur costs'
                 curr'
                 (disj unvisited curr')))))))

;; Part 1

(def data
  (->> (slurp "src/day15/input.txt")
       str/split-lines
       (map-indexed
        (fn [y s]
          (->> s
               (map-indexed
                (fn [x char]
                  (hash-map [x y] (Integer. (str char)))))
               (apply merge))))
       (apply merge)))

(def max-x
  ((comp first key)
   (apply max-key (comp first key) data)))

(def max-y
  ((comp last key)
   (apply max-key (comp last key) data)))

(defn gen-neighbors [[x y]]
  (->>
   ;; we'll assume that we'll always take a step towards the exit, never away.
   (list [(inc x) y]
         #_[(dec x) y]
         [x (inc y)]
         #_[x (dec y)])
   (filter (fn [[x' y']]
             (and (< -1 x' (inc max-x))
                  (< -1 y' (inc max-y)))))))

(def all-points
  (for [x (range (inc max-x))
        y (range (inc max-y))]
    [x y]))

(def graph
  (reduce (fn [m point]
            (assoc m point
                   (reduce
                    #(assoc %1 %2 (get data %2))
                    {}
                    (gen-neighbors point))))
          {}
          all-points))

(time
 (dijkstra graph [0 0] :target [max-x max-y]))

#_#_#_#_#_
;; Part 2

(def max-x'
  (+ 40 max-x))

(def max-y'
  (+ 40 max-y))

(defn gen-neighbors' [[x y]]
  (->>
   (list [(inc x) y]
         #_[(dec x) y]
         [x (inc y)]
         #_[x (dec y)])
   (filter (fn [[x' y']]
             (and (< -1 x' (inc max-x'))
                  (< -1 y' (inc max-y')))))))

(def all-points'
  (for [x (range (inc max-x'))
        y (range (inc max-y'))]
    [x y]))

(defn mod' [num div]
  (nth (cycle (range 1 div))
       (dec num)))

#_
(def graph'
  (reduce (fn [m point]            
            (assoc m point
                   (reduce
                    #(let [[x y] %2
                           remainder-x (mod x 10)
                           remainder-y (mod y 10)
                           quotient-x (quot x 10)
                           quotient-y (quot y 10)
                           value (mod' (+ quotient-x
                                          quotient-y
                                          (get data [remainder-x remainder-y]))
                                       10)]
                       (assoc %1 %2 value))
                    {}
                    (gen-neighbors' point))))
          {}
          all-points'))

#_#_
(time
 (dijkstra graph' [0 0] :target [max-x' max-y']))

(clojure.pprint/pprint graph')
