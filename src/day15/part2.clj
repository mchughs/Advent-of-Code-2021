(ns day15.part2
  "Solution taken from https://github.com/wevre/advent-of-code/blob/master/src/advent_of_code/2021/day_15.clj
   Couldn't solve part 2 with my dijstra's implementation, nor an A* implementation I tried.
   If anyone reading this see's why my part 2 wouldn't work with my part 1 code, let me know."
  (:require [clojure.data.priority-map :refer [priority-map]]))

;;NOTE: I tried a manhattan distance penalty tacked on to the distance, but it
;;      made no difference, so I removed it.

(def start [0 0])
(def dim 100)
(def scale 1)

(defn parse-input "Return a vector of risk values." [s]
  (->> (re-seq #"\d" s) (map #(Integer/parseInt %))))

(defn risk<-loc [coll [r c]]
  (let [wrap (fn [x d] (if (< x dim) [x d] (recur (- x dim) (inc d))))
        [nr dr] (wrap r 0)
        [nc dc] (wrap c 0)
        v (+ dr dc (nth coll (+ nc (* nr dim))))]
    (inc (mod (dec v) 9))))

(defn neighbors [[r c]]
  (let [upp (dec (* scale dim))]
    (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]
          :let [fr (+ r dr) fc (+ c dc)]
          :when (and (<= 0 fr upp) (<= 0 fc upp))]
      [fr fc])))

(defn update-distance [risks dist]
  (fn [distances loc]
    (update distances loc (fnil min ##Inf) (+ dist (risk<-loc risks loc)))))

(defn lowest-risk [risks]
  (let [upp (dec (* scale dim))
        end [upp upp]]
    (loop [distances (priority-map start 0) visited #{}]
      (let [[node dist] (first distances)
            neighbors (->> node neighbors (remove visited))]
        (if (= node end)
          {:dist (distances node)
           :untouched (- (* scale scale (count risks)) (count visited) (count distances))}
          (recur (-> (reduce (update-distance risks dist) distances neighbors)
                     (dissoc node))
                 (conj visited node)))))))

(comment
  (with-redefs [dim 10 scale 5]
    (time
     (->
      (parse-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
      (lowest-risk))))

  ;; puzzle 1
  (time
   (lowest-risk (parse-input (slurp "src/day15/input.txt"))))
  ;;=> 811 "Elapsed time: 2402.363662 msecs"

  ;; puzzle 2
  (time
   (with-redefs [scale 5]
     (lowest-risk (parse-input (slurp "src/day15/input.txt")))))
  ;;=> 3012 "Elapsed time: 57542.997612 msecs"
  )
