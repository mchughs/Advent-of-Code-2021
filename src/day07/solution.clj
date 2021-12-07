(ns day07.solution
  (:require [clojure.string :as str]))

;; Part 1

(def crab-positions
  (mapv #(Integer. %)
        (-> (slurp "src/day07/input.txt")
            (str/trim)
            (str/split #","))))

(defn find-min-dist [coll]
  (let [candidates (range (apply min coll) (inc (apply max coll)))]
    (apply min-key
           :dist
           (for [center candidates]
             {:dist (reduce #(+ %1 (Math/abs (- %2 center))) 0 coll)
              :center center}))))

(time (find-min-dist crab-positions))

;; Part 2

(defn find-min-dist' [coll]
  (let [candidates (range (apply min coll) (inc (apply max coll)))]
    (apply min-key
           :dist
           (for [center candidates]
             {:dist (reduce (fn [acc v]
                              (let [n (Math/abs (- v center))
                                    sum-of-arithmetic-progression (/ (* n (+ 1 n)) 2)]                                
                                (+ acc sum-of-arithmetic-progression)))
                            0 coll)
              :center center}))))

(time (find-min-dist' crab-positions))
