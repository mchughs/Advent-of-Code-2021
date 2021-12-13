(ns day13.solution 
  (:require [clojure.string :as str]))

;; Part 1

(def data
  (as-> (slurp "src/day13/input.txt") $
    (str/split $ #"\n\n")
    (map str/split-lines $)))

(def instructions
  (let [[points-strs folds-strs] data
        points (map (comp
                     (partial map #(Integer. %))
                     #(str/split % #","))
                    points-strs)
        folds (map (comp
                    (fn [[_ axis n]]
                      {(keyword axis) (Integer. n)})
                    (partial re-matches #"fold along ([xy])=(\d+)"))
                   folds-strs)]
    {:points points
     :folds folds}))

(defn fold [points fold]
  (let [horizontal (get fold :y)
        vertical (get fold :x)]    
    (if horizontal
      (map (fn [[x y]]             
             [x
              (- horizontal
                 (Math/abs (- y horizontal)))])
           points)
      (map (fn [[x y]]
             [(- vertical
                 (Math/abs (- x vertical)))
              y])
           points))))

(->> (fold
      (:points instructions)
      (first (:folds instructions)))
     set
     count)

;; Part 2

(defn display! [points]
  (doseq [y (range (inc (last (apply max-key last points))))]
    (println
     (for [x (range (inc (first (apply max-key first points))))]
       (if (contains? points [x y])
         \#
         \.)))))

(->> (reduce
     fold
     (:points instructions)
     (:folds instructions))
     set
     display!)

;; AHGCPGAU for my input
