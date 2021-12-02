(ns day02.solution
  (:require [clojure.java.io :as io]))

;; Part 1

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map (partial re-seq #"(\w+) (\d+)"))
        (map first)
        (reduce (fn [[x y] [_ direction int-string]]
                  (let [n (Integer. int-string)]
                    (case direction
                      "forward" [(+ x n) y]
                      "up" [x (- y n)]
                      "down" [x (+ y n)])))
                [0 0])
        (apply *))))

;; Part 2

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map (partial re-seq #"(\w+) (\d+)"))
        (map first)
        (reduce (fn [[position depth aim] [_ direction int-string]]
                  (let [n (Integer. int-string)]
                    (case direction
                      "forward" [(+ position n) (+ depth (* aim n)) aim]
                      "up" [position depth (- aim n)]
                      "down" [position depth (+ aim n)])))
                [0 0 0])
        (take 2)
        (apply *))))

