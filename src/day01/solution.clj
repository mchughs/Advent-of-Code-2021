(ns day01.solution
  (:require [clojure.java.io :as io]))

;; Part 1

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        (map #(Integer. %))
        (partition 2 1)
        (reduce (fn [acc [x y]]
                  (if (> y x)
                    (inc acc)
                    acc))
                0))))

;; Part 2

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        (map #(Integer. %))
        (partition 3 1)
        (partition 2 1)
        (reduce (fn [acc [x y]]
                  (if (> (apply + y) (apply + x))
                    (inc acc)
                    acc))
                0))))
