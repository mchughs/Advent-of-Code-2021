(ns day10.solution
  "NOTE: Not super happy with this solution because of all the seq type transformations.
   We just want to use a stack-type here but we switch between lazyseq and vectors often in this solution."
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "src/day10/input.txt")
       str/split-lines
       (map (comp vec reverse))))

;; Part 1

(def opening
  #{\[ \( \{ \<})

(def incorrect-points
  {\] 57 \) 3 \} 1197 \> 25137})

(def match
  {\] \[
   \) \(
   \} \{
   \> \<})

(->> data
     (map (fn [coll]
            (loop [coll coll
                   stack []]
              (let [char (peek coll)]
                (cond
                  (contains? opening char)
                  (recur
                   (pop coll)
                   (conj stack char))

                  (= (peek stack)
                     (get match char))
                  (recur
                   (pop coll)
                   (pop stack))

                  (empty? coll)
                  true

                  :else
                  char)))))
     (reduce #(+ %1 (get incorrect-points %2 0)) 0))

;; Part 2

(def autocomplete-points
  {\[ 2 \( 1 \{ 3 \< 4})

(def sorted-scores
  (->> data
       (map (fn [coll]
              (loop [coll coll
                     stack []]
                (let [char (peek coll)]
                  (cond
                    (contains? opening char)
                    (recur
                     (pop coll)
                     (conj stack char))

                    (= (peek stack)
                       (get match char))
                    (recur
                     (pop coll)
                     (pop stack))

                    (empty? coll)
                    stack

                    :else
                    false)))))
       (filter identity)
       (map reverse)
       (map (partial reduce
                     #(+ (* 5 %1) (get autocomplete-points %2 0))
                     0))
       sort
       vec))

(get sorted-scores (/ (dec (count sorted-scores))
                      2))
