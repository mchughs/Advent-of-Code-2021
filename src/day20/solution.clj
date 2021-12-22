(ns day20.solution
  (:require [clojure.string :as s]))

(def data
  (-> (slurp "src/day20/input.txt")
      (s/split #"\n\n")))

(def image-enhancement-algorithm
  (first data))

(def init-input-image
  (-> data
      last
      s/split-lines))

(def pixel->bit
  {\. 0
   \# 1})

(defn convolute [toggle? input-image [x y]]
  (let [points (for [dx [-1 0 1]
                     dy [-1 0 1]]
                 [(+ x dx) (+ y dy)])
        binary-str (->> points
                        (sort-by last)
                        (map (fn [[x' y']]
                               (let [inf-edges (if toggle? \. \#)
                                     val (-> input-image
                                             (get y')
                                             (get x' inf-edges))]
                                 (pixel->bit val))))
                        (apply str))
        idx (Integer/parseInt binary-str 2)]
    (get image-enhancement-algorithm idx)))

(defn format-to-square [pixels]
  (let [side-length (int (Math/sqrt (count pixels)))
        rows (partition side-length pixels)]
    (mapv vec rows)))

(defn count-bright-pixels [output-image]
  (-> output-image
      flatten
      frequencies
      (get \#)))

(defn enhance [{:keys [toggle?]} input-image]
  (->> (for [y (range -1 (inc (inc (count input-image))))
             x (range -1 (inc (inc (count (first input-image)))))]
         [x y])       
       (map (partial convolute toggle? input-image))
       format-to-square))

;; Part 1

(->> init-input-image
     (enhance {:toggle? true})
     (enhance {:toggle? false})
     count-bright-pixels
     time)

;; Part 2

(let [applications 50]
  (->> init-input-image
       (iterate
        (comp
         (partial enhance {:toggle? false})
         (partial enhance {:toggle? true})))
       (take (inc (quot 50 2)))
       last
       count-bright-pixels
       time))
