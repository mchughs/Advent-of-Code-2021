(ns day05.solution
  (:require
   [clojure.string :as str]))

;; Part 1

(def pairs
  (->> (slurp "src/day05/input.txt")
       str/split-lines
       (map #(str/split % #" -> "))))

(defn points [points]
  (let [[[x1 y1] [x2 y2]] (map (comp
                                (partial map #(Integer. %))
                                #(str/split % #","))
                               points)]
    (cond
      (= x1 x2)
      (for [y (apply range (-> (list y1 y2)
                               sort
                               vec
                               (update 1 inc)))]
        [x1 y])
      (= y1 y2)
      (for [x (apply range (-> (list x1 x2)
                               sort
                               vec
                               (update 1 inc)))]
        [x y1])
      :else '())))

(def overlaps (->> pairs
                   (mapcat points)
                   frequencies
                   (filter (fn [[_ v]] (> v 1)))))
 
(count overlaps)

;; Part 2

(defn points' [points]
  (let [[[x1 y1] [x2 y2]] (map (comp
                                (partial map #(Integer. %))
                                #(str/split % #","))
                               points)]
    (cond
      (= x1 x2)
      (for [y (apply range (-> (list y1 y2)
                               sort
                               vec
                               (update 1 inc)))]
        [x1 y])
      (= y1 y2)
      (for [x (apply range (-> (list x1 x2)
                               sort
                               vec
                               (update 1 inc)))]
        [x y1])
      (= (Math/abs (- x1 x2))
         (Math/abs (- y1 y2)))
      (for [[x y] (zipmap (cond->
                           (apply range (-> (list x1 x2)
                                            sort
                                            vec
                                            (update 1 inc)))
                            (> x1 x2)
                            reverse)
                          (cond->
                           (apply range (-> (list y1 y2)
                                            sort
                                            vec
                                            (update 1 inc)))
                            (> y1 y2)
                            reverse))]
        [x y])
      :else '())))

(def overlaps' (->> pairs
                    (mapcat points')
                    frequencies
                    (filter (fn [[_ v]] (> v 1)))))

(count overlaps')
