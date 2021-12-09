(ns day09.solution 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (->> (slurp "src/day09/input.txt")
       str/split-lines
       (mapv #(mapv (fn [char] (Integer. (str char))) %))))

;; Part 1

(defn get-point-value [data [x y]]
  (-> data
      (get y)
      (get x)))

(defn get-neighboring-points [data x y max-x max-y]
  (->>
   (list [(inc x) y]
         [(dec x) y]
         [x (inc y)]
         [x (dec y)])
   (filter (fn [[x' y']]
             (and (< -1 x' max-x)
                  (< -1 y' max-y))))
   (map (partial get-point-value data))
   set))

(defn low-point? [{:keys [value neighbor-values]}]
  (every? #(< value %) neighbor-values))

(def low-points
  (let [max-x (count (first data))
        max-y (count data)
        neighboring-points (for [x (range max-x)
                                 y (range max-y)]
                             {:point [x y]
                              :value
                              (get-point-value data [x y])
                              :neighbor-values
                              (get-neighboring-points data x y max-x max-y)})]
    (filter low-point? neighboring-points)))

(reduce #(+ %1 (inc (:value %2))) 0 low-points)

;; Part 2

(defn find-neighbors [data max-x max-y points]
  (->>
   points
   (mapcat (fn [[x y]]
             (let [value (get-point-value data [x y])]
               (->>
                (list [(inc x) y]
                      [(dec x) y]
                      [x (inc y)]
                      [x (dec y)])
                (filter (fn [[x' y']]
                          (and (< -1 x' max-x)
                               (< -1 y' max-y))))
                (filter (fn [point]
                          (> 9
                             (get-point-value data point)
                             value)))))))
   set))

(defn- connected-points
  [data [x y]]
  (let [original-point [x y]
        max-x (count (first data))
        max-y (count data)]
    (->> #{original-point}
         (iterate (partial find-neighbors data max-x max-y))
         (take-while seq)
         (apply set/union))))

(->> low-points
     (map #(->> %
               :point
               (connected-points data)
               count))
     sort
     reverse
     (take 3)
     (apply *))

