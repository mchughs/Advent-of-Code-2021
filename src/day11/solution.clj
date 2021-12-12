(ns day11.solution 
  (:require [clojure.string :as str]))

;; Part 1

(def data
  (->> (slurp "src/day11/input.txt")
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

(defn neighbors [data [x y]]
  (apply merge
         (for [dx [-1 0 1]
               dy [-1 0 1]
               :let [x' (+ x dx)
                     y' (+ y dy)]
               :when (and (not= 0 dx dy)
                          (< -1 x' (inc max-x))
                          (< -1 y' (inc max-y)))]
           {[x' y'] (get data [x' y'])})))

(defn brighten-phase [data points]
  (reduce (fn [acc point]
            (update acc point inc))
          data
          points))

(defn map-vals [m f]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn flash [points data]
  (reduce
   (fn [acc point]    
     (cond
       (nil? (get acc point))
       acc
       (< 9 (get acc point))
       (let [flashed-neighbors (map-vals (neighbors acc point)
                                         #(if (number? %)
                                            (inc %)
                                            %))]
         (-> acc
             (merge flashed-neighbors)
             (assoc point nil)
             (update :flashes inc)))
       :else
       acc))
   data
   points))

(defn flashy? [data]
  (->> (dissoc data :flashes)
       vals
       (remove nil?)
       (some (partial < 9))))

(defn flash-phase [data points]
  (if (flashy? data)
    (->> data
         (iterate (partial flash points))
         (take-while flashy?)
         last
         (flash points))
    data))

(defn cooldown-phase [data points]
  (reduce (fn [acc point]
            (if (nil? (get acc point))
              (assoc acc point 0)
              acc))
          data
          points))

(def all-points
  (for [x (range (inc max-x))
        y (range (inc max-y))]
    [x y]))

(defn pad-number [n]
  (cond
    (nil? n) n
    (< 9 n) (str " " n)
    :else (str "  " n)))

(defn print! [octopuses]
  (println)
  (doseq [y (range (inc max-y))]
    (println
     (for [x (range (inc max-x))]
       (pad-number
        (get octopuses [x y]))))))

(defn step [all-points data]
  (-> data
      (brighten-phase all-points)
      (flash-phase all-points)
      (cooldown-phase all-points)))

(defn get-step [data n]
  (->> (assoc data :flashes 0)
       (iterate (partial step all-points))
       (take (inc n))
       last))

(:flashes (get-step data 100))

;; Part 2

(defn get-syncro-step [data]
  (->> (assoc data :flashes 0)
       (#(map list
              (iterate (partial step all-points) %)
              (range)))
       (drop-while
        (fn [data]
          (let [values (-> data
                           first
                           (dissoc :flashes)
                           vals)]
            (->> values
                 (remove nil?)
                 (every? (partial = 0))
                 not))))
       first
       last))

(get-syncro-step data)
