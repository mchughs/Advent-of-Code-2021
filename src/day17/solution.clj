(ns day17.solution
  (:require [clojure.string :as str]))

;; Part 1

(def target-area
  (->> (slurp "src/day17/input.txt")
       str/trim
       (re-matches #"target area: x=(\d+)..(\d+), y=-(\d+)..-(\d+)")
       rest
       (map #(Integer. %))       
       ((fn [[x1 x2 y1 y2]]
          (for [x (range x1
                         (inc x2))
                y (range (* -1 y1)
                         (inc (* -1 y2)))]            
            [x y])))
       set))

(defn step [{:keys [velocity _position] :as state}]
  (-> state
      (update :position (fn [[x y]]
                          (let [[vx vy] velocity]
                            [(+ x vx) (+ y vy)])))
      (update :velocity (fn [[vx vy]]
                            [(if (zero? vx)
                               vx
                               (dec vx))
                             (dec vy)]))))

(def init-position [0 0])

(def max-x
  (first (apply max-key first target-area)))

(def min-y
  (last (apply min-key last target-area)))

(defn beyond-target? [[x y]]
  (or (< max-x x)
      (> min-y y)))

(defn hit-target? [init-velocity]
  (->> {:velocity init-velocity
        :position init-position}
       (iterate step)
       (take-while #(-> %
                        :position
                        beyond-target?
                        not))
       ((juxt
         (comp (partial contains? target-area) :position last)
         (comp last :position (partial apply max-key (comp last :position)))))
       ((fn [[hit? max-y]]
          {:hit-target? hit?
           :maximum-y max-y}))))

(def upper-bound-vy ;; the vy only decreases by 1 each step so it has an upper initial bound or else the probe will translate x too far
  (first (apply min-key first target-area)))

(->> (for [vx (range (inc max-x))
           vy (range min-y upper-bound-vy)]
       (hit-target? [vx vy]))
     (filter :hit-target?)
     (apply max-key :maximum-y)
     :maximum-y
     time)

;; Part 2

(->> (for [vx (range (inc max-x))
           vy (range min-y upper-bound-vy)]
       (hit-target? [vx vy]))
     (filter :hit-target?)
     count)
