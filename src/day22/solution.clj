(ns day22.solution 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; Part 1

(def instructions
  (->>
   (slurp "src/day22/input.txt")
   str/split-lines
   (map (comp
         (fn [[_ status & ints]]
           (let [[x-min x-max y-min y-max z-min z-max] (map #(Integer. %) ints)]
             {:status (keyword status)
              :x [x-min x-max]
              :y [y-min y-max]
              :z [z-min z-max]}))
         (partial re-matches
                  #"(on|off) x=(-*\d+)\.\.(-*\d+),y=(-*\d+)\.\.(-*\d+),z=(-*\d+)\.\.(-*\d+).*")))))

(defn run-instruction [on-cubes {:keys [status x y z] :as _instruction}]
  (let [[x-min x-max] x
        [y-min y-max] y
        [z-min z-max] z
        cuboid (set (for [x (range x-min (inc x-max))
                          y (range y-min (inc y-max))
                          z (range z-min (inc z-max))]
                      [x y z]))
        f (if (= status :on)
            set/union
            set/difference)]
    (f on-cubes cuboid)))


(->> instructions
     (filter (fn [instruction]
               (let [coords (flatten (vals (dissoc instruction :status)))]
                 (every? #(<= -50 % 50) coords))))
     (reduce run-instruction #{})
     count
     time)

;; Part 2

;; parsing the axes as :x :y :z instead of positionally wasn't a good choice.
