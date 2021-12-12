(ns day12.solution 
  (:require [clojure.string :as str]))

(defn add-transition [m start end]
  (if (get m start)
    (update m start conj end)
    (assoc m start #{end})))

(def graph
  (->> (slurp "src/day12/input.txt")
       str/split-lines
       (map #(str/split % #"-"))
       (reduce (fn [m [a b]]
                 (if (or (= a "start")
                         (= b "end"))
                   (add-transition m a b)
                   (-> m
                       (add-transition a b)
                       (add-transition b a))))
               {})))

;; Part 1

(defn lower-case? [s]
  (= s (str/lower-case s)))

(defn repeated-lower-case? [coll]
  (some (fn [[k v]]
          (and (lower-case? k)
               (< 1 v)))
        (frequencies coll)))

(defn step [{:keys [paths]}]
  (let [possible-paths
        (for [path paths
              next (get graph (last path))]
          (conj path next))]
    {:paths (->> possible-paths
                 (remove #(= "end" (last %)))
                 (remove repeated-lower-case?))
     :completed (concat '()
                        (filter #(= "end" (last %))
                                possible-paths))}))

(->> {:paths '(["start"])
      :completed '()}
     (iterate step)
     (take-while #(or ((comp seq :completed) %)
                      ((comp seq :paths) %)))
     (reduce #(concat %1 (:completed %2)) '())
     count
     time)

;; Part 2

(defn allowed-repeated-lower-case? [coll]
  (let [freq (->> coll
                  (filter lower-case?)
                  frequencies)]
    (and
     (->> freq
          (filter (fn [[_ v]]
                    (= 2 v)))
          count
          (contains? #{0 1}))
     (->> freq
          (some (fn [[_ v]]
                  (< 2 v)))
          not)
     (-> freq
         (get "start")
         (= 1)))))

(defn step' [{:keys [paths]}]
  (let [possible-paths
        (for [path paths
              next (get graph (last path))]
          (conj path next))]
    {:paths (->> possible-paths
                 (remove #(= "end" (last %)))
                 (filter allowed-repeated-lower-case?))
     :completed (concat '()
                        (filter #(= "end" (last %))
                                possible-paths))}))


(->> {:paths '(["start"])
      :completed '()}
     (iterate step')
     (take-while #(or ((comp seq :completed) %)
                      ((comp seq :paths) %)))
     (reduce #(concat %1 (:completed %2)) '())
     count
     time)
