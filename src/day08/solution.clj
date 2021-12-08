(ns day08.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (->> (slurp "src/day08/input.txt")
       str/split-lines))

;; Part 1

(->> data     
     (map (comp
           #(filter (fn [n] (#{2 3 4 7} n)) %)
           #(map count %)
           #(str/split % #" ")
           last
           #(str/split % #"\| ")))
     flatten
     count)

;; Part 2

(defn categorize [connections-list]
  (->> (str/split connections-list #" ")
       (map #(set (map identity %)))))

(defn signal-matcher [signals]
  (let [one (first (get signals 2))
        four (first (get signals 4))
        seven (first (get signals 3))
        eight (first (get signals 7))
        three (first (filter #(= 2 (count (set/intersection % one)))
                             (get signals 5)))
        nine (first (filter #(= 1 (count (set/difference % three)))
                            (get signals 6)))
        two (first (filter #(let [e (first (set/difference eight nine))]
                              (contains? % e))
                           (get signals 5)))
        five (first (remove #(#{two three} %)
                           (get signals 5)))
        zero (first (filter #(and (not= % nine)
                                  (= 4 (count (set/difference % one))))
                            (get signals 6)))
        six (first (filter #(= 5 (count (set/difference % one)))
                            (get signals 6)))]
    {zero 0
     one 1
     two 2
     three 3
     four 4
     five 5
     six 6
     seven 7
     eight 8
     nine 9}))

(->> data
     (map (comp
           (fn [{:keys [signals outputs]}]
             (Integer. (apply str (map #(get signals %) outputs))))
           (fn [[signals outputs]]
             {:signals (->> signals
                            categorize
                            (reduce
                             #(update %1 (count %2) conj %2)
                             {})
                            signal-matcher)
              :outputs (categorize outputs)})
           #(str/split % #" \| ")))
     (apply +))
