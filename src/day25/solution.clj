(ns day25.solution 
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "src/day25/input.txt")
       str/split-lines
       ))

(def max-x (count (first data)))
(def max-y (count data))
  
(def herd
  (->> data
       (map-indexed (fn [y row]
                      (map-indexed (fn [x char]
                                     {[x y] char})
                                   row)))
       flatten
       (apply merge)
       (reduce-kv
        (fn [m k v]
          (case v
            \>
            (update m :east conj k)
            \v
            (update m :south conj k)
            \.
            m
            #_(update m :open conj k)))
        {:east #{}
         :south #{}
         #_#_:open #{}})))


(defn move-east [{:keys [east south]}]
  (->> east
       (reduce
        (fn [m [x y]]
          (let [adjacent [(if (= (inc x) max-x) 0 (inc x)) y]]
            (-> m
                (update :east conj
                        (if (and (not (contains? east adjacent))
                                 (not (contains? south adjacent)))
                          adjacent
                          [x y])))))
        {:east #{}
         :south south})))

(defn move-south [{:keys [east south]}]
  (->> south
       (reduce
        (fn [m [x y]]
          (let [adjacent [x (if (= (inc y) max-y) 0 (inc y))]]
            (-> m
                (update :south conj
                        (if (and (not (contains? east adjacent))
                                 (not (contains? south adjacent)))
                          adjacent
                          [x y])))))
        {:east east
         :south #{}})))

(def step (comp move-south move-east))

(->> herd
     (iterate step)
     (partition 2 1)
     (take-while (fn [[x fx]] (not= x fx)))
     count
     inc
     time)

