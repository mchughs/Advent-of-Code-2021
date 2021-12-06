(ns day06.solution
  (:require [clojure.string :as str]))

;; Part 1

(def fishes
  (mapv #(Integer. %)
        (-> (slurp "src/day06/input.txt")
            (str/trim)
            (str/split #","))))

(defn ageify [fish]
  (if (zero? fish)
    6
    (dec fish)))

(defn inc-day [fishes]
  (let [new-fish-count (count (filter zero? fishes))]
    (concat (map ageify fishes)
            (repeat new-fish-count 8))))


(defn get-day [n fishes]
  (time (nth (iterate inc-day fishes) n)))

(time
 (count (get-day 80 fishes)))

;; Part 2

(def descendants
  (memoize
   (fn [n fish]
     (loop [day 0
            fishes (list fish)]
       (if (= day n)
         (frequencies fishes)
         (recur (inc day)
                (inc-day fishes)))))))

(defn apply-step [freq step]
  (let [children-frequencies
        (for [[k v] freq
              :let [children (descendants step k)]]
          (reduce-kv #(assoc %1 %2 (* %3 v)) {} children))]
    (reduce (fn [acc m]
              (reduce-kv #(update %1 %2 + %3) acc m))
            (zipmap (range 9) (repeat 0))
            children-frequencies)))

(defn quick-count
  ;; NOTE chunk-size needs to divide n evenly
  [n fishes chunk-size]
  (loop [freq (frequencies fishes)
         day 0]
    (if (= day n)
      (reduce-kv #(+ %1 %3) 0 freq)
      (recur
       (apply-step freq chunk-size) 
       (+ day chunk-size)))))

(time
 (quick-count 256 fishes 16))
