(ns day14.solution 
  (:require [clojure.string :as str]))

;; Part 1

(def data
  (as-> (slurp "src/day14/input.txt") $
    (str/split $ #"\n\n")
    (map str/split-lines $)))

(def template
  (ffirst data))

(def rules
  (->> data
       last
       (map (comp
             (fn [[input insertion]]
               {input
                (apply str (interpose insertion input))})
             #(str/split % #" -> ")))
       (apply merge)))

(defn step [base]
  (let [pieces (->> base
                    (partition 2 1)
                    (map (comp
                          #(get rules % %)
                          (partial apply str))))]
    (->> (conj (reduce #(conj %1 (drop-last %2)) [] pieces)
               (->> pieces
                    last
                    last
                    list))
         (map (partial apply str))
         (apply str))))

(defn get-step [n]
  (->> template
       (iterate step)
       (take (inc n))
       last))

(->> 10
     get-step
     frequencies
     ((juxt (partial apply max-key last)
            (partial apply min-key last)))
     ((fn [[[_ most] [_ least]]]
        (- most least)))
     time)

;; Part 2

(def rules'
  (->> data
       last
       (map (comp
             (fn [[input insertion]]
               {input #{(str (first input) insertion)
                        (str insertion (last input))}})
             #(str/split % #" -> ")))
       (apply merge)))

(defn step-freqs [freq]
  (reduce-kv (fn [acc k v]
               (loop [[x & xs] (get rules' k 0)
                      acc acc]
                 (if (nil? x)
                   acc
                   (recur xs
                          (update acc x (fnil + 0) v)))))
             {}
             freq))

(defn pair-frequencies [n]
  (->> template
       (partition 2 1)
       (map (partial apply str))
       frequencies
       (iterate step-freqs)
       (take (inc n))
       last))

(defn alpha-frequencies [pair-frequencies]
  (reduce-kv
   (fn [acc k v]
     (let [[char1 char2] k]
       (-> acc
           (update char1 (fnil + 0) v)
           (update char2 (fnil + 0) v))))
   {}
   pair-frequencies))

(defn half [n]
  (if (zero? (mod n 2))
    (/ n 2)
    (/ (inc n) 2)))

(->> 40
     pair-frequencies
     alpha-frequencies
     ((juxt (partial apply max-key last)
            (partial apply min-key last)))
     ((fn [[[_ most] [_ least]]]
        (- (half most) (half least))))
     time)
