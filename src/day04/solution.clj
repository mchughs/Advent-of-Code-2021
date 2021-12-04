(ns day04.solution
  (:require
   [clojure.string :as str]))

(defn transpose [m]
  (apply mapv vector m))

(def data
  (-> (slurp "src/day04/input.txt")
      (str/split #"\n\n")))

;; Part 1

(def bingo-seq
  (as-> data $
    (first $)
    (str/split $ #",")
    (map #(Integer. %) $)))

(def boards
  (->> data
       rest
       (map str/split-lines)
       (map (partial map #(-> %
                              str/trim
                              (str/split #"\s+"))))
       (mapcat (partial (juxt transpose identity)))
       (partition 2)
       (map (fn [[cols rows]]
              (concat cols rows)))
       (map (partial reduce (fn [acc straight]
                              (->> straight
                                   (map #(Integer. %))
                                   set
                                   (conj acc)))
                     '()))))

(defn winner? [marked board]
  (when (some (partial clojure.set/superset? marked) board)
    board))

(def winning-board
  (loop [[val & rest] (rest bingo-seq)
         marked #{(first bingo-seq)}
         last-called nil]
    (if-let [winner (or (some (partial winner? marked) boards)
                        (nil? val))]
      {:winner winner
       :marked marked
       :last-called last-called}
      (recur rest
             (conj marked val)
             val))))

(defn calculate-score [{:keys [winner marked last-called]}]
  (* (apply + (clojure.set/difference (reduce #(clojure.set/union %1 %2)
                                              #{}
                                              winner)
                                      marked))
     last-called))

(calculate-score winning-board)

;; Part 2

(def last-winning-board
  (loop [[val & rest] (rest bingo-seq)
         marked #{(first bingo-seq)}
         last-called nil
         boards boards]
    (if-let [last-winner (or (= 1 (count boards))
                             (nil? val))]
      {:winner (first boards)
       :marked marked
       :last-called last-called}
      (recur rest
             (conj marked val)
             val
             (remove (partial winner? marked) boards)))))


(calculate-score last-winning-board)
