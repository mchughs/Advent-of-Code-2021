(ns day21.solution
  (:require [clojure.math.combinatorics :as combo]))

;; Part 1

(defn dice-roll [n]
  (as-> (range 1 101) $
    (cycle $)
    (partition 3 $)
    (nth $ n)
    (apply + $)))

(time
 (loop [turn 0
        player-positions {:p1 1 :p2 5}
        player-scores {:p1 0 :p2 0}]
   (if (or (<= 1000 (:p1 player-scores))
           (<= 1000 (:p2 player-scores)))
     (* (* 3 turn) ;; 3 dice rolls per turn
        (apply min (vals player-scores)))
     (let [player (if (zero? (mod turn 2)) :p1 :p2)
           roll (dice-roll turn)
           new-positions (update player-positions player #(inc (mod (+ roll (dec %)) 10)))]
       (recur (inc turn)
              new-positions
              (update player-scores player + (player new-positions)))))))

;; Part 2

(def dirac-dice
  (->> (combo/selections [1 2 3] 3)
       (map (comp list (partial apply +)))
       frequencies))

(def transitions
  (apply merge
         (for [position (range 1 11)
               [[roll] freq] dirac-dice]
           {{:position position
             :roll roll}
            {:points (inc (mod (+ roll (dec position)) 10))
             :freq freq}})))

(defn get-winner [roll-sequence]
  (let [roll-sequence (reverse roll-sequence)]
    (loop [[roll & remaining] roll-sequence
           turn 0
           player-positions {:p1 4 :p2 8}
           player-scores {:p1 0 :p2 0}]
      (cond
        (<= 21 (:p1 player-scores)) :p1
        (<= 21 (:p2 player-scores)) :p2
        (nil? roll) nil
        :else
        (let [player (if (zero? (mod turn 2)) :p1 :p2)
              new-positions (update player-positions player
                                    #(:points
                                      (get transitions
                                           {:position %
                                            :roll roll})))]
          (recur remaining
                 (inc turn)
                 new-positions
                 (update player-scores player + (player new-positions))))))))

(defn roll-die [history]
  (apply merge
   (for [[[roll] chance] dirac-dice
         [rolls chance'] history]
     {(conj rolls roll) (* chance chance')})))

(time
 (loop [rolls dirac-dice
        wins {:p1 (bigint 0) :p2 (bigint 0)}]
   (prn (count rolls))
   (if (zero? (count rolls))
     wins
     (let [{:keys [wins unwon-rolls]}
           (reduce (fn [{:keys [wins unwon-rolls]} [roll freq]]
                     (if-let [winner (get-winner roll)]
                       {:wins (update wins winner + freq)
                        :unwon-rolls unwon-rolls}
                       {:wins wins
                        :unwon-rolls (assoc unwon-rolls roll freq)}))
                   {:wins wins
                    :unwon-rolls {}}
                   (roll-die rolls))]
       (recur unwon-rolls
              wins)))))

;; 444356092776315
;; 341960390180808
