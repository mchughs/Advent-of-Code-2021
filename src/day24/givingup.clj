(ns day24.givingup
  "Take from https://github.com/tabidots/aoc2021/blob/main/src/aoc2021/day24.clj"
  (:require [clojure.java.io :as io]))

(def alu-steps
  "Extracts the three important values from each ALU instruction block."
  #_(with-open [rdr (io/reader (io/resource "../resources/day24.txt"))])
  (->> (slurp "src/day24/input.txt")
       clojure.string/split-lines
       (vec)
       (partition 18)       ;; Each block is 18 lines long
       (apply map vector)   ;; Transpose -> Each row is now the nth line of all blocks
       (remove #(= (count (set %)) 1)) ;; Remove the lines that are the same for each block
       (apply map vector)   ;; Transpose -> Each row is now the important lines of each block
       (map (fn [block]     ;; Convert the numbers to integers.
              (map #(read-string (re-find #"-?\d+" %))
                   block)))))

(comment
  "The operations do not need to be extracted from the puzzle input because
  the math has been done by hand.")

(defn alu-func
  "Given a z-value, an ALU program block represented by its three unique values
  a, b, and c, and an input value w, returns the resulting value z'."
  [z [a b c] w]
  (let [x  (if (= w (+ (mod z 26) b)) 0 1)  ; x is 0 if w = z mod 26 + b else 1
        y1 (inc (* 25 x))                   ; Z // A * (25x + 1) + x(w + c)
        y2 (* x (+ w c))]
    (+ y2 (quot (* z y1) a))))

(defn inverse-alu-func
  "Given a resulting value z' and an ALU program block, returns a map
  associating each seed-state z to the max/min (`mode`) w value that produces
  that result."
  [z' [a b c] mode]
  (apply merge-with mode
         (for [w (range 1 10)
          ;; match/not-match refers to the condition w = z mod 26 + b
          ;; the additional element % is to reverse the quotient function
               :let [match     (map #(+ % (* a z')) (range a))
                     not-match (map #(+ % (quot (* a (- z' (+ w c)))
                                                26))
                                    (range a))]
               z (set (filter pos? (concat match not-match)))
          ;; Prune the results by checking them forwards
               :when (= z' (alu-func z [a b c] w))]
           {z w})))

(defn table
  "Creates a table with the information necessary to find the max/min (`mode`)
  MONAD number possible step-by-step. If you are at digit D and the current
  z value is Z, (get-in table [D Z]) gives you the next w value you should input."
  [mode]
  (loop [digit 13, z's [0], result {}]
    (if (neg? digit) result
        (let [solutions (apply merge-with mode
                               (for [z' z's]
                                 (inverse-alu-func z' (nth alu-steps digit) mode)))]
          (recur (dec digit) (keys solutions) (conj result {digit solutions}))))))

(defn solve
  "Finds the max/min (`mode`) possible 14-digit MONAD number."
  [mode]
  (let [the-table (table mode)]
    (apply mode
           (remove nil?
                   (for [seed (range 1 10)]
                     (loop [[step & steps] alu-steps
                            result         [seed]
                            z              0]
                       (let [z' (alu-func z step (peek result))]
                         (if steps
                           (when-let [w (get-in the-table [(count result) z'])]
                             (recur steps (conj result w) z'))
                           (read-string (apply str result))))))))))

(defn part-1 [] (solve max))
(defn part-2 [] (solve min))
