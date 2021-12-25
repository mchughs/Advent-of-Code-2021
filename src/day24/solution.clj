(ns day24.solution
  (:require [clojure.string :as string]))

(def ^:dynamic registry
  (atom {:w 0
         :x 0
         :y 0
         :z 0}))

(def program
  (->> (slurp "src/day24/input.txt")
       string/split-lines
       (map (comp read-string
                  (fn [s] (string/replace s
                                          #"[wxyz]"
                                          #(str ":" %1)))
                  (partial format "(%s)")))))

(partition 18 program)

(defn inp [a]
  (fn [n]
    (swap! registry assoc a n)))

(defn arithmetic [op a b]
  (swap! registry (fn [reg]
                    (let [val (if (keyword? b)
                                (get reg b)
                                b)]
                      (update reg a op val)))))

(defn add [a b]
  (arithmetic + a b))

(defn mul [a b]
  (arithmetic * a b))

(defn div [a b]
  (when-not (and (number? b) (zero? b))
    (arithmetic quot a b)))

(defn mod [a b]
  (when-not (or (and (number? a) (neg? a))
                (and (number? b) (<= b 0)))
    (arithmetic clojure.core/mod a b)))

(defn eql [a b]
  (swap! registry (fn [reg]
                    (let [val (if (keyword? b)
                                (get reg b)
                                b)]
                      (update reg a #(if (= % val)
                                       1
                                       0))))))

(defn split-digits [n]
  (map #(Integer. (str %)) (str n)))

(defn run-program [input program]
  (with-bindings {#'registry
                  (atom {:w 0
                         :x 0
                         :y 0
                         :z 0})}
    (loop [[digit & remaining-digits :as digits]
           (split-digits input)
           [instruction & remaining :as my-program]
           program]
      (cond
        (nil? digit)
        (do
          (doseq [instruction my-program]
            (eval instruction))
          (:z @registry))

        (fn? (eval instruction))
        (do ((eval instruction) digit)
            (recur remaining-digits remaining))

        :else
        (do (eval instruction)
            (recur digits remaining))))))

(map run-program (partition 18 program))

#_
(->> 99999999999999
     (iterate dec)
     (filter
      (fn [n]
        (prn n)
        (or (and (not-any? zero? (split-digits n))
                 (zero? (run-program n)))
            (= n 99999999999990))))
     first
     time)
