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

#_
(defn split-digits [n]
  (map #(Integer. (str %)) (str n)))

(defn run-program [program digit init-z]
  (with-bindings {#'registry
                  (atom {:w digit
                         :x 0
                         :y 0
                         :z init-z})}
    (loop [[instruction & remaining] (rest program)]
      (if (nil? instruction)
        (:z @registry)
        (do (eval instruction)
            (recur remaining))))))

(def sub-programs
  (reverse (partition 18 program)))

#_
(for [sub-program sub-programs
      digit (range 1 10)]
  (run-program sub-program digit))

#_
(loop [[sub-program & remaining] sub-programs
       target 0
       path '()]
  (if (nil? sub-program)
    path
    (recur
     (conj
      path
      (->> (range -100 100)
           (map #(hash-map % (run-program sub-program 1 %)))
           (filter (comp (partial = target) first vals)))))))

(def x
  '(-20 6 -19 7 -18 8 -17 9 -16
    10 -15 11 -14 12 -13 13 -12 14))

(clojure.pprint/pprint
 (time
  (let [idx 1
        final-z 13]
    (remove empty?
            (for [digit (range 1 10)]
              (->> (range -40 40)
                   (map (fn [init-z]
                          (let [output (run-program (nth sub-programs idx)
                                                    digit
                                                    init-z)]
                            {:digit digit
                             :final-z output
                             :init-z init-z
                             :sub-program idx})))
                   (filter (comp (partial = final-z) :final-z))))))))
#_
(comment
  ;; = empty? - Example 1 = 

  user=> (empty? ())
  true
  user=> (empty? '(1))
  false
  ;; See also:
  seq
  empty
  not-empty
  )

(def possibilities
  [_ _ _ _ _ _ _ _ _ _ _ _ #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5 6 7 8 9}])

;; idx 0
(({:digit 1, :final-z 0, :init-z -20, :sub-program 0}
  {:digit 1, :final-z 0, :init-z 6, :sub-program 0})
 ({:digit 2, :final-z 0, :init-z -19, :sub-program 0}
  {:digit 2, :final-z 0, :init-z 7, :sub-program 0})
 ({:digit 3, :final-z 0, :init-z -18, :sub-program 0}
  {:digit 3, :final-z 0, :init-z 8, :sub-program 0})
 ({:digit 4, :final-z 0, :init-z -17, :sub-program 0}
  {:digit 4, :final-z 0, :init-z 9, :sub-program 0})
 ({:digit 5, :final-z 0, :init-z -16, :sub-program 0}
  {:digit 5, :final-z 0, :init-z 10, :sub-program 0})
 ({:digit 6, :final-z 0, :init-z -15, :sub-program 0}
  {:digit 6, :final-z 0, :init-z 11, :sub-program 0})
 ({:digit 7, :final-z 0, :init-z -14, :sub-program 0}
  {:digit 7, :final-z 0, :init-z 12, :sub-program 0})
 ({:digit 8, :final-z 0, :init-z -13, :sub-program 0}
  {:digit 8, :final-z 0, :init-z 13, :sub-program 0})
 ({:digit 9, :final-z 0, :init-z -12, :sub-program 0}
  {:digit 9, :final-z 0, :init-z 14, :sub-program 0}))


#_


(map (partial run-program (first sub-programs))
     (range 1 10))

#_(->> 99999999999999
       (iterate dec)
       (filter
        (fn [n]
          (prn n)
          (or (and (not-any? zero? (split-digits n))
                   (zero? (run-program n)))
              (= n 99999999999990))))
       first
       time)
