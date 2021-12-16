(ns day16.solution
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as c]))

;; Part 1

(def hex->bin
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(def super-packet
  (->> (slurp "src/day16/input.txt")
       str/trim
       (map hex->bin)
       str/join))

(defn bin->dec [bin]
  (Integer/parseInt bin 2))

(defn separate [n coll]
  ((juxt take drop) n coll))

(defn parse-literal [s]
  (->> s
       (partition 5)
       (map (comp (partial apply str) rest))
       str/join
       bin->dec))

(declare split-into-packets parse-packet)

(defn length-of-subpackets [s]
  (let [[l & [remaining]] (separate 15 s)
        length (bin->dec (apply str l))
        subpackets (take length remaining)]
    (->> subpackets
         split-into-packets
         (map parse-packet))))

(defn number-of-subpackets [s]
  (let [[n & [remaining]] (separate 11 s)
        number (bin->dec (apply str n))
        candidates (loop [candidates '()
                          packets-str (apply str remaining)]
                     (if (= \1 (last packets-str))
                       (conj candidates packets-str)
                       (recur
                        (conj candidates packets-str)
                        (->> packets-str
                             drop-last
                             (apply str)))))]
    (prn "number" number candidates)
#_    
    (->> candidates
         (map split-into-packets)
         (filter #(= number (count %)))
         first
         (map parse-packet))))

(defn parse-operator [s]
  (let [[length-type-id & remaining] s]
    (if (= \0 length-type-id)
      (length-of-subpackets remaining)
      (number-of-subpackets remaining))))

#_(parse-literal "101111111000101000")

(defn parse-packet [packet]
  (let [[v1 v2 v3 t1 t2 t3 & remaining] packet
        version (bin->dec (str v1 v2 v3))
        type-id (bin->dec (str t1 t2 t3))]    
    {:version version
     :type-id type-id
     :remaining (if (= 4 type-id)
                  (parse-literal remaining)
                  (parse-operator remaining))}))

#_#_#_#_#_(parse-packet super-packet)

        (parse-packet "00111000000000000110111101000101001010010001001000000000")

      (parse-packet "11010001010")

    (parse-packet "0101001000100100")

  (parse-packet "11101110000000001101010000001100100000100011000001100000")


(def packet-sizes
  (iterate inc 10)
  #_
  (map #(+ 6 (* 5 %))
       (range)))

#_
(def packet-sizes
  (concat
   ;; literal packets
   (map #(+ 6 (* 5 (inc %)))
        (range))
   ;; operation length
   (map #(+ 22 (* 5 (inc %)))
        (range))
   ;; operation number
   (map #(+ 18 (* 5 %))
        (range)))
  #_
  (->> (for [fives (map #(* 5 (inc %)) (range))
             headers [6 18 22]]
         (+ fives headers))
       (take 100) ;; Not good... Not sure how to returns the finite number under a certain size
       (filter #(or (zero? (mod % 5))
                    (zero? (mod % 4))))))

(defn subset-sum [n s]
  (->> (c/subsets s)
       (filter #(pos? (count %))) ; ignore empty set since (+) == 0
       (filter #(= n (apply + %)))))
  
(defn split-into-packets [packets]
  (let [length (count packets)
        candidates (filter #(> length %) packet-sizes)
        min-candidate (apply min candidates)
        max-repeats (inc (quot length min-candidate))
        #_#_splits (->> candidates
                    (mapcat (partial repeat max-repeats))
                    (subset-sum length)
                    first)]
    (prn candidates)
    #_
    (loop [packets packets
           splits splits
           acc '()]
      (if (empty? splits)
        acc
        (let [[split-size & remaining-splits] splits
              [packet remaining] (separate split-size packets)]
          (recur remaining
                 remaining-splits
                 (conj acc (apply str packet))))))))

#_
(split-into-packets "110100010100101001000100100")
#_  (split-into-packets "010100000011001000001000110000011")

#_#_(map parse-packet
         (split-into-packets "110100010100101001000100100"))
  (map parse-packet
       (split-into-packets "010100000011001000001000110000011"))

#_#_
"100010100000000001001010100000000001101010000000000000101111010001111000"
"VVVTTTILLLLLLLLLLL"

#_
(def x
  {:version 4
   :type-id 2
   :remaining
   (list {:version 1
          :type-id 2
          :remaining
          (list {:version 5
                 :type-id 2
                 :remaining
                 (list {:version 6
                        :type-id 4
                        :remaining 15})})})})

(defn peel [{:keys [version remaining]}]
  [version
   (if (seq? remaining)
     (map peel remaining)
     0)])

(defn sum-versions [hex-packet]
  (->> hex-packet
       (map hex->bin)
       str/join
       parse-packet
       #_#_#_
       peel
       flatten
       (apply +)))

#_
(sum-versions "38006F45291200")
#_
(sum-versions "EE00D40C823060")

(sum-versions "8A004A801A8002F478")
#_(def bin "100010100000000001 001010100000000001101010000000000000101111010001111000")
#_
(sum-versions "620080001611562C8802118E34")
