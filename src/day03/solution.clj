(ns day03.solution
  (:require [clojure.java.io :as io]))

(defn transpose [m]
  (apply mapv vector m))

;; Part 1

(def popularity
  (time
   (with-open [rdr (io/reader "src/day03/input.txt")]
     (->> rdr
          line-seq
          transpose
          (map frequencies)
          (map #(let [ones (get % \1)
                      zeroes (get % \0)]
                  (if (> ones zeroes)
                    1
                    0)))))))

(def gamma
  (Integer/parseInt
   (apply str popularity)
   2))

(def epsilon
  (bit-clear
   (bit-xor gamma
            (Integer/parseInt "111111111111" 2))
   12))

(* gamma epsilon)

;; Part 2

(def diagnostics
  (time
   (with-open [rdr (io/reader "src/day03/input.txt")]
     (->> rdr
          line-seq
          doall))))

(defn run-criteria [bit-criteria]
  (first
   (loop [i 0
          candidates diagnostics]
     (if (= 1 (count candidates))
       candidates
       (let [freq (frequencies (map #(get % i) candidates))
             ones (get freq \1)
             zeroes (get freq \0)
             bit (bit-criteria ones zeroes)]
         (recur (inc i)
                (filter #(= bit (get % i)) candidates)))))))

(def oxygen
  (run-criteria (fn [ones zeroes]
                  (if (>= ones zeroes)
                    \1
                    \0))))

(def co2
  (run-criteria (fn [ones zeroes]
                  (if (>= ones zeroes)
                    \0
                    \1))))

(* (Integer/parseInt oxygen 2)
   (Integer/parseInt co2 2))
