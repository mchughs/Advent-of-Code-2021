(ns day18.solution
  (:require [clojure.string :as str]
            [clojure.zip :as z]            
            [clojure.math.combinatorics :as combo]))

;; Helpers

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

;; Part 1

(def data
  (->> (slurp "src/day18/input.txt")
       str/split-lines
       (map (comp
             read-string
             str/trim))))

(defn addition [m n]
  [m n])

;; --- split action

(defn splitable? [loc]
  (let [node (z/node loc)]
    (and (number? node)
         (< 9 node))))

(defn split [n]
  [(int (Math/floor (/ n 2)))
   (int (Math/ceil (/ n 2)))])

(defn split-phase [locs]
  (when-let [split-loc (find-first splitable? locs)]
    (z/edit split-loc split)))

;; --- calc depth 

(defn loc-children [loc]
  (when-let [loc-child (z/down loc)]
    (->> loc-child
         (iterate z/right)
         (take-while some?))))

(defn loc-layers [loc]
  (->> [loc]
       (iterate (fn [locs]
                  (mapcat loc-children locs)))
       (take-while seq)))

(defn get-depth [tree target-loc]
  (let [layers (zipmap (loc-layers (z/vector-zip tree))
                       (range))]
    (->> (for [[layer depth] layers]
           (when (find-first #(= target-loc %) layer)
             depth))
         (find-first number?))))

;; --- explode action

(defn explodable? [tree loc]
  (when-not (z/end? loc)
    (let [node (z/node loc)]
      (and (< 3 (get-depth tree loc))
           (vector? node)
           (= 2 (count node))
           (number? (first node))
           (number? (last node))))))

(def opposites
  {z/left z/right
   z/right z/left})

(defn side-find [dir-fn loc]
  (when-let [found (->> loc
                        (iterate z/up)
                        (take-while (comp not nil?))
                        (map dir-fn)
                        (find-first (comp not nil?)))]
    (->> found
         (iterate (fn [loc]
                    (let [down (z/down loc)]
                      (if-let [side ((opposites dir-fn) down)]
                        side
                        down))))
         (find-first #(and (not (nil? %))
                           (number? (z/node %)))))))

(defn explode [loc]
  (let [[l r] (z/node loc)
        left (side-find z/left loc)
        right (side-find z/right loc)]
    (cond-> loc
      true (z/replace 0)
      (and left (not right))
      ((comp
        #(z/edit % + l)
        (partial side-find z/left)))
      (and right (not left))
      ((comp
        #(z/edit % + r)
        (partial side-find z/right)))
      (and left right)
      ((comp
        #(z/edit % + r)
        (partial side-find z/right)
        (partial side-find z/right)
        #(z/edit % + l)
        (partial side-find z/left))))))

(defn explode-phase [z locs]
  (when-let [explode-loc (find-first (partial explodable? z) locs)]
    (explode explode-loc)))

;; ---

(defn action [z]
  (let [locs (->> z
                  z/vector-zip
                  (iterate z/next)
                  (take-while (comp not z/end?)))]
    (z/root
     (if-let [exploded (explode-phase z locs)]
       exploded
       (if-let [splitted (split-phase locs)]
         splitted
         (first locs))))))

(defn reduce-snail-number [snail-number]
  (->> snail-number
       (iterate action)
       (partition 2 1)
       (take-while+ (fn [[x fx]]
                      (not= fx x)))
       last
       last))

(defn snail-arithmetic [snail-numbers]
  (reduce
   (fn [acc snail-number]
     (reduce-snail-number (addition acc snail-number)))
   snail-numbers))

(defn magnitude [[a b]]
  (+ (* 3 (if (number? a)
            a
            (magnitude a)))
     (* 2 (if (number? b)
            b
            (magnitude b)))))

(-> data
    snail-arithmetic
    magnitude
    time)

;; Part 2

(defn find-max [numbers]
  (let [pairs (combo/permuted-combinations numbers 2)]
    (->> pairs
         (map (comp magnitude snail-arithmetic))
         (apply max))))

(time
 (find-max data))
;; time ~302297ms Oof
;; could maybe use clojure.core.reducers to do some parallel reducing and speed things up
