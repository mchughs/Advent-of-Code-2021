(ns day23.solution)

;; Really no clue how to approach this

(def data
  (->> (slurp "src/day23/input.txt")
       (re-seq #"\w")))

(def spaces
  #{7 1 4 6 3 2 9 5 10 8 11 39 65 91 117 507 845 1183 1521})

(def hallway
  #{7 1 4 6 3 2 9 5 10 8 11})

(def doorways
  #{3 5 7 9})

(def rooms
  '(39 65 91 117 507 845 1183 1521))

#_(defn legal-transition? [a b]
    (and (contains? spaces a)
         (contains? spaces b)
         (or (= a (inc b))
             (= b (inc a))
             (= a (* 13 b))
             (= b (* 13 a)))))

;; a position should be the place and the energy it took to get there

(defn steps [state positions]
  (->> (for [[position reach] positions]
         (as-> [(inc position)
                (dec position)
                (* position 13)
                (quot position 13)] $
           (filter spaces $)
           (remove (partial get state) $)
           (zipmap $ (repeat (inc reach)))))
       (apply merge-with min)
       (merge-with min positions)))

(def init-state
  (merge
   (zipmap hallway (repeat nil))
   (zipmap rooms data)))

(steps init-state {1 0 3 0})

(defn move [state a]
  (if (get state a)
    (apply dissoc
           (->> {a 0}
                (iterate (partial steps state))
                (partition 2 1)
                (take-while (fn [[x fx]] (not= x fx)))
                last
                first)
           doorways)
    {}))

(def start 39)

(->> start
     (move init-state)
     
     
     )
