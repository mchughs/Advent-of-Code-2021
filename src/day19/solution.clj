(ns day19.solution
  "Felt close to a part 1 answer but couldn't quite close it out :("
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))



(def data
  (as-> (slurp "src/day19/input.txt") $
    (str/split $ #"\n\n")
    (map #(->> %
               str/split-lines
               rest
               (map (fn [s]
                      (let [[x y z] (str/split s #",")]
                        [(Integer. x)
                         (Integer. y)
                         (Integer. z)]))))
         $)))

(defn distance [[[x1 y1 z1]
                 [x2 y2 z2]]]
  (Math/sqrt
   (+ (Math/pow (- x2 x1) 2)
      (Math/pow (- y2 y1) 2)
      (Math/pow (- z2 z1) 2))))

(defn distance' [[x1 y1 z1]
                 [x2 y2 z2]]
  (Math/sqrt
   (+ (Math/pow (- x2 x1) 2)
      (Math/pow (- y2 y1) 2)
      (Math/pow (- z2 z1) 2))))

(defn all-distances [points]
  (->> (combo/combinations points 2)
       (map distance)
       set))

(defn roll [[x y z]]
  [x z (* -1 y)])

(defn turn [[x y z]]
  [(* -1 y) x z])

(defn calc-orientation [point [turns rolls rtr]]
  (->> point
       (iterate (comp roll turn roll))
       (take (inc rtr))
       last
       (iterate roll)
       (take (inc rolls))
       last
       (iterate turn)
       (take (inc turns))
       last))

(defn orientations
  "Explanation at https://stackoverflow.com/questions/16452383/how-to-get-all-24-rotations-of-a-3-dimensional-array"
  [point]
  (apply merge
         (for [turns [0 1 2 3]
               rolls [0 1 2]
               rtr [0 1]]
           {(calc-orientation point [turns rolls rtr])
            [turns rolls rtr]})))

(orientations [1 2 3])

(defn distances [origin points]
  (->> points
       (map (partial distance' origin))
       set))

(defn compare-sensors [sa sb]
  (reduce
   (fn [acc [pa pb]]
     (if (<= 12
             (count
              (set/intersection
               (distances pa sa)
               (distances pb sb))))
       (assoc acc pa pb)
       acc))
   {}
   (combo/cartesian-product sa sb)))

(def overlaps
  (as-> data $
    (zipmap (range) $)
    (combo/combinations $ 2)
    (map (fn [[[sid-a beacons-a]
               [sid-b beacons-b]]]
           {:beacons-mapping (compare-sensors beacons-a beacons-b)
            :sensors [sid-a sid-b]}) $)
    (remove (comp empty? first :beacons-mapping) $)
    (set $)))

(def reverse-overlaps
  (as-> data $
    (zipmap (range) $)
    (combo/combinations $ 2)
    (map (fn [[[sid-a beacons-a]
               [sid-b beacons-b]]]
           {:beacons-mapping (compare-sensors beacons-b beacons-a)
            :sensors [sid-b sid-a]}) $)
    (remove (comp empty? first :beacons-mapping) $)
    (set $)))

(defn pairwise-sum [p1 p2]
  (map + p1 p2))

(defn pairwise-diff [p1 p2]
  (map - p1 p2))

(defn pairwise-neg [p]
  (map (partial * -1) p))

(defn scanner-location [{:keys [beacons-mapping
                                sensors]}]
  (let [[origin-scanner unknown-scanner] sensors
        {:keys [points orientation]}
        (->> beacons-mapping
             (reduce
              (fn [{:keys [points orientation] :as p} [pa pb]]
                (if orientation
                  p
                  (let [orientation-map (orientations pb)
                        x (reduce-kv (fn [m k v]
                                       (assoc m k (pairwise-diff pa k)))
                                     {}
                                     orientation-map)
                        k (ffirst (filter (fn [[k v]] (contains? points v)) x))]
                    (if (= 1 (count points))                      
                      {:points points
                       :orientation (get orientation-map k)}
                      (let [remaining-candidates
                            (->> orientation-map
                                 keys
                                 (map (partial pairwise-diff pa))
                                 set)]
                        (if (empty? points)
                          {:points remaining-candidates}
                          {:points (set/intersection points remaining-candidates)}))))))
              {:points #{}}))]
    {:sensors sensors
     :relative-postition (first points)
     :orientation orientation}))

(def sensor-locations
  (->> overlaps
       (map scanner-location)
       (reduce
        (fn [m {:keys [sensors :relative-postition orientations]}]
          (let [[origin relative]]
            )
          (assoc m ))
        {0 [0 0 0]}))
  )

(clojure.pprint/pprint
 (map scanner-location overlaps))

(clojure.pprint/pprint
 (map scanner-location reverse-overlaps))

(comment
  '(0 0 0) ;; 0

  ; ---

  '(68 -1246 -43) ;; 1

  ; ---

  (pairwise-sum '(68 -1246 -43)
                (calc-orientation '(88 113 -1104)
                                  [2 2 0]))

  '(-20 -1133 1061) ;; 4

  ; ---

  (pairwise-sum '(-20 -1133 1061)
                (calc-orientation
                 (calc-orientation
                  (pairwise-neg
                   (calc-orientation
                    '(1125 -168 72)
                    [1 2 0]))
                  [1 0 1])
                 [2 2 0]))

  '(1105,-1205,1229) ;; 2

  ; ---

  (pairwise-sum '(68 -1246 -43)
                (calc-orientation '(160 -1134 -23)
                                  [2 2 0]))

  '(-92 -2380 -20) ;; 3
  )
