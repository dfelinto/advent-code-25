
(ns day8.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day8.txt")
(def test-file  "inputs/day8-test.txt")


(defn is [expected got] (if (= expected got) true (println "Error: expected:" expected ", got:" got)))


(defn square-distance [vec1 vec2]
  (int
   (+ (Math/pow (- (:x vec1) (:x vec2)) 2)
      (Math/pow (- (:y vec1) (:y vec2)) 2)
      (Math/pow (- (:z vec1) (:z vec2)) 2))))


;; (def square-distance-get (memoize square-distance))


(defn box
  "Return {:x 1 :y 2 :z 3}"
  [xyz]
  (->>
   xyz
   (map parse-long)
   (mapcat list [:x :y :z])
   (apply hash-map)))


(defn distances-get [boxes]
  (for [box1 boxes box2 boxes]
    {:box1 box1 :box2 box2 :dist (square-distance box1 box2)}))


(defn crack-the-code
  ([lines] (crack-the-code false))
  ([lines verbose?]
   (let [boxes (->>
                lines
                (map #(str/split % #","))
                (map box))
         distances (->>
                    boxes
                    distances-get
                    (filter
                     #(not (or (= (:box1 %) (:box2 %))
                               (neg? (:dist %)))))
                    (sort-by :dist))]
     (take 5 distances);; TODO logic
     )))

;; Next: creates a par of boxes [a b] sorted by the distance between
;; all boxes. To do this creates a connection between all boxes
;; sort by x (< x is the first box)
;; sort all based on the square distance


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn input
  [filepath]
  (println "Reading sequence from file:" filepath)
  (->>
   filepath
   (fs/absolutize)
   str
   slurp
   (str/split-lines)))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(defn test-sample-data []
  (is 40 (crack-the-code (input test-file) true)))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 8:" (crack-the-code (input input-file) true))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
