
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


(def test-cluster-data
  [[{:x 1 :y 2 :z 3}
    {:x 4 :y 5 :z 6}
    {:x 7 :y 9 :z 12}
    {:x 14 :y 20 :z 1}]
   [{:x 6 :y 7 :z 6}
    {:x 5 :y 6 :z 2}]
   [{:x 4 :y 8 :z 5}
    {:x 3 :y 6 :z 9}
    {:x 2 :y 5 :z 11}]
   [{:x 1 :y 0 :z 12}]])


(def idx-unset -1)

(defn box-cluster-idx [box lookup]
  (:idx (first (filter #(= box (:box %)) lookup))))


(defn cluster-boxes
  [boxes distances]
  (let [boxes-lookup (for [box boxes] {:box box :idx idx-unset})]
    (:clusters
     (reduce (fn [{:keys [clusters boxes-lookup]} connection]
               (let [{:keys [box1 box2]} connection
                     box1-idx (box-cluster-idx box1 boxes-lookup)
                     box2-idx (box-cluster-idx box2 boxes-lookup)]
                 (cond
                   (= box1-idx box2-idx idx-unset)
                   ;; Creates a new cluster for them two
                   {:clusters (conj clusters [box1 box2])
                    :boxes-lookup (->>
                                   boxes-lookup
                                   (filter #(not (or (= (:box %) box1) (= (:box %) box2))))
                                   (conj [{:box box1 :idx (count clusters)}
                                          {:box box2 :idx (count clusters)}]))}

                   (= box1-idx box2-idx)
                   ;; Ignores, it means they are in the same cluster already
                   {:clusters clusters :boxes-lookup boxes-lookup}

                   :else
                   ;; TODO handle the case where we want to merge clusters
                   {:clusters clusters :boxes-lookup boxes-lookup})))
             {:clusters [] :boxes-lookup boxes-lookup}
             distances))))


(defn result-from-clusters
  "Result the product of the size of the three largest clusters"
  [clusters]
  (->>
   clusters
   (sort #(> (count %1) (count %2)))
   (take 3)
   (map count)
   (reduce *)))


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
                    (sort-by :dist)
                    (map #(select-keys % [:box1 :box2])))
         clusters (cluster-boxes boxes distances)]
     (result-from-clusters clusters))))

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
