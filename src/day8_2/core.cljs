
(ns day8-2.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day8.txt")
(def test-file  "inputs/day8-test.txt")


(defn is [expected got] (if (= expected got) true (println "Error: expected:" expected ", got:" got)))


(defn square-distance [vec1 vec2]
  (long
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


(defn merged-boxes-lookup [boxes-lookup cluster-from box-idx-to]
  (let [cluster-set (set cluster-from)
        lean-lookup (filter #(not (contains? cluster-set (:box %))) boxes-lookup)
        extra-items (map #(hash-map :box % :idx box-idx-to) cluster-from)]
    (concat lean-lookup extra-items)))

(defn box-to-str [box]
  (str "{:x " (:x box)
       " :y " (:y box)
       " :z " (:z box)
       "}"))

(defn pp-clusters [clusters]
  (let [re-mixed-clusters (for [cluster clusters]
                            (map (fn [box] (box-to-str box)) cluster))]
    (println "clusters" re-mixed-clusters)))


(defn pp-boxes [box1 box2]
  (println "box1" (box-to-str box1) "box2" (box-to-str box2)))


(defn all-connected?
  "All items on the lookup have the same idx (and it is not idx-unset)"
  [boxes-lookup]
  (let [idx (:idx (first boxes-lookup))]
    (if (= idx idx-unset) false
        (every? #(= (:idx %) idx) boxes-lookup))))

(def not-all-connected? (complement all-connected?))


(defn cluster-boxes
  ([boxes distances] (cluster-boxes boxes distances false))
  ([boxes distances verbose?]
   (let [boxes-lookup (for [box boxes] {:box box :idx idx-unset})]
     (reduce (fn [{:keys [clusters boxes-lookup]} connection]
               (let [{:keys [box1 box2]} connection
                     box1-idx (box-cluster-idx box1 boxes-lookup)
                     box2-idx (box-cluster-idx box2 boxes-lookup)]
                 (when verbose?
                   (println)
                   (pp-clusters clusters)
                   (pp-boxes box1 box2)
                   (println "box1-idx" box1-idx "box2-idx" box2-idx)
                   ;; (println "boxes-lookup" boxes-lookup)
                   )
                 (cond
                   (= box1-idx box2-idx idx-unset)
                   ;; Creates a new cluster for them two
                   {:clusters (conj clusters [box1 box2])
                    :boxes-lookup (->>
                                   boxes-lookup
                                   (filter #(not (or (= (:box %) box1) (= (:box %) box2))))
                                   (concat [{:box box1 :idx (count clusters)}
                                            {:box box2 :idx (count clusters)}]))}

                   (= box1-idx box2-idx)
                   ;; Ignores, it means they are in the same cluster already
                   {:clusters clusters :boxes-lookup boxes-lookup}

                   (some #(= idx-unset %) [box1-idx box2-idx])
                   ;; Add one to the cluster of the other
                   (let [box-from (if (= box1-idx idx-unset) box1 box2)
                         box-idx-to (max box1-idx box2-idx)
                         new-boxes-lookup (->>
                                           boxes-lookup
                                           (filter #(not (or (= (:box %) box1) (= (:box %) box2))))
                                           (concat [{:box box1 :idx box-idx-to}
                                                    {:box box2 :idx box-idx-to}]))]
                     (if (not-all-connected? new-boxes-lookup)
                       {:clusters (update
                                   clusters
                                   box-idx-to
                                   conj box-from)
                        :boxes-lookup new-boxes-lookup}
                       (reduced (* (:x box1) (:x box2)))))

                   :else
                   ;; {:clusters clusters :boxes-lookup boxes-lookup}
                   ;; Merge the clusters, arbritrarly merge it to the box1 cluster
                   (let [box-idx-to box1-idx
                         box-idx-from box2-idx
                         cluster-from (get clusters box-idx-from)
                         new-boxes-lookup (merged-boxes-lookup boxes-lookup cluster-from box-idx-to)]
                     (if not-all-connected?
                       {:clusters (->
                                   clusters
                                   ;; Empty the old list, keep it there, so the idx don't shift
                                   (assoc box-idx-from [])
                                   ;; Update the "to" list
                                   (update
                                    box-idx-to
                                    concat cluster-from))
                        :boxes-lookup new-boxes-lookup}
                       (reduced (* (:x box1) (:x box2))))))))
             {:clusters [] :boxes-lookup boxes-lookup}
             distances))))


(defn result-from-clusters
  "Result the product of the size of the three largest clusters"
  [clusters]
  (->>
   clusters
   (sort #(> (count %1) (count %2)))
   (take 3)
   (map (fn [x] (max 1 (count x))))
   (reduce *)))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
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
                               ;; Random criteria to remove one of each dist pairs
                               (> (hash (:box1 %)) (hash (:box2 %))))))
                    (sort-by :dist)
                    (map #(select-keys % [:box1 :box2])))
         clusters (cluster-boxes boxes distances verbose?)]
     (println "Data prepared, let's start")
     clusters)))


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
  (is 25272 (crack-the-code (input test-file) false)))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 8:" (crack-the-code (input input-file)))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
