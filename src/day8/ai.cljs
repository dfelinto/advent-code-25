;; My thoughts on chatGPT take
;;
;; * Nice use of destruction on square-distance to get the arguments accessible right away
;; * `zipmap` indeed sounds perfect for mapping values to a {:x :y :z}
;; * Nice use of (some #(when) on box-cluster-idx
;; * ahhh didn't know about remove. It seems like a complement filter
;;   it does makes things more readable, looks very idiomatic
;; * Nice way to access a list by simply using (list idx) instead of (get list idx)
;; * result-from-clusters looks way more idiomatic, I like it
;; * interesting mapv and all, less clean, but probably better

(ns day8.ai
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(def input-file "inputs/day8.txt")
(def test-file  "inputs/day8-test.txt")

(def idx-unset -1)

(defn square-distance [{x1 :x y1 :y z1 :z} {x2 :x y2 :y z2 :z}]
  (long (+ (Math/pow (- x1 x2) 2)
           (Math/pow (- y1 y2) 2)
           (Math/pow (- z1 z2) 2))))

(defn box
  "Return {:x ... :y ... :z ...} from a sequence of 3 numbers"
  [xyz]
  (->> xyz
       (map parse-long)
       (zipmap [:x :y :z])))

(defn distances-get [boxes]
  (for [b1 boxes, b2 boxes]
    {:box1 b1 :box2 b2 :dist (square-distance b1 b2)}))


(defn distances-get-improved
  "AI take on it, so it has no duplis, and it doesn't include the box itself"
  [boxes]
  (for [i (range (count boxes))
        j (range (inc i) (count boxes))]  ;; j > i ensures no duplicates
    (let [box1 (nth boxes i)
          box2 (nth boxes j)]
      {:box1 box1
       :box2 box2
       :dist (square-distance box1 box2)})))

(defn box-cluster-idx [box lookup]
  (:idx (some #(when (= box (:box %)) %) lookup)))

(defn merged-boxes-lookup [boxes-lookup cluster-from box-idx-to]
  (let [cluster-set (set cluster-from)
        lean-lookup (remove #(contains? cluster-set (:box %)) boxes-lookup)
        extra-items (map #(hash-map :box % :idx box-idx-to) cluster-from)]
    (concat lean-lookup extra-items)))

(defn cluster-boxes
  ([boxes distances] (cluster-boxes boxes distances false))
  ([boxes distances verbose?]
   (let [boxes-lookup (mapv #(hash-map :box % :idx idx-unset) boxes)]
     (:clusters
      (reduce (fn [{:keys [clusters boxes-lookup]} {:keys [box1 box2]}]
                (let [idx1 (box-cluster-idx box1 boxes-lookup)
                      idx2 (box-cluster-idx box2 boxes-lookup)]
                  (cond
                    ;; both unclustered: create new cluster
                    (and (= idx1 idx-unset) (= idx2 idx-unset))
                    {:clusters (conj clusters [box1 box2])
                     :boxes-lookup (->> boxes-lookup
                                        (remove #(or (= (:box %) box1)
                                                     (= (:box %) box2)))
                                        (concat [{:box box1 :idx (count clusters)}
                                                 {:box box2 :idx (count clusters)}]))}

                    ;; both in same cluster: no-op
                    (= idx1 idx2)
                    {:clusters clusters :boxes-lookup boxes-lookup}

                    ;; one unclustered: add to the other's cluster
                    (some #(= idx-unset %) [idx1 idx2])
                    (let [box-from (if (= idx1 idx-unset) box1 box2)
                          box-idx-to (max idx1 idx2)]
                      {:clusters (update clusters box-idx-to conj box-from)
                       :boxes-lookup (merged-boxes-lookup boxes-lookup [box-from] box-idx-to)})

                    ;; merge clusters
                    :else
                    (let [to idx1
                          from idx2
                          cluster-from (clusters from)]
                      {:clusters (-> clusters
                                     (assoc from [])
                                     (update to concat cluster-from))
                       :boxes-lookup (merged-boxes-lookup boxes-lookup cluster-from to)}))))
              {:clusters [] :boxes-lookup boxes-lookup}
              distances)))))

(defn result-from-clusters [clusters]
  (->> clusters
       (sort-by count >)
       (take 3)
       (map #(max 1 (count %)))
       (reduce *)))


(defn crack-the-code
  ([lines limit] (crack-the-code lines limit false))
  ([lines limit verbose?]
   (let [boxes (->> lines (map #(str/split % #",")) (map box))
         distances (->> (distances-get boxes)
                        (remove #(or (= (:box1 %) (:box2 %))
                                     (> (hash (:box1 %)) (hash (:box2 %)))))
                        (sort-by :dist)
                        (map #(select-keys % [:box1 :box2]))
                        (take limit))
         clusters (cluster-boxes boxes distances verbose?)]
     (result-from-clusters clusters))))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn input [filepath]
  (-> filepath fs/absolutize str slurp str/split-lines))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------
(defn test-sample-data []
  (let [result (crack-the-code (input test-file) 10 true)]
    (println "Test result:" result)
    (= result 40)))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------
(defn main []
  (time (println "Result for day 8:" (crack-the-code (input input-file) 1000))))

(main)
