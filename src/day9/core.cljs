
(ns day9.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day9.txt")
(def test-file  "inputs/day9-test.txt")


(defn is [expected got] (if (= expected got) true (println "Error: expected:" expected ", got:" got)))


(defn parse-corners
  "1,2 -> {:x 1 :y 2}"
  [line] (->>
          (str/split line #",")
          (map parse-long)
          (zipmap [:x :y])))

(defn area [from to]
  ;;   (println from to)
  (let [width (inc (abs (- (:x from) (:x to))))
        height (inc (abs (- (:y from) (:y to))))
        square-area (* width height)]
    ;; (println square-area)
    square-area))


(defn calculate-areas [corners]
  (for [from-idx (range (count corners))
        to-idx (range (inc from-idx) (count corners))]
    (let [from (corners from-idx)
          to (corners to-idx)]
      (area from to))))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (let [corners (->> lines
                      (map parse-corners)
                      vec)
         areas (calculate-areas corners)]
     (apply max areas))))


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
  (is 50 (crack-the-code (input test-file) true)))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 9:" (crack-the-code (input input-file)))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
