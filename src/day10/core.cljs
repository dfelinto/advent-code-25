
(ns day10.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day10.txt")
(def test-file  "inputs/day10-test.txt")


(defn is [expected got] (if (= expected got) true (println "Error, got:" got "\nExpected  :" expected)))


(defn parse-buttons
  "(3) (1,3) (2) (2,3) (0,2) (0,1) -> [[3] [2] [1 3] [2 3] [0 2] [0 1]]
   Note that the result is sorted on length of groups"
  [buttons]
  (->>
   buttons
   (re-seq #"\(.+?\)")
   (map #(mapv parse-long
               (str/split
                (subs % 1 (dec (count %)))
                #",")))
   (sort-by count)))


"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
(defn parse-line [line]
  (let [[_ lights buttons joltage] (re-find #"\[(.*)\] (\(.*\)) \{(.*)\}" line)]
    [(map #(if (= '\# %) 1 0) lights)
     (parse-buttons buttons)
     (mapv parse-long (str/split joltage #","))]))


;; light - buttons - joltage
(defn process-line [line]
  (let [[lights buttons _] parse-line]
    10))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (reduce + (map process-line lines))))


;; ------------------------------------------------------------
;; Unittesting
;; ------------------------------------------------------------

(defn test-parse-line []
  (let [input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
        expected [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
    (is expected (parse-line input))))


(defn run-debug []
  (test-parse-line))


(run-debug)

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
  (is 7 (crack-the-code (input test-file) true)))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 9:" (crack-the-code (input input-file)))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
