
(ns day8.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day8.txt")
(def test-file  "inputs/day8-test.txt")

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code
  ([filepath] (crack-the-code filepath false))
  ([filepath verbose?]
   (println "Reading sequence from file:" filepath)
   (->>
    filepath
    (fs/absolutize)
    str
    slurp
    (str/split-lines)
    (println))))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(defn test-sample-data []
  (is 40 (crack-the-code test-file true)))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 7.2:" (crack-the-code input-file true))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
