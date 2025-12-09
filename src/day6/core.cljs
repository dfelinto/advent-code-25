(ns day6.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day6.txt")
(def test-file  "../../inputs/day6-test.txt")


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")]
    999))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 4277556 (crack-the-code test-file))))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 6:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
