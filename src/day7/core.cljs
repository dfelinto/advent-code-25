(ns day7.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day7.txt")
(def test-file  "../../inputs/day7-test.txt")

(defn process-tree [init rest]
  (:acc
   (reduce (fn [{:keys [acc lines]} line]
             (let [prev-rays (->
                              ...)])
             {:acc 1 :lines line})
           {:acc 0 :last-line init}
           rest)))


;; .......S.......
;; ...............
;; ....... ^.......
;; ...............
;; ...... ^. ^......
;; ...............
;; ..... ^. ^. ^.....
;; ...............
;; .... ^. ^... ^....
;; ...............
;; ... ^. ^... ^. ^...
;; ...............
;; .. ^... ^..... ^..
;; ...............
;; . ^. ^. ^. ^. ^... ^.
;; ...............


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        ;; _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")
        lines (str/split-lines content)
        [first-line & rest] lines
        init (.indexOf first-line "S")]
    (process-tree init rest)))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 21 (crack-the-code test-file))))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 7:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
