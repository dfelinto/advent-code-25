(ns day7.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day7.txt")
(def test-file  "../../inputs/day7-test.txt")

(defn new-rays [new-line old-line]
  (letfn [(line-total [line]
            (count (filter true? line)))]
    (- (line-total new-line) (line-total old-line))))

(defn line-rays
  "Return a seq of bools for each ray in a cell"
  [line prev-line]
  (let [line-splitters (map #(= % '\^) line)
        line-len (count line)]
    (println prev-line line)
    (cond
      (zero? (count (filter true? line-splitters)))
      prev-line
      :else
      (map #(cond
              (nth prev-line %)
              true
              (zero? %)
              false
              (= (dec line-len) %)
              false
              :else
              (or (nth line-splitters (dec %))
                  (nth line-splitters (inc %))))
           (range line-len)))))


(defn process-tree [lines]
  (let [first-line (map #(= % '\S) (first lines))]
    (:acc
     (reduce (fn [{:keys [acc prev-line]} line]
               ;; a chr is true if:
               ;; parent is true (nop)
               ;; sibling is an active split (+1)
               (let [new-line (line-rays line prev-line)
                     new-acc (+ acc (new-rays new-line prev-line))]
                 {:acc new-acc :prev-line new-line}))
             {:acc 0 :prev-line first-line}
             (rest lines)))))

;; (count (filter #(= % true) a))
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
        lines (str/split-lines content)]
    (process-tree lines)))


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
