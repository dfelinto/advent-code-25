(ns day7-2.core
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
    (cond
      (zero? (count (filter true? line-splitters)))
      prev-line
      :else
      (map #(cond
              (nth line-splitters %)
              false
              (nth prev-line %)
              true
              (zero? %)
              (nth line-splitters (inc %))
              (= (dec line-len) %)
              (nth line-splitters (dec %))
              :else
              (or (nth line-splitters (dec %))
                  (nth line-splitters (inc %))))
           (range line-len)))))


(defn ptree
  "Debug function to print the current line with rays and splitters"
  [line new-line active-splitters]
  (println "  "
           (apply str
                  (map #(cond
                          (true? %1)
                          '\|
                          (true? %2)
                          '\+
                          (= '\^ %3)
                          '\-
                          :else
                          '\.) new-line active-splitters line))))


(defn process-tree
  ([lines] (process-tree lines false))
  ([lines verbose?]
   (let [first-line (map #(= % '\S) (first lines))]
     (:acc
      (reduce (fn [{:keys [acc prev-line]} line]
                (let [new-line (line-rays line prev-line)
                      active-splitters (->>
                                        line
                                        (map #(and %1 (= %2 '\^)) prev-line))
                      active-splitters-count (->>
                                              active-splitters
                                              (filter true?)
                                              count)
                      new-acc (+ acc active-splitters-count)]
                  (when verbose? (ptree line new-line active-splitters))
                  {:acc new-acc :prev-line new-line}))
              {:acc 0 :prev-line first-line}
              (rest lines))))))


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code
  ([filepath] (crack-the-code filepath false))
  ([filepath verbose?]
   (println "Reading sequence from file:" filepath)
   (let [abs-path (path/join js/__dirname filepath)
         ;; _ (println "Reading sequence from file:" abs-path)
         content (fs/readFileSync abs-path "utf8")
         lines (str/split-lines content)]
     (process-tree lines verbose?))))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 40 (crack-the-code test-file true))))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 7.2:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
