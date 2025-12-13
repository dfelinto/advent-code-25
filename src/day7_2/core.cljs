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
  [ray-idx line]
  (println "  "
           (apply str
                  (map-indexed #(cond
                                  (= %2 '\^)
                                  '\^
                                  (= ray-idx %1)
                                  '\|
                                  :else
                                  '.) line))))


(defn process-line
  ([ray-idx line next-lines] (process-line ray-idx line next-lines false))
  ([ray-idx line next-lines verbose?]
   (when verbose? (ptree ray-idx line))
   (let [[next-line & lines] next-lines]
     (cond
       ;; Timeline ends here
       (nil? next-line)
       1

       (= (nth line ray-idx) '\^)
       (+ (process-line (dec ray-idx) next-line lines false)
          (process-line (inc ray-idx) next-line lines verbose?))

       :else
       (process-line ray-idx next-line lines verbose?)))))


(defn process-tree
  ([lines] (process-tree lines false))
  ([lines verbose?]
   (let [[first-line & lines] lines
         ray-idx (.indexOf first-line '\S)
         [second-line & lines] lines]
     (process-line ray-idx second-line lines verbose?))))


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
;; Unittests
;; ------------------------------------------------------------

(deftest test-tree-1 []
  (let [input
        ["....S...."
         "........."
         "........."
         "........."
         "........."]]
    (is (= 1 (process-tree input)))))


(deftest test-tree-2 []
  (let [input
        ["....S...."
         "........."
         ".^.....^."
         "........."
         "........."]]
    (is (= 1 (process-tree input)))))


(deftest test-tree-3 []
  (let [input
        ["....S...."
         "........."
         "....^...."
         "........."
         "........."]]
    (is (= 2 (process-tree input)))))


(deftest test-tree-4 []
  (let [input
        ["....S...."
         "....^...."
         "...^....."
         "........."
         "........."]]
    (is (= 3 (process-tree input)))))


(deftest test-tree-5 []
  (let [input
        ["....S...."
         "....^...."
         "...^.^..."
         "........."
         "........."]]
    (is (= 4 (process-tree input)))))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 40 (crack-the-code test-file true))))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 7.2:" (crack-the-code input-file true))))

(enable-console-print!)
;; (run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
