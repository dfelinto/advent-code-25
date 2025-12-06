(ns day4.ai-plus
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

;; ------------------------------------------------------------
;; Config
;; ------------------------------------------------------------
(def input-file "../../inputs/day4.txt")
(def test-file  "../../inputs/day4-test.txt")

(def paper-chr "@")
(def null-chr ".")
(def illegal-papers-around 4)

(def testing? false) ;; set false to run main after tests

;; ------------------------------------------------------------
;; Core functions
;; ------------------------------------------------------------

(defn pad-lines
  "Add a border of null-chr around the grid."
  [lines]
  (let [w (count (first lines))
        border (apply str (repeat (+ w 2) null-chr))]
    (vec (concat
          [border]
          (map #(str null-chr % null-chr) lines)
          [border]))))

(defn get-kernel
  "Return the 3x3 neighborhood of the character at (row-idx col-idx) as a string."
  [row-idx col-idx lines]
  (let [prev (lines (dec row-idx))
        curr (lines row-idx)
        next (lines (inc row-idx))]
    (str
     (subs prev (dec col-idx) (+ col-idx 2))
     (nth curr (dec col-idx))
     (nth curr (inc col-idx))
     (subs next (dec col-idx) (+ col-idx 2)))))

(defn process-row
  "Count accessible paper rolls in one row (ignore first/last column)."
  [row-idx lines]
  (reduce
   (fn [acc col-idx]
     (let [chr (get-in lines [row-idx col-idx])]
       (if (= chr paper-chr)
         (let [kernel (get-kernel row-idx col-idx lines)
               num    (count (filter #(= % paper-chr) kernel))]
           (if (< num illegal-papers-around)
             (inc acc)
             acc))
         acc)))
   0
   (range 1 (dec (count (first lines))))))

(defn count-accessible-papers [lines]
  (let [padded (pad-lines lines)
        rows (range 1 (dec (count padded)))]
    (reduce (fn [acc row-idx]
              (+ acc (process-row row-idx padded)))
            0
            rows)))

(defn crack-the-code [filepath]
  (let [abs (path/join js/__dirname filepath)
        content (fs/readFileSync abs "utf8")
        lines   (str/split-lines content)]
    (count-accessible-papers lines)))

;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------

(deftest test-padding
  (let [input ["abcde" "fghijkl"]
        expected ["......."
                  ".abcde."
                  ".fghijkl."
                  "......."]]
    (is (= expected (vec (pad-lines input))))))

(deftest test-get-kernel
  (let [input ["abcde"
               "fghij"
               "klmno"]]
    (is (= "abcfhklm" (get-kernel 1 1 input)))
    (is (= "bcdgilmn" (get-kernel 1 2 input)))))

(deftest test-simple-grid-1
  (let [input ["..."
               ".@."
               "..."]]
    (is (= 1 (count-accessible-papers input)))))

(deftest test-simple-grid-2
  (let [input ["....."
               ".@@@."
               ".@@@."
               ".@@@."
               "....."]]
    (is (= 4 (count-accessible-papers input)))))

(deftest test-sample-file
  (is (= 13 (crack-the-code test-file))))

;; ------------------------------------------------------------
;; Main runner
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 4:" (crack-the-code input-file)))

(defn run-tests-and-main []
  ;; run tests (prints results)
  (run-tests 'day4.core)
  ;; optionally run main after tests
  (when-not testing?
    (main)))

;; ------------------------------------------------------------
;; Entry point
;; ------------------------------------------------------------
(run-tests-and-main)