(ns day4.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

(def input-file "../../inputs/day4.txt")
(def test-file  "../../inputs/day4-test.txt")

(def paper-chr "@")
(def null-chr ".")

;; Create a new object that extend the initial
;; lines with `.`, so it is surrounded by non-paper
;; This way all the processed elements (lines) will
;; be surrounded by data, and we need less exceptions
;; Yes this is probably more expensive :)
(defn create-extended-lines [lines]
  (let [grid-width (count (first lines))
        grid-height (count lines)]
    ;; extend range to one before and one after the number of lines
    ;; for the two extra lines we want to add
    (for [y (range -1 (inc grid-height))]
      (cond
        (or (= y -1) (= y grid-height))
        ;; line completely made of "."
        (apply str (for [_ (range (+ grid-width 2))] null-chr))
        :else
        (str null-chr (nth lines y) null-chr)))))


(defn get-accessible-paper-rolls [lines]
  123)


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content  (fs/readFileSync abs-path "utf8")
        lines (str/split-lines content)
        ;; line-len (count(first lines))
        lines-buffer (create-extended-lines lines)]
    (get-accessible-paper-rolls lines-buffer)))


;; ------------------------------------------------------------
;; Unit Tests
;; ------------------------------------------------------------

(deftest test-buffer-lines-creation
  (let [input ["abcde" "fghijkl"]
        expected ["......."
                  ".abcde."
                  ".fghijkl."
                  "......."]]
    (is (= expected (create-extended-lines input)))))


(deftest test-free-papers-1
  (let [input ["..."
               ".@."
               "..."]]
    (is (= 1 (get-accessible-paper-rolls input)))))

(deftest test-free-papers-2
  (let [input ["....."
               ".@@@."
               ".@@@."
               ".@@@."
               "....."]]
    (is (= 4 (get-accessible-paper-rolls input)))))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data
  (is (= 13 (crack-the-code test-file))))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 4:" (crack-the-code input-file)))


(defn test-and-run []
  (when (run-tests)
    (main)))


(test-and-run)
