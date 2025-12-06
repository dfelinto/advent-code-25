(ns day4.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

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
    lines-buffer))


;; ------------------------------------------------------------
;; CLI / Tests
;; ------------------------------------------------------------
(defn main []
  (println "Result for day 4:" (crack-the-code input-file)))

(defn testing []
  (let [result   (crack-the-code test-file)
        expected 13]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))


(defn testing []
  (let [lines ["abcde" "fghijkl"]]
    (println (create-extended-lines lines))))

(testing)
