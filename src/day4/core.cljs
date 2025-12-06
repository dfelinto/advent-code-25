(ns day4.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

(def input-file "../../inputs/day4.txt")
(def test-file  "../../inputs/day4-test.txt")

(def paper-chr "@")
(def null-chr ".")
(def illegal-papers-around 4)

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


(defn get-kernel [id row-prev row row-next]
  (concat
   ;; subs expects the end index to be exclusive
   (subs row-prev (dec id) (+ 2 id))
   (nth row (dec id)) (nth row (inc id))
   ;; subs expects the end index to be exclusive
   (subs row-next (dec id) (+ 2 id))))


;; look for all elements in row and return the ammount
;; of accessible paper rolls
(defn process-line
  ([row-prev row row-next] (process-line row-prev row row-next false))
  ([row-prev row row-next verbose?]
   (when verbose? (println "proccess-line:" row-prev row row-next))
   (when verbose? (println "row" row "process-row" (first (butlast row))))
   (:acc
    (reduce (fn [{:keys [id acc]} chr]
              ;;  (println "chr" chr)
              (if (= chr paper-chr)
                (let [kernel (get-kernel id row-prev row row-next)
                      ;; count the ammount of papers around the current chr
                      ;; aka count the papers inside the kernel
                      num-papers (apply + (for [k kernel :when (= k paper-chr)] 1))
                      _ (when verbose?
                          (println "id" id)
                          (println "kernel" (apply str kernel))
                          (println "num-papers" num-papers))]
                  (if (< num-papers illegal-papers-around)
                    {:id (inc id) :acc (inc acc)}
                    {:id (inc id) :acc acc}))
                {:id (inc id) :acc acc}))
            ;; Begin at the second character (:id 1)
            {:id 1 :acc 0}
            ;; Remove the first and final characters
            ;; so we only process the "meat" of the line (without the buffer)
            (drop 1 (butlast row))))))


;; Find how many paper rolls are surrounded by less than 4 rolls
(defn get-accessible-paper-rolls
  ([lines] (get-accessible-paper-rolls lines false))
  ([lines verbose?]
   (:papers
    ;; Iterate over the third line onward, and always process the middle line
    ;; This way we skip the buffer lines and deal only with the real data
    (reduce
     (fn [{:keys [row1 row2 papers]} row3]
       (when verbose? (println "Row 1:" row1 "Row 2:" row2 "Row 3:" row3))
       {:row1 row2 :row2 row3 :papers (+ papers (process-line row1 row2 row3 verbose?))})
     {:row1 (first lines) :row2 (second lines) :papers 0}
     (drop 2 lines)))))


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


(deftest test-get-kernel
  (let [input ["abcde"
               "fghij"
               "klmno"
               "pqrst"
               "uvxyz"]
        expected1 (apply str ["abc" "f" "h" "klm"]);;g
        expected2 (apply str ["bcd" "g" "i" "lmn"]);;h
        ]
    (is (= expected1 (apply str (get-kernel 1 (first input) (second input) (nth input 2)))))
    (is (= expected2 (apply str (get-kernel 2 (first input) (second input) (nth input 2)))))))


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

;; (enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
