;; ChatGPT idiomatic pass
;; I really liked the use of `cond` and the abundant use of maps instead of vectors
;; also removing the unecessary `let` in the `crack-the-code` function was a nice touch.

(ns day3-2.ai
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def input-file "../../inputs/day3.txt")
(def test-file  "../../inputs/day3-test.txt")
;; (def test-file "../../inputs/day3-test-debug.txt")

(def max-digits 12)

;; ------------------------------------------------------------
;; Find the highest digit in a string starting at index 0.
;; Return [pos digit]
;; ------------------------------------------------------------
(defn find-highest-digit [s]
  (let [{:keys [best-pos best-digit]}
        (reduce
         (fn [{:keys [idx best-pos best-digit] :as acc} ch]
           (let [n (js/parseInt ch)
                 idx' (inc idx)]
             (cond
               ;; Early stop if we hit 9 (use same keys shape)
               (= n 9)
               (reduced {:idx idx' :best-pos idx :best-digit 9})

               ;; Found a larger digit
               (> n best-digit)
               {:idx idx' :best-pos idx :best-digit n}

               ;; No change
               :else
               {:idx idx' :best-pos best-pos :best-digit best-digit})))
         ;; initial accumulator
         {:idx 0 :best-pos 0 :best-digit -1}
         s)]
    [best-pos best-digit]))


;; ------------------------------------------------------------
;; Find the best digit starting *after* position.
;; sub-line is guaranteed to have enough digits right of it.
;; Return [absolute-pos digit]
;; ------------------------------------------------------------
(defn find-digit [sub-line position]
  (let [sub-line (subs sub-line (inc position))
        [rel-pos digit] (find-highest-digit sub-line)
        abs-pos (+ rel-pos position 1)]
    [abs-pos digit]))


;; ------------------------------------------------------------
;; Build the battery number by selecting max-digits digits.
;; For each step we:
;; 1. Determine the remaining digits needed
;; 2. Cut the safe substring
;; 3. Pick the next digit and its position
;; 4. Accumulate into battery (base-10)
;; ------------------------------------------------------------
(defn battery-from-line [line]
  (println "Processing line:" line)
  (let [len (count line)]
    (:battery
     (reduce
      (fn [{:keys [battery parent-pos]} idx]
        (let [remaining  (- max-digits idx 1)
              safe-line  (subs line 0 (- len remaining))
              [child-pos digit] (find-digit safe-line parent-pos)]
          {:battery (+ digit (* 10 battery))
           :parent-pos child-pos}))
      {:battery 0 :parent-pos -1}
      (range max-digits)))))


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (let [abs-path (path/join js/__dirname filepath)
        content  (fs/readFileSync abs-path "utf8")]
    (println "Reading sequence from file:" abs-path)
    (reduce (fn [acc line]
              (+ acc (battery-from-line line)))
            0
            (str/split-lines content))))


;; ------------------------------------------------------------
;; CLI / Tests
;; ------------------------------------------------------------
(defn main []
  (println "Result for day 3.2:" (crack-the-code input-file)))

(defn testing []
  (let [result   (crack-the-code test-file)
        expected 3121910778619]
    (if (= result expected)
      (main)
      (println "Test failed:" result "(expected" expected ")"))))

(testing)
