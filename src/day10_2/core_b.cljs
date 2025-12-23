
(ns day10-2.core-b
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day10.txt")
(def test-file  "inputs/day10-test.txt")


(defn is [got expected] (if (= expected got) true (print "\nExpected  :" expected "\nError, got:" got "\n")))
;; (defn is [& _])


(defn parse-buttons
  "(3) (1,3) (2) (2,3) (0,2) (0,1) -> [[3] [1 3] [2] [2 3] [0 2] [0 1]]"
  [buttons]
  (->>
   buttons
   (re-seq #"\(.+?\)")
   (map #(map parse-long
              (str/split
               (subs % 1 (dec (count %)))
               #",")))
   (mapv vec)))


"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
(defn parse-line [line]
  (let [[_ lights buttons joltage] (re-find #"\[(.*)\] (\(.*\)) \{(.*)\}" line)]
    [(map #(if (= '\# %) 1 0) lights)
     (parse-buttons buttons)
     (mapv parse-long (str/split joltage #","))]))


;; ```
;; find the odd buttons
;; for each one of them, create a new branch:
;;   +1
;;   find the off buttons
;;     for each one of them, create a new branch:
;;       +1
;; else
;;   divide J / 2
;;   +2
;;   find the odd buttons
;; ```

(def not-empty? (complement empty?))


(declare get-min)
(def get-min' (memoize (fn [& args] (apply get-min args))))


(defn get-min
  "Always return the smallest branch"
  ([buttons joltages] (get-min' 0 buttons joltages))
  ([total buttons joltages]
   ;;  (println joltages)
   (let [odd-buttons (filter (fn [b] (some #(odd? (get joltages %)) b)) buttons)
         ;;  _ (println "odd-buttons" odd-buttons "joltages" joltages "total" total)
         ]
     (cond
       (some neg-int? joltages)
       1000000000000000

       (every? zero? joltages)
       total

       (not-empty? odd-buttons)
       (apply min
              (reduce
               (fn [acc button]
                 (let [joltages' (map-indexed (fn [idx j] (if (some #(= idx %) button) (dec j) (identity j))) joltages)]
                   ;;  (println "new-joltages" joltages' button)
                   (conj acc (get-min' (inc total) buttons (vec joltages')))))
               []
               odd-buttons))

       :else
       (+ total (* 2 (get-min' 0 buttons (mapv #(if (zero? %) 0 (/ % 2)) joltages))))))))



;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
;; [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
;; light - buttons - joltage
(defn process-line
  ([line] (process-line line false))
  ([line verbose?]
   (let [[_ buttons joltages] (parse-line line)
         total (get-min buttons joltages)]
     (when verbose? (println total ":" line))
     total)))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (reduce + (map (partial #(process-line % verbose?)) lines))))


;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------


(defn test-single-line-1 []
  (is (crack-the-code ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"] true)
      10))

(time (test-single-line-1))

(defn test-single-line-2 []
  (is (crack-the-code ["[..##.#] (0,1,2,5) (0,1,5) (0,5) (2,4) (2,3,5) (0,3,4) {223,218,44,22,9,239}"] true)
      10))

(time (test-single-line-2))


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn input
  [filepath]
  (println "Reading sequence from file:" filepath)
  (->>
   filepath
   (fs/absolutize)
   str
   slurp
   (str/split-lines)))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(defn test-sample-data []
  (is (crack-the-code (input test-file) true)
      33))

(time (test-sample-data))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 10.2:" (crack-the-code (input input-file) true))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
