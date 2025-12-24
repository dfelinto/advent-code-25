
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


(def not-empty? (complement empty?))


(declare get-min)
(def get-min' (memoize (fn [& args] (apply get-min args))))


(defn flatten-buttons
  "Flatten the sequence completely"
  [buttons]
  (mapcat #(identity %) buttons))


(defn flatten-buttons-per-n
  "Flatten the sequence, while preserving the vecs which were the leaves"
  [buttons]
  (map #(flatten-buttons %) buttons))


(defn flatten-with-count
  "Break down the buttons into an array of {}, indexed with n (number of pressed buttons)
   [[[2] [2 4]] [[3] [5 2 7]]] ->  [ {:n 0 :bts [2]} {:n 0 :bts [2 4]} {:n 1 :bts [3]}  {:n 1 :bts [5 2 7]} ]"
  [buttons]
  (let [mapped
        ;; n is 1-based
        (for [n (range 1 (inc (count buttons)))]
          (let [buttons-n (nth buttons (dec n))]
            (mapv #(assoc {} :n n :bts %) buttons-n)))]
    (flatten-buttons mapped)))


(defn get-pattern-from-values
  "[3 5 4 7] -> [ true true false true ]"
  [values]
  (mapv #(odd? %) values))
;; (def get-pattern-from-joltages' (memoize get-pattern-from-joltages))


(defn count-digits-in-vec
  "Return how many of a given digit there is in a vec."
  [vec digit]
  (count (filter #(= % digit) vec)))


(defn evaluate-buttons-total
  "Convert buttons to the pattern (lights) they turn on with count
   [ [1 3] [0 2] [0] [5] ] -> [ 2 1 1 0 1 ]
   "
  [buttons max-digit]
  ;; (let [fl-buttons (flatten-buttons buttons)
  ;;       _ (println fl-buttons)])
  (mapv #(count-digits-in-vec buttons %)
        (range max-digit)))


(defn evaluate-buttons
  "Convert buttons to the pattern (lights) they turn on
   [ [1 3] [0 2] [0] [5] ] -> [ 0 1 1 0 1 ]
   "
  [buttons max-digit]
  (get-pattern-from-values (evaluate-buttons-total buttons max-digit)))


(defn match-pattern? [pattern buttons]
  (= pattern (evaluate-buttons buttons (count pattern))))


(defn combine-buttons
  "Create all possible combinations between the buttons to a maximum of `n-max` buttons together"
  [buttons]
  (let [buttons-len (count buttons)]
    (flatten-buttons-per-n
     (reduce
      (fn [acc n]
        (let [prev (last acc)]
          (conj
           acc
           (for [idx (range (- buttons-len n))]
             (let [head (get buttons idx)
                   tail (->>
                         prev
                         (drop (inc idx))
                         (reduce concat))]
               (map #(concat head  %) tail))))))
      [(for [button buttons] [button])]
      (range 1 buttons-len)))))

(def combine-buttons' (memoize combine-buttons))


(defn get-min
  "Always return the smallest branch"
  ([buttons joltages] (get-min' 0 buttons joltages))
  ([total buttons joltages]
   (cond
     (every? zero? joltages)
     total

     (some neg-int? joltages)
     2000000

     ;;  ;; This is the maximum amount of clicks it can possibly need
     ;;  (> total (long (/ (apply + joltages) (apply min (mapv count buttons)))))
     ;;  3000000

     :else
     (let [pattern (get-pattern-from-values joltages)
           ;;  _  (println joltages total)
           button-matches (->>
                           buttons
                           (combine-buttons')
                           (flatten-with-count)
                           (filter #(match-pattern? pattern (:bts %))))]

       (if (not-empty? button-matches)
         (apply min
                (reduce
                 (fn [acc potential-buttons]
                   (let [joltages' (mapv - joltages (evaluate-buttons-total (:bts potential-buttons) (count joltages)))
                         _ (println potential-buttons)
                         half-joltage (mapv #(if (zero? %) 0 (/ % 2)) joltages')
                         branch (get-min' buttons half-joltage)]
                     (conj acc (+ total (* 2 branch) (:n potential-buttons)))))
                 []
                 button-matches))

         ;; If empty it means there was no match in the branch
         1000000)))))



;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
;; [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
;; light - buttons - joltage
(defn process-line
  ([line] (process-line line false))
  ([line verbose?]
   ;;  (println line)
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

;; Reference line/value from the internet
;; 19, 199, 2, 6, 19, 3.
(defn test-single-line-2 []
  (is (crack-the-code ["[..##.#] (0,1,2,5) (0,1,5) (0,5) (2,4) (2,3,5) (0,3,4) {223,218,44,22,9,239}"] true)
      248))

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
