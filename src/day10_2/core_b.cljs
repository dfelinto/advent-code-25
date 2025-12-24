;; I didn't manage to crack this one completely by myself
;; (see my overflown attempt at core_a.cljs
;;
;; But I then did succeed in implementing this algorithm:
;; https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
;;
;; That alone was painful enough that I think I deserved my star :)
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
        ;; Make n 1-based
        (for [n (range 1 (inc (count buttons)))]
          (let [buttons-n (nth buttons (dec n))]
            (mapv #(assoc {} :n n :bts (vec %)) buttons-n)))]
    (flatten-buttons mapped)))

(def flatten-with-count' (memoize flatten-with-count))


(defn get-pattern-from-values
  "[3 5 4 7] -> [ true true false true ]"
  [values]
  (mapv #(odd? %) values))
(def get-pattern-from-values' (memoize get-pattern-from-values))


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


(def evaluate-buttons-total' (memoize evaluate-buttons-total))


(defn evaluate-buttons
  "Convert buttons to the pattern (lights) they turn on
   [ [1 3] [0 2] [0] [5] ] -> [ 0 1 1 0 1 ]
   "
  [buttons max-digit]
  (let [pattern
        (get-pattern-from-values (evaluate-buttons-total' buttons max-digit))
        ;; _  (println "evaluate-buttons" buttons max-digit pattern)
        ]
    pattern))


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
      ;; Begins at range 1, because n=0 is given by the
      ;; for loop above, which is simply all the initial buttons.
      (range 1 buttons-len)))))

(def combine-buttons' (memoize combine-buttons))


(defn half-joltage-safe [joltages]
  (mapv #(if (zero? %) 0 (/ % 2)) joltages))


(defn get-min
  "Always return the smallest branch"
  ([buttons joltages] (get-min buttons joltages false))
  ([buttons joltages verbose?]
   (when verbose? (println "joltages" joltages "buttons" buttons))
   (cond
     ;; Successfully got to a result
     (every? zero? joltages)
     0

     ;; Dead-end
     (some neg-int? joltages)
     2000000

     :else
     (let [pattern (get-pattern-from-values' joltages)
           _  (when verbose? (println  "pattern" pattern))
           button-matches (->>
                           buttons
                           (combine-buttons')
                           (flatten-with-count')
                           (filter #(match-pattern? pattern (:bts %))))

           ;; There are cases where we need to go on both paths, to the "even" split, but
           ;; we need to try the "odds" as well. We do this by first running the "even" split
           ;; when appliable, and then adding it to the "odd" min reduce.
           is-all-even? (every? #(even? %) joltages)
           even-branch (if is-all-even? [(* 2 (get-min' buttons (half-joltage-safe joltages) verbose?))] [])]
       (cond
         (not-empty? button-matches)
         (->> button-matches
              (reduce
               (fn [acc potential-buttons]
                 (let [joltages' (mapv - joltages (evaluate-buttons-total' (:bts potential-buttons) (count joltages)))
                       _ (when verbose? (println potential-buttons))
                       _ (when verbose? (println "joltages'" joltages'))
                       half-joltage (half-joltage-safe joltages')
                       _ (when verbose? (println "half-joltage" half-joltage))
                       branch (get-min' buttons half-joltage verbose?)]
                   (conj acc (+ (* 2 branch) (:n potential-buttons)))))
               even-branch)
              (apply min))


         ;; In this case, the list is empty, but luckily we can still pursuit the "even" path
         (true? is-all-even?)
         (first even-branch)

         ;; If any of the joltages is odd  then it means it is a real dead-end
         ;; since there are no matches and nowhere to go from here.
         :else
         7777777777777)))))



;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
;; [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
;; light - buttons - joltage
(defn process-line
  ([line] (process-line line false))
  ([line verbose?]
   (when verbose? (println line))
   (let [[_ buttons joltages] (parse-line line)
         total (get-min buttons joltages)]
     (when verbose? (println total ":" line))
     total)))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (reduce + (pmap (partial #(process-line % verbose?)) lines))))


;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------

(defn test-single-line-1 []
  (is (crack-the-code ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"] true)
      10))

(defn test-single-line-3 []
  (is (crack-the-code ["[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"] true)
      11))

(defn test-single-line-3-b []
  (is (crack-the-code ["[.###.#] (0,1) (0) (1) {2,2}"] true)
      2))

(defn test-single-line-3-c []
  (is (crack-the-code ["[.###.#] (0,1,2,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"] true)
      11))

(defn test-single-line-4 []
  (is (crack-the-code ["[.#.#] (0,1,2) (1,3) {9,18,9,9}"] true)
      18))

(defn test-single-line-5 []
  (is (crack-the-code ["[..##.#] (0,1,2,5) (0,1,5) (0,5) (2,4) (2,3,5) (0,3,4) {223,218,44,22,9,239}"] true)
      248))

(defn test-single-line-6 []
  (is (crack-the-code ["[.####] (1,2,3,4) (0,2,4) (0,1,3) {18,20,10,20,10}"] true)
      24))

(defn test-single-line-7 []
  (is (crack-the-code ["[.####] (1,2) (0,2) (0,1) {4,4,4}"] true)
      6))

(defn run-tests []
  (test-single-line-3)
  (test-single-line-3-b)
  (test-single-line-3-c)
  (test-single-line-1)
  (test-single-line-4)
  (test-single-line-5)
  (test-single-line-6)
  (test-single-line-7))

;; (run-tests)

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

;; (time (test-sample-data))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 10.2:" (crack-the-code (input input-file) false))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
