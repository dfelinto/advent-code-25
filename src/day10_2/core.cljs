
(ns day10-2.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day10.txt")
(def test-file  "inputs/day10-test.txt")


(defn is [got expected] (if (= expected got) true (print "\nExpected  :" expected "\nError, got:" got "\n")))
;; (defn is [& _])


(defn parse-buttons
  "(3) (1,3) (2) (2,3) (0,2) (0,1) -> [[3] [2] [1 3] [2 3] [0 2] [0 1]]
   Note that the result is sorted on length of groups"
  [buttons]
  (->>
   buttons
   (re-seq #"\(.+?\)")
   (map #(map parse-long
              (str/split
               (subs % 1 (dec (count %)))
               #",")))
   (sort-by count)
   (mapv vec)))


"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
(defn parse-line [line]
  (let [[_ lights buttons joltage] (re-find #"\[(.*)\] (\(.*\)) \{(.*)\}" line)]
    [(map #(if (= '\# %) 1 0) lights)
     (parse-buttons buttons)
     (mapv parse-long (str/split joltage #","))]))


(defn flatten-buttons
  "Flatten the sequence, while preserving the vecs which were the leaves"
  [buttons]
  (map #(mapcat identity %) buttons))
;; (reduce (fn [acc n]
;;           (conj
;;            acc
;;            (reduce (fn [acc i] (concat acc i)) [] n)))
;;         [] buttons))


(defn combine-buttons
  "Create all possible combinations between the buttons to a maximum of `n-max` buttons together"
  [buttons n-max]
  (let [buttons-len (count buttons)]
    (flatten-buttons
     (reduce
      (fn [acc n]
        (let [prev (last acc)]
          (conj
           acc
           (for [idx (range (buttons-len))]
             (let [head (get buttons idx)
                   tail (->>
                         prev
                         (drop idx)
                         (reduce concat))]
               (map #(concat head  %) tail))))))
      [(for [button buttons] [button])]
      (range 1 n-max)))))


;; Although this solution worked, it doesn't run on babashka
;;
;; (defn count-digits-in-vec
;;   "Return how many of a given digit there is in a vec.
;;    Expects vec to be sorted"
;;   [vec digit]
;;   (let [from (.indexOf vec digit)
;;         to (.lastIndexOf vec digit)]
;;     (if (= from -1)
;;       0
;;       (- (inc to) from)))
;;   )

(defn count-digits-in-vec
  "Return how many of a given digit there is in a vec."
  [vec digit]
  (count (filter #(= % digit) vec)))


(defn evaluate-buttons
  "Convert buttons to the lights they turn on
   [ 1 1 4 0 2 0 0 ] -> [ 1 0 1 0 1 ]
   "
  [buttons max-digit]
  (map #(mod (count-digits-in-vec buttons %) 2)
       (range max-digit)))


;; Abandoned function - I decided to stick to a simpler vector of 0 and 1s
;; This was converting the array to a long
;; (defn convert-light
;; [lights]
;;   (let [rev-lights (reverse lights)]
;;     (reduce (fn [acc idx]
;;               (let [light (nth rev-lights idx)]
;;                 (+ acc (* light (Math/pow 10 idx)))))
;;             0
;;             (range (count lights)))))


(defn match-lights? [lights buttons]
  (= lights (evaluate-buttons buttons (count lights))))


;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
;; [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
;; light - buttons - joltage
(defn process-line [line]
  (let [[lights buttons _] (parse-line line)
        buttons-combinations (combine-buttons buttons (count buttons))
        result
        (reduce
         (fn [idx buttons-combinations]
           (if (some #(match-lights? lights %) buttons-combinations)
             (reduced idx)
             (inc idx)))
         1
         buttons-combinations)]
    result))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   ;; Since each line is completely independent, pmap gives a 2x speed :)
   (reduce + (pmap process-line lines))))


;; ------------------------------------------------------------
;; Unittesting
;; ------------------------------------------------------------

(def debug-first-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

(def debug-lights [[3] [2] [1 3] [2 3] [0 2] [0 1]])

(defn test-parse-line []
  (let [expected [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
    (is expected (parse-line debug-first-line))))

(defn test-combine-buttons-data-1 []
  (is (combine-buttons debug-lights 1) [debug-lights]))

(defn test-combine-buttons-data-2 []
  (is (combine-buttons debug-lights 2)
      '(([3] [2] [1 3] [2 3] [0 2] [0 1])
        ([3 2] [3 1 3] [3 2 3] [3 0 2] [3 0 1]
               [2 1 3] [2 2 3] [2 0 2] [2 0 1]
               [1 3 2 3] [1 3 0 2] [1 3 0 1]
               [2 3 0 2] [2 3 0 1]
               [0 2 0 1]))))


(defn test-combine-buttons-data-2-plus []
  (let [[_ buttons _] (parse-line debug-first-line)]
    (is (combine-buttons buttons 2)
        '(([3] [2] [1 3] [2 3] [0 2] [0 1])
          ([3 2] [3 1 3] [3 2 3] [3 0 2] [3 0 1]
                 [2 1 3] [2 2 3] [2 0 2] [2 0 1]
                 [1 3 2 3] [1 3 0 2] [1 3 0 1]
                 [2 3 0 2] [2 3 0 1]
                 [0 2 0 1])))))


(defn test-process-line []
  (is (process-line debug-first-line)
      2))


(defn test-combine-buttons-1 []
  (is  (combine-buttons [["a"] ["b"] ["c"]] 1)
       '((["a"] ["b"] ["c"]))))

(defn test-combine-buttons-2 []
  (is (combine-buttons [["a"] ["b"] ["c"]] 2)
      '((["a"] ["b"] ["c"])
        (["a" "b"] ["a" "c"] ["b" "c"]))))

(defn test-combine-buttons-3-simpler []
  (is (combine-buttons [["a"] ["b"] ["c"]] 3)
      '((["a"] ["b"] ["c"])
        (["a" "b"] ["a" "c"] ["b" "c"])
        (["a" "b" "c"]))))

(defn test-combine-buttons-3 []
  (is (combine-buttons [["a"] ["b"] ["c"] ["d"]] 3)
      '((["a"] ["b"] ["c"] ["d"])
        (["a" "b"] ["a" "c"] ["a" "d"]
                   ["b" "c"] ["b" "d"] ["c" "d"])
        (["a" "b" "c"] ["a" "b" "d"] ["a" "c" "d"] ["b" "c" "d"]))))


(defn test-combine-buttons-4 []
  (is (combine-buttons [["a"] ["b"] ["c"] ["d"]] 4)
      '((["a"] ["b"] ["c"] ["d"])
        (["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"])
        (["a" "b" "c"] ["a" "b" "d"] ["a" "c" "d"] ["b" "c" "d"])
        (["a" "b" "c" "d"]))))


(defn test-combine-buttons-5
  "The only combine-buttons test which actually has vec as input"
  []
  (is (combine-buttons [[0 1] [2] [3 4]] 3)
      '(([0 1] [2] [3 4])
        ([0 1 2] [0 1 3 4] [2 3 4])
        ([0 1 2 3 4]))))


(defn test-count-digits-in-vec-1 []
  (is (count-digits-in-vec [0 0 1 1 1 2 3] 1)
      3))


(defn test-count-digits-in-vec-2 []
  (is (count-digits-in-vec [0 1 1 1 2 3] 0)
      1))

(defn test-count-digits-in-vec-3 []
  (is (count-digits-in-vec [1 1 1 2 3] 4)
      0))


(defn test-evaluate-buttons-1 []
  (is (evaluate-buttons [1 2] 3)
      [0 1 1]))

(defn test-evaluate-buttons-2 []
  (is (evaluate-buttons [0 0 1 1 1 2] 5)
      [0 1 1 0 0]))

(defn test-evaluate-buttons-3 []
  (is (evaluate-buttons [0 0 0 1 1 1 2] 3)
      [1 1 1]))


(defn run-debug []
  (test-parse-line)
  (test-process-line)
  (test-combine-buttons-data-1)
  (test-combine-buttons-data-2)
  (test-combine-buttons-data-2-plus)
  (test-combine-buttons-1)
  (test-combine-buttons-2)
  (test-combine-buttons-3-simpler)
  (test-combine-buttons-3)
  (test-combine-buttons-4)
  (test-combine-buttons-5)
  (test-count-digits-in-vec-1)
  (test-count-digits-in-vec-2)
  (test-count-digits-in-vec-3)
  (test-evaluate-buttons-1)
  (test-evaluate-buttons-2)
  (test-evaluate-buttons-3)
  ;;
  )


;; (run-debug)

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
      38))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 10.2:" (crack-the-code (input input-file)))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
