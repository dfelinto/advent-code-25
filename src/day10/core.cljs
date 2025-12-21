
(ns day10.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day10.txt")
(def test-file  "inputs/day10-test.txt")


(defn is [got expected] (if (= expected got) true (print "Expected  :" expected "\n\nError, got:" got "\n")))
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
  (mapcat #(mapcat identity %) buttons)) ;; chatgpt version
;; behold my version using nested reduce:
;; (reduce (fn [acc n]
;;           (concat
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
           (for [idx (range (- buttons-len n))]
             (let [head (get buttons idx)
                   tail (->>
                         prev
                         (drop (inc idx))
                         (reduce concat))]
               (map #(concat head  %) tail))))))
      [(for [button buttons] [button])]
      (range 1 n-max)))))


;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
;; [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
;; light - buttons - joltage
(defn process-line [line]
  (println "line" line)
  (let [[lights buttons _] (parse-line line)
        ;; buttons (mapv vec buttons)
        _ (prn "buttons" buttons)
        ;; TODO need to make sure this is indeed lazy
        buttons-combinations (combine-buttons buttons (count buttons))
        ;; buttons-combinations (doall (combine-buttons buttons 2))
        ;;
        ]
    (println buttons-combinations)
    ;;
    )
  12)


;; TODO remove even pairs of the same value (not needed, but I probably should)
;; TODO Use the buttons to turn on the lights
;; TODO::: something seems to be be working for the real data
;; funny enough my 1:1 test works. So it must be the data type, some () vs []
;; NEED to check the flatten function and the prev. one


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (reduce + (mapv process-line lines))))


;; ------------------------------------------------------------
;; Unittesting
;; ------------------------------------------------------------

(def debug-first-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

(def debug-lights [[3] [2] [1 3] [2 3] [0 2] [0 1]])

(defn test-parse-line []
  (let [expected [[0 1 1 0] [[3] [2] [1 3] [2 3] [0 2] [0 1]] [3 5 4 7]]]
    (is expected (parse-line debug-first-line))))

(defn test-combine-buttons-data-1 []
  (is (combine-buttons debug-lights 1) debug-lights))

(defn test-combine-buttons-data-2 []
  (is (combine-buttons debug-lights 2)
      '([3] [2] [1 3] [2 3] [0 2] [0 1]
            [3 2] [3 1 3] [3 2 3] [3 0 2] [3 0 1]
            [2 1 3] [2 2 3] [2 0 2] [2 0 1]
            [1 3 2 3] [1 3 0 2] [1 3 0 1]
            [2 3 0 2] [2 3 0 1]
            [0 2 0 1])))


(defn test-combine-buttons-data-2-plus []
  (let [[_ buttons _] (parse-line debug-first-line)
        ;; vbuttons (mapv vec buttons)
        ]
    (is (combine-buttons buttons 2)
        '([3] [2] [1 3] [2 3] [0 2] [0 1]
              [3 2] [3 1 3] [3 2 3] [3 0 2] [3 0 1]
              [2 1 3] [2 2 3] [2 0 2] [2 0 1]
              [1 3 2 3] [1 3 0 2] [1 3 0 1]
              [2 3 0 2] [2 3 0 1]
              [0 2 0 1]))))


(defn test-combine-buttons-1 []
  (is  (combine-buttons [["a"] ["b"] ["c"]] 1)
       '(["a"] ["b"] ["c"])))

(defn test-combine-buttons-2 []
  (is (combine-buttons [["a"] ["b"] ["c"]] 2)
      '(["a"] ["b"] ["c"]
              ["a" "b"] ["a" "c"] ["b" "c"])))

(defn test-combine-buttons-3-simpler []
  (is (combine-buttons [["a"] ["b"] ["c"]] 3)
      '(["a"] ["b"] ["c"]
              ["a" "b"] ["a" "c"]
              ["b" "c"]
              ["a" "b" "c"])))

(defn test-combine-buttons-3 []
  (is (combine-buttons [["a"] ["b"] ["c"] ["d"]] 3)
      '(["a"] ["b"] ["c"] ["d"]
              ["a" "b"] ["a" "c"] ["a" "d"]
              ["b" "c"] ["b" "d"] ["c" "d"]
              ["a" "b" "c"] ["a" "b" "d"] ["a" "c" "d"] ["b" "c" "d"])))


(defn test-combine-buttons-4 []
  (is (combine-buttons [["a"] ["b"] ["c"] ["d"]] 4)
      '(["a"] ["b"] ["c"] ["d"]
              ["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"]
              ["a" "b" "c"] ["a" "b" "d"] ["a" "c" "d"] ["b" "c" "d"]
              ["a" "b" "c" "d"])))


(defn test-combine-buttons-5
  "The only combine-buttons test which actually has vec as input"
  []
  (is (combine-buttons [[0 1] [2] [3 4]] 3)
      '([0 1] [2] [3 4]
              [0 1 2] [0 1 3 4] [2 3 4]
              [0 1 2 3 4])))



(defn run-debug []
  (test-parse-line)
  (test-combine-buttons-data-1)
  (test-combine-buttons-data-2)
  (test-combine-buttons-data-2-plus)
  (test-combine-buttons-1)
  (test-combine-buttons-2)
  (test-combine-buttons-3-simpler)
  (test-combine-buttons-3)
  (test-combine-buttons-4)
  (test-combine-buttons-5)
  ;;
  )


(run-debug)

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
  (is 7 (crack-the-code (input test-file) true)))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 9:" (crack-the-code (input input-file)))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
