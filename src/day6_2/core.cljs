(ns day6-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day6.txt")
(def test-file  "../../inputs/day6-test.txt")


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")
        lines (str/split-lines content)
        numbers (->>
                 (pop lines)
                 (map #(str/split (str/triml %) #" +"))
                 (apply concat)
                 (map parse-long))
        ops (map #(if (= % "+") + *)
                 (->
                  lines
                  peek
                  (str/split #" +")))
        line-len (count ops)]
    ;; (println "Numbers: " numbers)
    ;; (println "Operators: " ops)
    ;; (println "Line length:" line-len)
    (first
     (reduce
      (fn [[acc numbers-left] op]
        ;; (println "Inside: " acc op)
        ;; (println "Test: " (op 2 3))
        ;; (println "partition =" (partition 1 line-len numbers-left))
        [(+ acc
            (->>
             numbers-left
             (partition 1 line-len)
             (apply concat)
             (reduce op)))
         (rest numbers-left)])
      [0 numbers]
      ops))))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 3263827 (crack-the-code test-file))))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 6:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
