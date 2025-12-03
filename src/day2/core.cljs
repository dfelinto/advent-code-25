(ns day2.core)

(println "Day 2 baby! Santa here I come")

(def INPUT_FILE "../../inputs/day2.txt")
(def TEST_FILE "../../inputs/day2-test.txt")


(defn crack-the-code [filepath]
  123)


;; ---- CLI / Test ----
(defn main []
  (println "Result for day 2:" (crack-the-code INPUT_FILE)))

(defn testing []
  (let [result (crack-the-code TEST_FILE)
        expected 1227775554]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))


(testing)
