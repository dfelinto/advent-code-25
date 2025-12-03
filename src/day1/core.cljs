(ns day1.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day1.txt")
(def TEST_FILE "../../tests/inputs/test-day1.txt")


;; need a function that takes:
;; filepath
;; export number




(defn numbers-from-file [filepath callback]
  (js/console.log js/__dirname)
  (let [abs_filepath (path/join js/__dirname filepath)]
    (println "Reading numbers from file:" abs_filepath)
    ;; use the synchronous so the data returns right away
    (fs/readFile abs_filepath "utf8"
                 (fn [err data]
                   (if err (js/console.error "Error loading file!")
                       (callback (str/split-lines data)))))))



(defn crack-the-code [filepath]
  (let [position_initial 50
        count 0
        position_cur position_initial]
    (numbers-from-file filepath
                       (fn [lines]
                         (doseq [line lines]
                           (println line position_initial))))
    count))


(defn main []
  (let [result (crack-the-code INPUT_FILE)]
    (println "Result for day 1 is: " result)))


(defn testing []
  (let [result (crack-the-code TEST_FILE)]
    (= result 3)))

(if (testing)
  (main)
  (println "Test failed, main code didn't run"))