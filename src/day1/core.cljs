(ns day1.core
  (:require ["fs" :as fs]
            ["path" :as path]))


;; need a function that takes:
;; filepath
;; export number


(defn numbers-from-file [filepath]
  (js/console.log js/__dirname)
  (let [abs-filepath (path/join js/__dirname filepath)]
    (println "Reading numbers from file:" abs-filepath)
    (fs/readFile abs-filepath "utf8"
                 (fn [err data]
                   (if err (js/console.error "Error loading file!")
                       data)))))


(defn crack-the-code [filepath]
  (let [numbers (numbers-from-file filepath)]
    numbers))


(defn main []
  (let [result (crack-the-code "../../inputs/day1.txt")]
    (println "Result for day 1 is: " result)))


(defn testing []
  (let [result (crack-the-code "../../tests/inputs/test-day1.txt")]
    (= result 3)))

(if (testing)
  (main)
  (println "Test failed, main code didn't run"))