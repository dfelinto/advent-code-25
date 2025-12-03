(ns day1.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day1.txt")
(def TEST_FILE "../../tests/inputs/test-day1.txt")


(defn numbers-from-file [filepath callback]
  (let [abs_filepath (path/join js/__dirname filepath)]
    (println "Reading numbers from file:" abs_filepath)
    ;; Synchronous file reading otherwise we can't validate test before
    ;; deciding whether to run the main file.
    (fs/readFile abs_filepath "utf8"
                 (fn [err data]
                   (if err (js/console.error "Error loading file!")
                       (callback (str/split-lines data)))))))


(defn crack-the-code [filepath callback]
  (let [position_initial 50
        count (atom 0)
        position_cur position_initial]
    (numbers-from-file filepath
                       (fn [lines]
                         (doseq [line lines]
                           ;; TODO: do logic
                           (swap! count inc))
                         (callback @count)))))


(defn main []
  (crack-the-code INPUT_FILE
                  (fn [result]
                    (println "Result for day 1:" result))))


(defn testing []
  (crack-the-code TEST_FILE
                  (fn [result]
                    (if (= result 3)
                      (main)
                      (println "Test failed:" result "(expected 3)")))))



(testing)
