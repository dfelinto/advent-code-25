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


;; L68
;; L30
;; R48
;; L5
;; R60
;; L55
;; L1
;; L99
;; R14
;; L82

;; (println (str/starts-with? s "L"))  ;; Output: true

;; (let [[dir & rest-chars] s
;;       value (apply str rest-chars)]
;;   (println "Direction:" dir)
;;   (println "Value:" value))


;; return direction (L or R) and the value
(defn decompose-line [line]
  (let [[direction & rest-chars] line
        value (apply str rest-chars)]
    [direction value]))


;; return absolute value from line
;; R=Right means positive numbers
;; L=Left means negative numbers
(defn value-from-line [line]
  (let [[direction value] (decompose-line line)]
    ;; I need to multiple by 1 when R rotation so
    ;; the string gets converted to a number
    (if (= direction "L") (* -1 value) (* 1 value))))


(defn rotate-dial [position offset]
  (mod (+ position offset) 100))


(defn crack-the-code [filepath callback]
  (let [position_initial 50
        count (atom 0)
        position_cur (atom position_initial)]
    (numbers-from-file filepath
                       (fn [lines]
                         (doseq [line lines]
                           (swap! position_cur rotate-dial (value-from-line line))
                           ;; Increase count everytime the dial is on 0
                           (when (= 0 @position_cur)
                             (swap! count inc)))
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
