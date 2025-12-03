(ns day1-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day1.txt")
(def TEST_FILE "../../inputs/day1-test.txt")


(defn numbers-from-file [filepath callback]
  (let [abs_filepath (path/join js/__dirname filepath)]
    (println "Reading numbers from file:" abs_filepath)
    ;; Synchronous file reading otherwise we can't validate test before
    ;; deciding whether to run the main file.
    (fs/readFile abs_filepath "utf8"
                 (fn [err data]
                   (if err (js/console.error "Error loading file!")
                       (callback (str/split-lines data)))))))


;; return direction (L or R) and the value
(defn decompose-line [line]
  (let [[direction & rest-chars] line
        value (apply str rest-chars)]
    [direction value]))


;; return absolute value from line
;; R=Right means positive numbers
;; L=Left means negative numbers
(defn value-from-line [line]
  (let [[direction raw_value] (decompose-line line)]
    ;; I need to multiple by 1 when R rotation so
    ;; the string gets converted to a number
    (if (= direction "L") (* -1 raw_value) (* 1 raw_value))))


;; Count everytime the dial ends or passthrough the 0
;;
;; My approach is to pretend the dial starts at zero.
;;
;; When the offset is positive, it means adding the count to the offset since the pointer
;; has to first get to the current position, to then continue with the offset.
;;
;; For the negative offsets, I flip the dial. This means we flip the offset (-) and make
;; the position to be subtracted from 100. The modulus is important in this case to prevent
;; 100 - 0 to end up as 100.
;;
(defn rotate-dial [position {:keys [offset, count]}]
  (let [delta (if (pos? offset) (+ position offset) (- (mod (- 100 position) 100) offset))
        turns (quot delta 100)]
    (swap! count + turns)
    (mod (+ position offset) 100)))


(defn crack-the-code [filepath callback]
  (let [position_initial 50
        count (atom 0)
        position_cur (atom position_initial)]
    (numbers-from-file filepath
                       (fn [lines]
                         (doseq [line lines]
                           (swap! position_cur rotate-dial {:offset (value-from-line line) :count count}))
                         (callback @count)))))


(defn main []
  (crack-the-code INPUT_FILE
                  (fn [result]
                    (println "Result for day 1:" result))))


(defn testing []
  (crack-the-code TEST_FILE
                  (fn [result]
                    (if (= result 6)
                      (main)
                      (println "Test failed:" result "(expected 6)")))))



(testing)
