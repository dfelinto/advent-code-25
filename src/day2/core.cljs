(ns day2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(println "Day 2 baby! Santa here I come")

(def INPUT_FILE "../../inputs/day2.txt")
(def TEST_FILE "../../inputs/day2-test.txt")


;; return true if number is like 4242 1010 11 111333 145145
(defn is-symmetrical? [number]
  (let [half_char_count (/ (count number) 2)
        begin (subs number 0 half_char_count)
        end (subs number half_char_count)
        match (= begin end)]
    match))


;; ;; return true if number is like 4242 1010 11 111333 145145
;; altenative with more math :)
;; (defn is-symmetrical? [str_number]
;;   (let [half_char_count (/ (count str_number) 2)
;;         number (js/parseInt str_number)
;;         multiplier (Math/pow 10 half_char_count)
;;         begin (quot number multiplier)
;;         end (mod number multiplier)
;;         match (= begin end)]
;;     match))


;; return true if number pass santa's special criteria
(defn is-special-number? [number]
  (let [str_number (str number)
        char_count (count str_number)]
    (if (odd? char_count)
      false
      (is-symmetrical? str_number))))


(defn get-special-numbers [sequence]
  (let [limits (str/split sequence #"-")]
    (reduce (fn [count number]
              (let [new-count (+ count (if (is-special-number? number) number 0))]
                new-count))
            0
            (range (js/parseInt (first limits)) (inc (js/parseInt (second limits)))))))


(defn crack-the-code [filepath]
  (let [abs_filepath (path/join js/__dirname filepath)
        content (fs/readFileSync abs_filepath "utf8")]
    (println "Reading sequence from file:" abs_filepath)
    (reduce (fn [count sequence]
              (let [new-count (+ count (get-special-numbers sequence))]
                new-count))
            0
            (str/split content #","))))


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
