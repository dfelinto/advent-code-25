(ns day2-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day2.txt")
(def TEST_FILE "../../inputs/day2-test.txt")
(def DEBUG_FILE "../../inputs/day2-test-debug.txt")


;; return true if number is like 4242 1010 11 111333 145145
(defn is-symmetrical? [number]
  (let [half_char_count (/ (count number) 2)
        begin (subs number 0 half_char_count)
        end (subs number half_char_count)
        match (= begin end)]
    match))


;; return true if number pass santa's special criteria
;; larger number: 4444553231
;; #chars 10
;; 1 * 2 * 5
;; 22 22 22 22 22
;; 55555 55555
;; 1 1 1 1 1 1 1 1 1 1
;;
;; #chars 9
;; 3 * 3
;; 333 333 333
;; 1 1 1 1 1 1 1 1 1
;;
;; #chars 8
;; 2 * 4
;; 4444 4444
;; 22 22 22 22
;; 1 1 1 1 1 1 1 1
;;
;; #chars 7
;; 1 1 1 1 1 1 1
;; #chars 6
;; #chars 5
;; #chars 4
;; #chars 3
;; #chars 2
;; 1 1
;;
;; #chars 1

;; check if the equal parts of the number are equal
(defn is_repeated? [{:keys [number parts]}]
  (let [number_str (str number)
        number_count (count number_str)
        len (/ number_count parts)]
    ;; (println "is_repeated? number:" number "parts:" parts "len:" len)
    (second (reduce (fn [[begin _] id]
                      (let [offset (* id len)
                            part (subs number_str offset (+ offset len))
                            match? (= begin part)]
                        ;; (println "begin:" begin "part:" part "number:" number "match:" match?)
                        ;; if matches, continue, otherwise stops
                        (if match? [begin true] (reduced [begin false]))))
                    [(subs number_str 0 len) false]
                    (range 1 parts)))))


(defn is-special-number? [number]
  (let [count_number (count (str number))]
    (reduce (fn [_ divider]
              ;; check if the number can be divided in equal parts of "divider"
              (let [is_divisible? (= 0 (mod count_number divider))]
                (when (and is_divisible? (is_repeated? {:number number :parts divider}))
                  ;; stop reduce earlier returning true
                  (reduced true))))
            false
            ;; iteracts over the potential dividers for this amount of digits
            ;; starts at 2 since we need at least 2 repetitions
            (range 2 (inc count_number)))))


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
  (println "Result for day 2.2:" (crack-the-code INPUT_FILE)))

(defn testing []
  (let [result (crack-the-code TEST_FILE)
        expected 4174379265]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))



(defn debug []
  (let [result (crack-the-code DEBUG_FILE)
        expected 123123]
    (if (= result expected)
      (testing)
      (println (str "Debug failed: " result " (expected " expected ")")))))

;; (testing)

(debug)
