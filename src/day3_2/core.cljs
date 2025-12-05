(ns day3-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day3.txt")
(def TEST_FILE "../../inputs/day3-test.txt")

(def MAX_DIGITS 2)

;; Find the highest digit from id onwards
;; Return [id digit]
(defn find-highest-digit [line]
  (let [[_ id digit]
        (reduce (fn [[id cur_id cur_max] letter]
                  (let [next_id (inc id)
                        number (js/parseInt letter)
                        new_max (max number cur_max)]
                    (if (= number 9)
                      (reduced [_ id 9])
                      [next_id (if (> new_max cur_max) id cur_id) new_max])))
                [0 0 -1]
                line)]
    ;; (println "Line:" line "digit:" digit "id:" id)
    [id digit]))


;; expects line to be already cut to make sure there
;; enough digits to the remaining numbers
;; the cutting is on the end, to preserve the overal index
(defn find-digit [sub_line position]
  (let [;; Ignore the last digit since we need to
        ;; leave one digit for the units
        subline (subs sub_line (inc position))
        [id digit]
        (find-highest-digit subline)]
    [id digit]))


;; find the biggest digit (or the first 9) up to the nth-to-last digit
;; then afterwards find the next biggest from that position onwards
;; until we have a total of MAX_DIGITS
(defn battery-from-line [line]
  ;; (println "Processing line:" line)
  (let [line_len (count line)]
    (first (reduce
            ;; id of: the element in the line array
            ;; parent_id: previous id added to the battery
            ;; battery: accumulated battery value
            (fn [[battery parent_id] id]
              (let [;; safe_line is the line that guarantees enough digits right of it
                    ;; for the yet required digits
                    skip_digits (- MAX_DIGITS id 1)
                    safe_line (subs line 0 (- line_len skip_digits))
                    [child_id, digit] (find-digit safe_line parent_id)
                    ;; incrementally shift the battery digits to their right pow 10 value
                    new_battery (+ digit (* 10 battery))]
                [new_battery child_id]))
            [0, -1, 0] (range MAX_DIGITS)))))


(defn crack-the-code [filepath]
  (let [abs_filepath (path/join js/__dirname filepath)
        content (fs/readFileSync abs_filepath "utf8")]
    (println "Reading sequence from file:" abs_filepath)
    (reduce (fn [count line]
              (let [new_count (+ count (battery-from-line line))]
                new_count))
            0
            (str/split-lines content))))


;; ---- CLI / Test ----
(defn main []
  (println "Result for day 3.2:" (crack-the-code INPUT_FILE)))

(defn testing []
  (let [result (crack-the-code TEST_FILE)
        expected 357];; 3121910778619
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))


(testing)
