(ns day3-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day3.txt")
(def TEST_FILE "../../inputs/day3-test.txt")


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
    (println "Line:" line "digit:" digit "id:" id)
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

;; find the biggest digit (or the first 9) up to the second-to-last digit
;; then afterwards find the second biggest from that position onwards
;; compose the number with math (for a second I was going to combine the
;; str and parseInt :P but it just occured to me I can multiple the
;; first digit by 10 ... BRILLIANT haha)
(defn battery-from-line [line]
  (println "Processing line:" line)
  (let [subline (subs line 0 (-> line (count) (dec)))
        [id tens] (find-digit subline -1)
        units (second (find-digit line id))
        ;; (* 10 what? *.* )
        battery (+ tens tens tens tens tens tens tens tens tens tens units)]
    battery))


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
