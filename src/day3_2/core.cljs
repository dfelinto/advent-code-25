(ns day3-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day3.txt")
(def TEST_FILE "../../inputs/day3-test.txt")



(defn find-highest-digit [line]
  (let [[_ offset tens]
        (reduce (fn [[id cur_id cur_max] letter]
                  (let [next_id (inc id)
                        number (js/parseInt letter)
                        new_max (max number cur_max)]
                    (if (= number 9)
                      (reduced [_ id 9])
                      [next_id (if (> new_max cur_max) id cur_id) new_max])))
                [0 0 -1]
                line)]
    ;; (println "Line:" line "digit:" tens "offset:" offset)
    [offset tens]))


(defn find-tens [line]
  (let [;; Ignore the last digit since we need to
        ;; leave one digit for the units
        subline (subs line 0 (dec (count line)))]
    (find-highest-digit subline)))


(defn find-units [line position]
  ;; get the line from the tens onward
  (let [subline (subs line (inc position))
        [_ units] (find-highest-digit subline)]
    units))


;; find the biggest digit (or the first 9) up to the second-to-last digit
;; then afterwards find the second biggest from that position onwards
;; compose the number with math (for a second I was going to combine the
;; str and parseInt :P but it just occured to me I can multiple the
;; first digit by 10 ... BRILLIANT haha)
(defn battery-from-line [line]
  ;; (println "Processing line:" line)
  (let [[id tens] (find-tens line)
        units (find-units line id)
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
        expected 3121910778619]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))


(testing)
