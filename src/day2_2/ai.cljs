(ns day2-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

;; ---- File paths ----
(def INPUT_FILE "../../inputs/day2.txt")
(def TEST_FILE "../../inputs/day2-test.txt")
(def DEBUG_FILE "../../inputs/day2-test-debug.txt")

;; ---- Core Functions ----

;; Check if the number can be split into `parts` equal substrings
(defn is-repeated? [{:keys [number parts]}]
  (let [s          (str number)
        len        (quot (count s) parts)
        first-part (subs s 0 len)]
    (every? #(= first-part (subs s (* % len) (* (inc %) len)))
            (range parts))))

;; Check if a number is "special" (can be evenly split into repeating parts)
(defn is-special-number? [number]
  (let [digits-count (count (str number))]
    (some true?                                 ;; stop at first true
          (for [parts (range 2 (inc digits-count))
                :let [divisible? (= 0 (mod digits-count parts))]
                :when divisible?]
            (is-repeated? {:number number :parts parts})))))

;; Process a range string like "100-200" and sum special numbers
(defn get-special-numbers [sequence]
  (let [[start-str end-str] (str/split sequence #"-")
        start (js/parseInt start-str)
        end   (js/parseInt end-str)]
    (reduce (fn [acc n]
              (if (is-special-number? n)
                (+ acc n)   ;; sum numbers; replace with 1 to count
                acc))
            0
            (range start (inc end)))))

;; Main function: read file, split by commas, sum all special numbers
(defn crack-the-code [filepath]
  (let [abs-file (path/join js/__dirname filepath)
        content  (fs/readFileSync abs-file "utf8")]
    (println "Reading sequence from file:" abs-file)
    (reduce (fn [total seq-str]
              (+ total (get-special-numbers seq-str)))
            0
            (str/split content #","))))

;; ---- CLI / Testing ----

(defn main []
  (println "Result for day 2.2:" (crack-the-code INPUT_FILE)))

(defn testing []
  (let [result   (crack-the-code TEST_FILE)
        expected 4174379265]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))

(defn debug []
  (let [result   (crack-the-code DEBUG_FILE)
        expected 123123]
    (if (= result expected)
      (testing)
      (println (str "Debug failed: " result " (expected " expected ")")))))

;; Entry point
(debug)
