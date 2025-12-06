;; Damn the use of partition is craaazy :)
;; Great use of --> to avoid nested parenthesis
;; Specially in combination with map and apply +
(ns day4.ai-plus
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

;; ------------------------------------------------------------
;; Constants
;; ------------------------------------------------------------

(def paper-chr "@")
(def null-chr ".")
(def illegal-papers-around 4)

;; ------------------------------------------------------------
;; Padding
;; ------------------------------------------------------------

(defn create-extended-lines [lines]
  (let [w (count (first lines))
        pad (apply str (repeat (+ w 2) null-chr))]
    (concat
     [pad]
     (map #(str null-chr % null-chr) lines)
     [pad])))

;; ------------------------------------------------------------
;; Kernel extraction
;; ------------------------------------------------------------

(defn get-kernel [i p r n]
  ;; Build 3x3 around row r, column i
  (list
   ;; prev row
   (subs p (dec i) (+ i 2))
   ;; same row chars left & right
   (str (nth r (dec i)) (nth r (inc i)))
   ;; next row
   (subs n (dec i) (+ i 2))))

;; Flatten kernel -> sequence of chars
(defn kernel->chars [kernel]
  (apply concat (map seq kernel)))

;; ------------------------------------------------------------
;; Process one interior line
;; ------------------------------------------------------------

(defn process-line
  ([p r n] (process-line p r n false))
  ([p r n verbose?]
   (->> r
        (map-indexed vector)
        (drop 1)      ;; skip padded left
        (butlast)     ;; skip padded right
        (reduce
         (fn [acc [i chr]]
           (if (= chr paper-chr)
             (let [kernel (get-kernel i p r n)
                   chars  (kernel->chars kernel)
                   count @(count (filter #(= % paper-chr) chars))]
               (if (< count @illegal-papers-around)
                 (inc acc)
                 acc))
             acc))
         0))))

;; ------------------------------------------------------------
;; Whole-grid processing
;; ------------------------------------------------------------

(defn get-accessible-paper-rolls [lines]
  (->> lines
       (partition 3 1)
       (map (fn [[p r n]] (process-line p r n)))
       (apply +)))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------

(defn read-lines [filepath]
  (let [abs (path/join js/__dirname filepath)
        content (fs/readFileSync abs "utf8")]
    (str/split-lines content)))

(defn crack-the-code [filepath]
  (-> filepath
      read-lines
      create-extended-lines
      get-accessible-paper-rolls))

;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------

(deftest test-buffer-lines-creation
  (let [input ["abcde" "fghij"]
        expected ["......."
                  ".abcde."
                  ".fghij."
                  "......."]]
    (is (= expected (vec (create-extended-lines input))))))

(deftest test-get-kernel
  (let [grid ["abcde"
              "fghij"
              "klmno"]
        k1 (apply str (kernel->chars (get-kernel 1 (grid 0) (grid 1) (grid 2))))
        k2 (apply str (kernel->chars (get-kernel 2 (grid 0) (grid 1) (grid 2))))]
    (is (= "abcghi" k1))  ;; manually verified kernels
    (is (= "bcdhij" k2))))

(deftest test-free-paper-1
  (let [input ["..."
               ".@."
               "..."]]
    (is (= 1 (get-accessible-paper-rolls input)))))

(deftest test-free-paper-2
  (let [input ["....."
               ".@@@."
               ".@@@."
               ".@@@."
               "....."]]
    (is (= 4 (get-accessible-paper-rolls input)))))

;; Replace with your real file path:
(def test-file "../../inputs/day4-test.txt")

(deftest test-sample-data
  (is (= 13 (crack-the-code test-file))))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Day 4 result:" (crack-the-code "../../inputs/day4.txt")))

;; Run tests and only run main if all passed
(when (zero? (:fail (run-tests)))
  (main))
