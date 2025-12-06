;; AI Pass
;;
;; * Good to know about `repeat`
;; * str definitively better than concat
;; * Nice `(count (filter #(= % paper-chr) kernel))]` line too
(ns day4.ai
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

(def input-file "../../inputs/day4.txt")
(def test-file  "../../inputs/day4-test.txt")

(def paper-chr "@")
(def null-chr ".")
(def illegal-papers-around 4)

;; ------------------------------------------------------------
;; Grid padding
;; ------------------------------------------------------------

(defn create-extended-lines [lines]
  (let [w (count (first lines))
        h (count lines)
        border (apply str (repeat (+ w 2) null-chr))]
    (map (fn [y]
           (cond
             (= y -1) border
             (= y h)  border
             :else    (str null-chr (nth lines y) null-chr)))
         (range -1 (inc h)))))

;; ------------------------------------------------------------
;; Kernel extraction
;; ------------------------------------------------------------

(defn get-kernel [i prev curr next]
  ;; return a STRING, not a seq of chars
  (str
   (subs prev (dec i) (+ i 2))
   (nth curr (dec i))
   (nth curr (inc i))
   (subs next (dec i) (+ i 2))))

;; ------------------------------------------------------------
;; Per-line processing
;; ------------------------------------------------------------

(defn process-line [p c n]
  (:acc
   (reduce
    (fn [{:keys [id acc]} ch]
      (if (= ch paper-chr)
        (let [kernel (get-kernel id p c n)
              num    (count (filter #(= % paper-chr) kernel))]
          (if (< num illegal-papers-around)
            {:id (inc id) :acc (inc acc)}
            {:id (inc id) :acc acc}))
        {:id (inc id) :acc acc}))
    {:id 1 :acc 0}
    ;; drop padded first/last chars
    (drop 1 (butlast c)))))

;; ------------------------------------------------------------
;; Entire grid scanning
;; ------------------------------------------------------------

(defn get-accessible-paper-rolls
  ([lines] (get-accessible-paper-rolls lines false))
  ([lines verbose?]
   (:papers
    (reduce
     (fn [{:keys [row1 row2 papers]} row3]
       {:row1 row2
        :row2 row3
        :papers (+ papers (process-line row1 row2 row3 verbose?))})
     {:row1 (first lines)
      :row2 (second lines)
      :papers 0}
     (drop 2 lines)))))

;; ------------------------------------------------------------
;; File reading
;; ------------------------------------------------------------

(defn crack-the-code [filepath]
  (let [abs (path/join js/__dirname filepath)
        content (fs/readFileSync abs "utf8")
        lines   (str/split-lines content)
        padded  (create-extended-lines lines)]
    (get-accessible-paper-rolls padded)))

;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------

(deftest test-buffer-lines-creation
  (let [input ["abcde" "fghijkl"]
        expected ["......."     ;; correct
                  ".abcde."
                  ".fghijkl."
                  "......."]]
    (is (= expected (vec (create-extended-lines input))))))

(deftest test-get-kernel
  (let [input ["abcde"
               "fghij"
               "klmno"]]
    (is (= "abcfhklm" (get-kernel 1 (first input) (second input) (nth input 2))))
    (is (= "bcdgilmn" (get-kernel 2 (first input) (second input) (nth input 2))))))

(deftest test-free-papers-1
  (let [input ["..."
               ".@."
               "..."]]
    (is (= 1 (get-accessible-paper-rolls input)))))

(deftest test-free-papers-2
  (let [input ["....."
               ".@@@."
               ".@@@."
               ".@@@."
               "....."]]
    (is (= 4 (get-accessible-paper-rolls input)))))

(deftest test-sample-data
  (is (= 13 (crack-the-code test-file))))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 4:" (crack-the-code input-file)))

(run-tests)
;; manually comment out main while debugging
(main)
