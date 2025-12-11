(ns day6-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day6.txt")
(def test-file  "../../inputs/day6-test.txt")



(defn pp
  "Debug printing of seq
   
   To be used during -> or ->> code"
  [seq]
  ;;   (prn "XXX" seq)
  seq)

(defn get-column-len
  "Return the len of the operation column
   
   This will work for all but the last column
   "
  [op]
  (dec (count op)))


(defn get-numbers-from-column
  "return a seq with all the numbers for this column"
  [len string]
  ;;   (println "len:" len "string: " string)
  (first
   (reduce
    (fn [[acc string] idx]
      ;; we iterate over one number at a time
      ;; by using partition to get the digits from
      ;; that column
      (let [n (->>
               string
               (partition 1 len)
               (apply concat)
               (apply str)
               str/trim
               (parse-long)
               ;;    (pp)
               )]
        ;; use rest here, so that partition can now check
        ;; the next digit (partition has no offset unfortunatelly)
        [(conj acc n) (rest string)]))
    [[] string]
    (range len))))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")
        lines (str/split-lines content)
        numbers (->>
                 (pop lines)
                 (apply str))
        ops (->>
             lines
             peek
             ;; add an extra space for the final column
             ;; so `get-column-len` works since this column
             ;; is one chr short (it has no right-leading space)
             (#(str % " "))
             (re-seq #". +")
             (map #(if (= (first %) "+")
                     {:op + :len (get-column-len %)}
                     {:op * :len (get-column-len %)})))
        line-len (count (first lines))]
    (first
     (reduce
      (fn [[acc numbers-left] op]
        [(+ acc
            (->>
             numbers-left
             ;; get complete column string
             (partition (:len op) line-len)
             ;; make it a continuos string (for partition later)
             (apply concat)
             (apply str)
             (get-numbers-from-column (:len op))
             (reduce (:op op))))
         (subs
          numbers-left
          ;; one extra chr for the space between the columns
          (inc (:len op)))])
      [0 numbers]
      ops))))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 3263827 (crack-the-code test-file))))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 6:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
