(ns day7-2.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))


(def input-file "../../inputs/day7.txt")
(def test-file  "../../inputs/day7-test.txt")

(defn new-rays [new-line old-line]
  (letfn [(line-total [line]
            (count (filter true? line)))]
    (- (line-total new-line) (line-total old-line))))

(defn line-rays
  "Return a seq of bools for each ray in a cell"
  [line prev-line]
  (let [line-splitters (map #(= % '\^) line)
        line-len (count line)]
    (cond
      (zero? (count (filter true? line-splitters)))
      prev-line
      :else
      (map #(cond
              (nth line-splitters %)
              false
              (nth prev-line %)
              true
              (zero? %)
              (nth line-splitters (inc %))
              (= (dec line-len) %)
              (nth line-splitters (dec %))
              :else
              (or (nth line-splitters (dec %))
                  (nth line-splitters (inc %))))
           (range line-len)))))


(defn ptree
  "Debug function to print the current line with rays and splitters"
  [line new-line active-splitters]
  (println "  "
           (apply str
                  (map #(cond
                          (true? %2)
                          '\^
                          (true? %1)
                          '\|
                          (= '\^ %3)
                          '\^
                          :else
                          '\.) new-line active-splitters line))))


(defn process-line
  ([line prev-line next-lines] (process-line line prev-line next-lines false))
  ([line prev-line next-lines verbose?]
   (let [active-splitters (map #(and %1 (= %2 '\^)) prev-line line)
         line-range (range (count prev-line))]
     ;;  (when verbose? (println "->" (apply str (map #(if % '\| " ") prev-line))))
     ;;  (when verbose? (println "->" line))
     ;;  (when verbose? (println "->" (apply str (map #(if (= % '\^) '\. " ") active-splitters))))
     (when verbose? (ptree line prev-line active-splitters))
     (reduce
      +
      (map-indexed
       (fn [idx is-splitter?]
         ;;  (println idx is-splitter?)
         (let [[next-line & lines] next-lines
               line-a (map #(= % (dec idx)) line-range)
               line-b (map #(= % (inc idx)) line-range)
               is-ray? (true? (nth prev-line idx))
               is-nada? (not is-ray?)]
           ;;  (when verbose? (println idx ":" (if is-ray? "r :" "  :") (if is-splitter? "^ :" "  :") (if is-nada? "-" " ")))
           (cond
             ;; Timeline ends here
             (nil? next-line)
             (if is-ray? 1 0)

             is-splitter?
             (+ (process-line next-line line-a lines verbose?)
                (process-line next-line line-b lines))

             ;; no splitter, no ray, just ignores this cell
             is-nada?
             0

             ;; active ray, continue to the next line with a single ray
             ;; pass the current line as the previous line since the
             ;; only relevant info is this current ray, and there is only one
             ;; ray per line any-ways, thanks to timeline split
             is-ray?
             ;;  (let [_ (when verbose? (println "|" idx))]
             (process-line next-line (map #(= % idx) line-range) lines verbose?)
             ;; if we were inside a reduce (instead of a map) we could end here

             :else
             0)))
       active-splitters)))))


(defn process-tree
  ([lines] (process-tree lines false))
  ([lines verbose?]
   (let [[first-line & lines] lines
         first-line (map #(= % '\S) first-line)
         [second-line & lines] lines]
     (process-line second-line first-line lines verbose?))))
;;  (:acc
;;   (reduce (fn [{:keys [acc prev-line]} line]
;;             (let [new-line (line-rays line prev-line)
;;                   active-splitters (->>
;;                                     line
;;                                     (map #(and %1 (= %2 '\^)) prev-line))
;;                   active-splitters-count (->>
;;                                           active-splitters
;;                                           (filter true?)
;;                                           count)
;;                   new-acc (+ acc active-splitters-count)]
;;               (when verbose? (ptree line new-line active-splitters))
;;               {:acc new-acc :prev-line new-line}))
;;           {:acc 0 :prev-line first-line}
;;           (rest lines))))))


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code
  ([filepath] (crack-the-code filepath false))
  ([filepath verbose?]
   (println "Reading sequence from file:" filepath)
   (let [abs-path (path/join js/__dirname filepath)
         ;; _ (println "Reading sequence from file:" abs-path)
         content (fs/readFileSync abs-path "utf8")
         lines (str/split-lines content)]
     (process-tree lines verbose?))))


;; ------------------------------------------------------------
;; Unittests
;; ------------------------------------------------------------

(deftest test-tree-1 []
  (let [input
        ["....S...."
         "........."
         "........."
         "........."
         "........."]]
    (is (= 1 (process-tree input)))))


(deftest test-tree-2 []
  (let [input
        ["....S...."
         "........."
         ".^.....^."
         "........."
         "........."]]
    (is (= 1 (process-tree input)))))


(deftest test-tree-3 []
  (let [input
        ["....S...."
         "........."
         "....^...."
         "........."
         "........."]]
    (is (= 2 (process-tree input)))))


(deftest test-tree-4 []
  (let [input
        ["....S...."
         "....^...."
         "...^....."
         "........."
         "........."]]
    (is (= 3 (process-tree input)))))


(deftest test-tree-5 []
  (let [input
        ["....S...."
         "....^...."
         "...^.^..."
         "........."
         "........."]]
    (is (= 4 (process-tree input)))))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 40 (crack-the-code test-file true))))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 7.2:" (crack-the-code input-file true)))

(enable-console-print!)
;; (run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
