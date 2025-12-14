(ns day7-2.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day7.txt")
(def test-file  "inputs/day7-test.txt")
(def output-file "output.csv")


(defn is [expected got]
  (if (= expected got)
    true
    (println "Error: expected:" expected ", got:" got)))


(defn is-splitter? [idx line prev-line] (if (= '\^ (nth line idx)) 0 (nth prev-line idx)))

(defn is-left-timeline? [idx line prev-line] (if (= '\^ (get line (inc idx))) (nth prev-line (inc idx)) 0))

(defn is-right-timeline? [idx line prev-line] (if (= '\^ (get line (dec idx))) (nth prev-line (dec idx)) 0))


(defn process-line
  ([line prev-line next-lines] (process-line line prev-line next-lines false))
  ([line prev-line next-lines verbose?]
   (let [[next-line & lines] next-lines
         current-line
         (map (fn [idx]
                (reduce (fn [acc fnx]
                          (+ acc (fnx idx line prev-line)))
                        0
                        [is-splitter? is-left-timeline? is-right-timeline?]))
              (range (count line)))]
     (if next-line
       (recur next-line current-line lines verbose?)
       (reduce + current-line)))))


(defn process-tree
  ([lines] (process-tree lines false))
  ([lines verbose?]
   (let [[line & lines] lines
         first-line (map #(if (= % '\S) 1 0) line)
         [second-line & lines] lines]
     ;; Start processing the second line onwards
     (process-line second-line first-line lines true))))


;; ------------------------------------------------------------
;; Naïve solution - this works well until around 50 lines of data
;; Check graphs/day7.jpg to understand why it fails afterwards
;; ------------------------------------------------------------

(defn process-line-naive
  ([ray-idx line next-lines] (process-line-naive ray-idx line next-lines false))
  ([ray-idx line next-lines verbose?]
   ;;  (when verbose? (ptree ray-idx line))
   (let [[next-line & lines] next-lines]
     (cond
       ;; Timeline ends here
       (nil? next-line)
       1

       (= (nth line ray-idx) '\^)
       (+ (process-line-naive (dec ray-idx) next-line lines false)
          (process-line-naive (inc ray-idx) next-line lines verbose?))

       :else
       (process-line-naive ray-idx next-line lines verbose?)))))


(defn process-tree-naive
  ([lines] (process-tree-naive lines false))
  ([lines verbose?]
   (let [[first-line & lines] lines
         ray-idx (.indexOf first-line '\S)
         [second-line & lines] lines]
     (process-line-naive ray-idx second-line lines verbose?))))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code
  ([filepath] (crack-the-code filepath false))
  ([filepath verbose?]
   (println "Reading sequence from file:" filepath)
   (->>
    filepath
    (fs/absolutize)
    str
    slurp
    (str/split-lines)
    (process-tree))))


(defn output-all-solutions
  "Output a csv file with the solution for each possible path"
  ([filepath] (crack-the-code filepath false))
  ([filepath verbose?]
   (println "Reading sequence from file:" filepath)
   (let [lines (->>
                filepath
                (fs/absolutize)
                str
                slurp
                (str/split-lines))
         results
         (reduce
          (fn [acc idx]
            (conj acc [idx (process-tree (take idx lines))]))
          []
          (range (count lines)))
         ;; (range 5))
         content (->>
                  results
                  (map #(str/join ";" %))
                  (str/join "\n"))]
     (spit output-file content :append false)
     (println "Results outputted to:" output-file))))

;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(defn test-sample-data []
  (is 40 (crack-the-code test-file true)))


(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 7.2:" (crack-the-code input-file true))))

;; (enable-console-print!)
;; (run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
