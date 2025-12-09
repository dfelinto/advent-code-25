(ns day5.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cljs.test :refer-macros [deftest is run-tests]]))

(def input-file "../../inputs/day5.txt")
(def test-file  "../../inputs/day5-test.txt")


;; Assume the ranges are already merged
(defn merge-ranges
  ([ranges] (merge-ranges ranges false))
  ([ranges verbose?]
   (:merged-ranges
    (reduce (fn [{:keys [min-prev
                         max-prev
                         merged-ranges]} range]
              (when verbose?
                (println "range" range
                         "merged-ranges" merged-ranges))
              (let [min-cur (:begin range)
                    max-cur (:end range)
                    min-new (min min-cur min-prev)
                    max-new (max max-cur max-prev)]
                (when verbose?
                  (println "min-cur" min-cur
                           "max-cur" max-cur
                           "min-new" min-new
                           "max-new" max-new
                           "\n"))
                (if (> min-cur (inc max-prev))
                  ;; don't merge, just add the new range
                  {:min-prev      min-cur
                   :max-prev      max-cur
                   :merged-ranges (conj merged-ranges range)}
                  ;; else means the range starts within the previous range
                  ;; so we remove the previously added range, and add
                  ;; a new one which contains both
                  {:min-prev      min-new
                   :max-prev      max-new
                   :merged-ranges (->
                                   merged-ranges
                                   pop
                                   (conj {:begin min-new :end max-new}))})))
            {:min-prev      -1
             :max-prev      -1
             :merged-ranges []}
            ranges))))


(defn range-from-line [line]
  (let [[_ begin end] (re-matches #"([0-9]*)\-([0-9]*)"  line)]
    {:begin (parse-long begin)
     :end   (parse-long end)}))


(defn process-content [content]
  (select-keys
   (reduce (fn [{:keys [ranges numbers is-number]} line]
             (cond
               (empty? line)
               {:ranges    ranges
                :numbers   numbers
                :is-number true}

               (not is-number)
               {:ranges    (conj ranges (range-from-line line))
                :numbers   numbers
                :is-number false}

               :else
               {:ranges    ranges
                :numbers   (conj numbers (parse-long line))
                :is-number true}))
           {:ranges []
            :numbers []
            :is-number false}
           (str/split-lines content))
   [:ranges :numbers]))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")
        [ranges numbers] (process-content content)]
    (println "ranges" ranges "numbers" numbers)
    10));; TODO


;; ------------------------------------------------------------
;; Unit Tests
;; ------------------------------------------------------------

(deftest test-range-from-file []
  (is (= {:begin 34 :end 199} (range-from-line "34-199"))))


(defn list-to-lines [items]
  (apply str (map #(str % "\n") items)))


(deftest test-process-content []
  (let [input ["1-29"
               "50-120"
               ""
               "2"
               "4"]
        expected {:ranges
                  [{:begin 1 :end 29}
                   {:begin 50 :end 120}]
                  :numbers
                  [2 4]}]
    (is (= expected (->
                     input
                     (list-to-lines)
                     (process-content))))))


(deftest test-merge-ranges []
  (let [input ["1-50" "25-30" "35-60" "65-70"]
        expected ["1-60" "65-70"]
        input-ranges (map range-from-line input)
        expected-ranges (map range-from-line expected)]
    (is (= expected-ranges (merge-ranges input-ranges)))))



;; (test-merge-ranges)
;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

;; (deftest test-sample-data
;;   (is (= 3 (crack-the-code test-file))))

;; (test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 5:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
;; (main)
