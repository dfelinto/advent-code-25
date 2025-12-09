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
  (let [[_ begin-raw end-raw] (re-matches #"([0-9]*)\-([0-9]*)"  line),
        begin (parse-long begin-raw)
        end (parse-long end-raw)]
    {:begin (min begin end)
     :end   (max begin end)}))


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


(defn count-fresh-fruits
  ([ranges fruits] (count-fresh-fruits ranges fruits false))
  ([ranges fruits verbose?]
   (when verbose? (println "ranges" ranges "\n" "fruits" fruits "\n"))
   (:acc (reduce (fn [{:keys [acc ranges-iter]} fruit]
                   (when verbose?
                     (println "fruit" fruit "acc" acc)
                     ;;  (println "ranges left" ranges-iter "\n")
                     )
                   (loop [ranges-loop ranges-iter]
                     (cond
                       (empty? ranges-loop)
                       ;; spoiled fruit - and all the ones afterwards
                       {:acc acc :ranges-iter []}

                       (< fruit (:begin (first ranges-loop)))
                       ;; spoiled fruit
                       {:acc acc :ranges-iter ranges-loop}

                       (<= fruit (:end (first ranges-loop)))
                       ;; fresh fruit
                       {:acc (inc acc) :ranges-iter ranges-loop}

                       :else
                       ;; we don't know, need to check next range
                       (recur (rest ranges-loop)))))
                 {:acc 0
                  :ranges-iter ranges}
                 fruits))))

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (println "Reading sequence from file:" filepath)
  (let [abs-path (path/join js/__dirname filepath)
        _ (println "Reading sequence from file:" abs-path)
        content (fs/readFileSync abs-path "utf8")
        {ranges-raw :ranges
         numbers-raw :numbers} (select-keys (process-content content) [:ranges :numbers])
        _ (println "Content processed")
        numbers (sort numbers-raw)
        _ (println "Numbers sorted")
        ranges (->>
                ranges-raw
                (sort-by :begin)
                merge-ranges)
        ;; _ (println "Ranges merged" ranges)
        ;; _ (println "Numbers" numbers)
        fresh-fruits (count-fresh-fruits ranges numbers)]
    fresh-fruits))


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


(deftest test-count-fruits []
  ;; since this test is already sorted
  ;; we can go straight to count-fruits
  (let [ranges [{:begin 1 :end 29}
                {:begin 50 :end 120}]
        numbers [2 4 51 122 450]]
    (is (= 3 (count-fresh-fruits ranges numbers)))))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(deftest test-sample-data []
  (is (= 3 (crack-the-code test-file))))


;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (println "Result for day 5:" (crack-the-code input-file)))

(enable-console-print!)
(run-tests)

;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
