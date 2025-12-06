;; AI (chatGPT) unittest
(ns day3-2.core-test
  (:require [cljs.test :refer-macros [deftest is run-tests]]
            [day3-2.core :as core]))

(deftest test-find-highest-digit
  (is (= [0 9] (core/find-highest-digit "987")))
  (is (= [2 7] (core/find-highest-digit "1237")))
  (is (= [0 5] (core/find-highest-digit "5"))))

(deftest test-find-digit
  (is (= [1 9] (core/find-digit "987" -1)))
  (is (= [2 7] (core/find-digit "1237" 0))))

(deftest test-battery-from-line
  (let [line "987654321111111"
        battery (core/battery-from-line line)]
    (is (integer? battery))
    (is (>= battery 0))))

(deftest test-crack-the-code
  (let [test-file "../../inputs/day3-test.txt"
        result (core/crack-the-code test-file)
        expected 3121910778619]
    (is (= result expected))))

;; Run tests
(run-tests)
