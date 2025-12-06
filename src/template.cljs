(ns dayXXX_DAY.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def input-file "../../inputs/dayXXX_DAY.txt")
(def test-file  "../../inputs/dayXXX_DAY-test.txt")

;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn crack-the-code [filepath]
  (let [abs-path (path/join js/__dirname filepath)
        content  (fs/readFileSync abs-path "utf8")]
    (println "Reading sequence from file:" abs-path)
    888888))


;; ------------------------------------------------------------
;; CLI / Tests
;; ------------------------------------------------------------
(defn main []
  (println "Result for day XXX_DAY:" (crack-the-code input-file)))

(defn testing []
  (let [result   (crack-the-code test-file)
        expected 99999]
    (if (= result expected)
      (main)
      (println (str "Test failed: " result " (expected " expected ")")))))


(testing)
