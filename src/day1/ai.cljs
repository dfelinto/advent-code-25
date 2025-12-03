;; ChatGPT pass to increase idiomatic.

;; Small note:
;; Although most of it is valid I decided to use async
;; intentionally, to ramp the complexity a bit.
;; IIRC I ended up learning and using atom just because of that.
;;
;; The (* 1 value) to parse to a number I was sure there
;; was a proper way, but I was happy with the symmetry
;; with the (* -1) solution. Now that I think of it I could
;; double-down and use: `(* (if (= direction "L") -1 1) value)`!
;;
;; Interesting use of reduce and second. TIL
;;
;; 1. Removed atoms and callbacks
;;    - Makes crack-the-code pure and easier to test.
;;    - No need for mutable state with swap!.
;;
;; 2. Synchronous file read for CLI
;;    - fs/readFileSync simplifies testing logic.
;;
;; 3. Functional fold (reduce)
;;    - Tracks [position count] pair without mutation.
;;    - Cleaner and easier to reason about.
;;
;; 4. Maps for line decomposition
;;    - {:direction "L" :value 42} is clearer than [dir val].
;;
;; 5. Inline number parsing
;;    - Explicit js/parseInt avoids multiplying by 1.
;;
;; 6. Separated pure logic from side effects
;;    - crack-the-code does only computation.
;;    - main and testing handle printing.


(ns day1.core
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(def INPUT_FILE "../../inputs/day1.txt")
(def TEST_FILE "../../inputs/day1-test.txt")

;; ---- File reading ----
(defn numbers-from-file [filepath]
  (let [abs-file (path/join js/__dirname filepath)
        content (fs/readFileSync abs-file "utf8")] ;; sync for CLI
    (str/split-lines content)))

;; ---- Line parsing ----
(defn decompose-line [line]
  {:direction (subs line 0 1)
   :value (js/parseInt (subs line 1))}) ;; parse string to number

(defn value-from-line [{:keys [direction value]}]
  (if (= direction "L")
    (* -1 value)
    value)) ;; right is positive

;; ---- Dial rotation ----
(defn rotate-dial [position offset]
  (mod (+ position offset) 100))

;; ---- Core computation ----
(defn crack-the-code [filepath]
  (let [lines (numbers-from-file filepath)]
    (second ;; return the count
     (reduce (fn [[position count] line]
               (let [offset (value-from-line (decompose-line line))
                     new-pos (rotate-dial position offset)
                     new-count (if (= new-pos 0) (inc count) count)]
                 [new-pos new-count]))
             [50 0] ;; initial position and count
             lines))))

;; ---- CLI / Test ----
(defn main []
  (println "Result for day 1:" (crack-the-code INPUT_FILE)))

(defn testing []
  (let [result (crack-the-code TEST_FILE)]
    (if (= result 3)
      (main)
      (println "Test failed:" result "(expected 3)"))))

(testing)
