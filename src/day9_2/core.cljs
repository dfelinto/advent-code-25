
(ns day9-2.core
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))


(def input-file "inputs/day9.txt")
(def test-file  "inputs/day9-test.txt")


(defn is [expected got] (if (= expected got) true (println "Error: expected:" expected ", got:" got)))


(defn parse-corners
  "1,2 -> {:x 1 :y 2}"
  [line] (->>
          (str/split line #",")
          (map parse-long)
          (zipmap [:x :y])))


(defn area [from to]
  ;;   (println from to)
  (let [width (inc (abs (- (:x from) (:x to))))
        height (inc (abs (- (:y from) (:y to))))
        square-area (* width height)]
    ;; (println square-area)
    square-area))


(defn calculate-areas [corners]
  (for [from-idx (range (count corners))
        to-idx (range (inc from-idx) (count corners))]
    (let [from (corners from-idx)
          to (corners to-idx)]
      {:area (area from to) :from from :to to})))


(defn xor [a? b?]
  (cond
    (and a? b?)
    false
    (or a? b?)
    true
    :else
    false))


(defn min-max [c1 c2]
  (let [min-x (min (:x c1) (:x c2))
        min-y (min (:y c1) (:y c2))
        max-x (max (:x c1) (:x c2))
        max-y (max (:y c1) (:y c2))]
    [min-x min-y max-x max-y]))


(def min-max-optimized (memoize min-max))


(defn is-outside?
  ([c1 c2 p] (is-outside? c1 c2 p false))
  ([c1 c2 p verbose?]
   (let [[min-x  min-y  max-x  max-y] (min-max-optimized c1 c2)
         x (:x p)
         y (:y p)
         c1' {:x (:x c1) :y (:y c2)}
         c2' {:x (:x c2) :y (:y c1)}]
     (when verbose? (println c1 c2 "p" p))
     (or
      ;; treat the corners as if they were outside
      (= p c1)
      (= p c2)
      (= p c1')
      (= p c2')
      (< x min-x)
      (> x max-x)
      (< y min-y)
      (> y max-y)))))

(def is-inside? (complement is-outside?))

(defn perimeter-segments [perimeter]
  (partition 2 1 (conj perimeter (first perimeter))))


(def perimeter-segments-cached (memoize perimeter-segments))


(defn vertical? [[p1 p2]] (= (:x p1) (:x p2)))


(defn intersect? [[seg1 seg2]]
  (let [[p1 p2] seg1
        [p3 p4] seg2
        x-min (apply min (mapv :x [p1 p2]))
        x-max (apply max (mapv :x [p1 p2]))
        x (:x p3)
        y-min (apply min (mapv :y [p3 p4]))
        y-max (apply max (mapv :y [p3 p4]))
        y (:y p1)]
    (cond
      ;; Parallel lines don't intersect
      (= (vertical? seg1) (vertical? seg2))
      false

      (and (> x x-min) (< x x-max)
           (> y y-min) (< y y-max))
      true

      :else
      false)))


(defn segments [c1 c3]
  (let [c2 {:x (:x c3) :y (:y c1)}
        c4 {:x (:x c1) :y (:y c3)}]
    (partition 2 1 [c1 c2 c3 c4 c1])))


(defn is-valid-connection?
  "The connection is valid if no points from the perimeter are inside the rectangle"
  [perimeter areas]
  (let [c1 (:from areas)
        c2 (:to areas)
        intersection-pairs (for [x (segments c1 c2)
                                 y (perimeter-segments-cached perimeter)] [x y])]
    (cond
      ;; If any point is inside we already know it is invalid
      (some #(is-inside? c1 c2 % false) perimeter)
      false

      ;; Check for any segment intersecting the rectangle.
      (some #(intersect? %) intersection-pairs)
      false

      :else
      true)))


(defn get-chr [point perimeter]
  (cond
    (first (filter #(= point %) perimeter))
    (str (.indexOf perimeter point))
    ;; "#"

    ;; (is-outside? {:x 9 :y 7} {:x 2 :y 3} point)
    ;; "."

    :else
    "."))


(defn pp-debug [perimeter]
  (let [width (+ 2 ;; pad
                 (inc (apply max (mapv :x perimeter))))
        height (+ 1 ;; pad
                  (inc (apply max (mapv :y perimeter))))
        grid (for [y (range height)
                   x (range width)]
               (str
                (get-chr {:x x :y y} perimeter)
                (when (= x (dec width)) "\n")))]
    (println width height)
    (println (apply str grid))))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (let [corners (->> lines
                      (map parse-corners)
                      vec)
         ;;  perimeter (connect-corners corners)
         perimeter corners ;; as it turned out, the perimeter and corners are the same
         areas (calculate-areas corners)]
     ;;  (is corners (connect-corners corners))
     (when verbose? (pp-debug perimeter))
     ;;  (is (count corners) (count perimeter))
     (->>
      areas
      (filter (partial is-valid-connection? perimeter))
      (mapv :area)
      (apply max)))))


;; ------------------------------------------------------------
;; File processing
;; ------------------------------------------------------------
(defn input
  [filepath]
  (println "Reading sequence from file:" filepath)
  (->>
   filepath
   (fs/absolutize)
   str
   slurp
   (str/split-lines)))


;; ------------------------------------------------------------
;; Scenario Test
;; ------------------------------------------------------------

(defn test-sample-data []
  (is 24 (crack-the-code (input test-file) true)))

(test-sample-data)

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(defn main []
  (time (println "Result for day 9.2:" (crack-the-code (input input-file) false))))


;; There is no way to process the output of (run-tests) to know if it fails.
;; so we keep (main) manually commented out until all tests pass
(main)
