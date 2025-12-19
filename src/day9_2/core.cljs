
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
  (partition 2 1 perimeter))
;; (partition 2 1 (conj perimeter (first perimeter))))


(def perimeter-segments-cached (memoize perimeter-segments))


(defn vertical? [[p1 p2]] (= (:x p1) (:x p2)))


(defn op' [min-max key seg]
  (apply min-max (mapv key seg)))

(def op (memoize op'))

(defn intersect? [[seg1 seg2]]
  (cond
    ;; Parallel lines don't intersect
    (= (vertical? seg1) (vertical? seg2))
    true

    ;; Check if both points of seg2 are outside
    ;; seg1 points, but within the vertical range
    (vertical? seg1)
    (and
     ;; check if they are outside in opposite sides
     (< (op min :x seg2) (op min :x seg1))
     (> (op max :x seg2) (op max :x seg1))
     ;; check if they are within range
     (> (op min :y seg2) (op min :y seg1))
     (< (op max :y seg2) (op max :y seg1)))

    :else
    (and
     ;; check if they are outside in opposite sides
     (< (op min :y seg2) (op min :y seg1))
     (> (op max :y seg2) (op max :y seg1))
     ;; check if they are within range
     (> (op min :x seg2) (op min :x seg1))
     (< (op max :x seg2) (op max :x seg1)))))


(defn segments [c1 c3]
  (let [c2 {:x (:x c3) :y (:y c1)}
        c4 {:x (:x c1) :y (:y c3)}]
    (partition 2 1 [c1 c2 c3 c4 c1])))


(defn is-valid-connection?
  "The connection is valid if no points from the perimeter are inside the rectangle"
  [perimeter area]
  (let [c1 (:from area)
        c2 (:to area)
        intersection-pairs (for [x (segments c1 c2)
                                 y (perimeter-segments-cached perimeter)] [x y])
        intersection-pairs [(second intersection-pairs)]]
    ;; (println (count intersection-pairs))
    ;; (println (count (segments c1 c2)))
    ;; (println (count (perimeter-segments-cached perimeter)))
    ;; (println (perimeter-segments-cached perimeter))
    ;; (println intersection-pairs)
    (cond
      ;; If any point is inside we already know it is invalid
      (some #(is-inside? c1 c2 % false) perimeter)
      false

      ;; Check for any segment intersecting the rectangle.
      (some #(intersect? %) intersection-pairs)
      false

      :else
      true)))


(defn debug-my-code []
  (let [area {:area 4502966061, :from {:x 17482, :y 84618}, :to {:x 82284, :y 15132}}
        perimeter [{:x 1738 :y 48641} {:x 94997 :y 48641}]]
    (println (is-valid-connection? perimeter area))))

;; (debug-my-code)


(defn get-chr [point perimeter]
  (cond
    (first (filter #(= point %) perimeter))
    ;; (str (.indexOf perimeter point))
    "#"

    ;; (is-outside? {:x 9 :y 7} {:x 2 :y 3} point)
    ;; "."

    :else
    "."))


(defn get-chr-fast [point perimeter-set]
  (if (contains? perimeter-set point)
    "#" "."))


(defn pp-debug [perimeter]
  (let [width (+ 2 ;; pad
                 (inc (apply max (mapv :x perimeter))))
        height (+ 1 ;; pad
                  (inc (apply max (mapv :y perimeter))))
        perimeter-set (set perimeter)]
    (println width height)
    (doall
     (for [y (range height)]
       (println
        (reduce str (mapv #(get-chr-fast {:x % :y y} perimeter-set) (range width))))))))


(defn crack-the-code
  ([lines] (crack-the-code lines false))
  ([lines verbose?]
   (let [corners (->> lines
                      (map parse-corners)
                      vec)
         perimeter corners ;; as it turned out, the perimeter and corners are the same
         areas (calculate-areas corners)
         _ (when verbose? (pp-debug perimeter))
         larger-area (->>
                      areas
                      (filter (partial is-valid-connection? perimeter))
                      (sort-by :area)
                      ;; (mapv :area)
                      ;; (apply max)
                      (last))]
     (println "Larger area: " larger-area)
     (:area larger-area))))


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

;; current (wrong) larger area:
;; {:area 4502966061, :from {:x 17482, :y 84618}, :to {:x 82284, :y 15132}}
