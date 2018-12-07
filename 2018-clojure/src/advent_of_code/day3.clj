(ns advent-of-code.day3
  (:require [clojure.math.combinatorics :as combo]))

(def input
  (clojure.string/split-lines
   (slurp "input/day3.txt")))

(defn parse-line [line]
  (let [[_ n x y w h] (re-matches #"^#(\d*) @ (\d*),(\d*): (\d*)x(\d*)$" line)]
    {:num (read-string n)
     :x (read-string x)
     :y (read-string y)
     :w (read-string w)
     :h (read-string h)}))

;; Part 1 ---------------------------------------------------------------------------------

(defn get-coords [dict]
  (let [dim-range #(range %1 (+ %1 %2))]
    (for [x (dim-range (:x dict) (:w dict))
          y (dim-range (:y dict) (:h dict))]
      [x y])))

(defn map-coords [accum n coords]
  (reduce (fn [accum' coord]
            (let [oldValue (get accum' coord [])]
              (assoc accum' coord (conj oldValue n))))
          accum
          coords))

(defn combiner [dicts]
  (let [counts (map #(map-coords {} (:num %) (get-coords %))  dicts)]
    (apply merge-with concat counts)))

(def part1
  (let [dicts (map parse-line input)
        coord-map (combiner dicts)
        has-more-than-1-val #(->>
                   %
                   val
                   count
                   (< 1))]
    (count (filter has-more-than-1-val coord-map))))


;; Part 2 ---------------------------------------------------------------------------------


;; ---------------------------------------------------------------------------------


(def test
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 101781)
    ;; (checker part2 "evsialkqyiurohzpwucngttmf")
))

(def test-input
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])
