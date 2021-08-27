(ns advent-of-code.day3
  (:require [clojure.math.combinatorics :as combo]
            [clojure.algo.generic.functor :refer [fmap]]
            ;; :verbose
))

(def input
  (let
   [parse-line (fn parse-line [line]
                 (let [[_ n x y w h] (re-matches #"^#(\d*) @ (\d*),(\d*): (\d*)x(\d*)$" line)]
                   {:num (read-string n)
                    :x (read-string x)
                    :y (read-string y)
                    :w (read-string w)
                    :h (read-string h)}))]
    (map parse-line (clojure.string/split-lines
                     (slurp "input/day3.txt")))))


;; Part 1 ---------------------------------------------------------------------------------


(defn get-coords [{x :x y :y w :w h :h}]
  (let [dim-range #(range %1 (+ %1 %2))]
    (for [x' (dim-range x w)
          y' (dim-range y h)]
      [x' y'])))

(defn claim-coords [dicts]
  (let [reduce-claim #(reduce (fn [accum coord]
                             (let [oldClaims (get accum coord [])]
                               (assoc accum coord (conj oldClaims (:num %)))))
                           {}
                           (get-coords %))]
    (map reduce-claim dicts)))

(defn coordinate-map [dicts]
  (let [claims (claim-coords dicts)]
    (apply merge-with concat (claim-coords dicts))))

(defn claims-with-conflicts [dicts]
  (let [more-than-1-claim #(->> % val count (< 1))]
    (->>
     dicts
     coordinate-map
     (filter more-than-1-claim)
     keys
     concat)))

(def part1 (count (claims-with-conflicts input)))


;; Part 2 ---------------------------------------------------------------------------------


(def part2
  (let [coords-with-multiple-claims (set (claims-with-conflicts input))
        coord-has-multiple-claims? #(contains? coords-with-multiple-claims %)
        claim-has-conflicts? #(some coord-has-multiple-claims? (get-coords %))]
    (filter #(not (claim-has-conflicts? %)) input)))

;; Test ----------------------------------------------------------------------------------


(def test
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 101781)
    (checker (:num (first part2)) 909)
))

(def test-input
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])
