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

(defn claim-coords [dicts]
  (let [map-claim (fn [dict]
                    (map-coords {}
                                (:num dict)
                                (get-coords dict)))]
    (map map-claim dicts)))

(defn coordinate-map [dicts]
  (let [claims (claim-coords dicts)]
    (apply merge-with concat (claim-coords dicts))))

(defn has-more-than-1-val [dicts]
  (let [more-than-1-val #(->> % val count (< 1))]
    (->>
     dicts
     coordinate-map
     (filter more-than-1-val)
     keys
     concat)))

(def part1 (count (has-more-than-1-val input)))


;; Part 2 ---------------------------------------------------------------------------------


(def part2
  (let [set-with-more-than-1 (set (has-more-than-1-val input))
        coord-with-more-than-1 #(contains? set-with-more-than-1 %)
        all-good #(not (some coord-with-more-than-1 (get-coords %)))]
    (filter all-good input)))


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
