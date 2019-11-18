(ns advent-of-code.day2
  (:require [clojure.math.combinatorics :as combo]))

(def input
  (clojure.string/split-lines
   (slurp "input/day2.txt")))

;; Part 1 ---------------------------------------------------------------------------------

(def part1
  (let [repetitions (map (comp vals frequencies) input)
        numWithNRepetitions #(->> repetitions
                                  (filter (partial some #{%}))
                                  count)]
    (* (numWithNRepetitions 2) (numWithNRepetitions 3))))


;; Part 2 ---------------------------------------------------------------------------------


(defn numDiffChars [wordPair]
  (let [pairedChars (apply map vector wordPair)]
    (->> pairedChars
         (filter (partial apply not=))
         count)))

(defn string-pairs-differing-by-1-char [words]
  (let [pairs (combo/combinations words 2)]
    ;; (filter #(->> % numDiffChars (= 1)) pairs)))
    (filter #(= 1 (numDiffChars %)) pairs)))

(defn only-matching-chars [[w1 w2]]
  (let [pairedChars (map vector w1 w2)
        onlyMatchingPairs (filter (partial apply =) pairedChars)]
    (map first onlyMatchingPairs)))

(def part2 (->> input
                string-pairs-differing-by-1-char
                first
                only-matching-chars
                (apply str)))

;; ---------------------------------------------------------------------------------

(def test_
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 6448)
    (checker part2 "evsialkqyiurohzpwucngttmf")))

(def test-input ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
