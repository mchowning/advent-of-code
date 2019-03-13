(ns advent-of-code.day2
  (:require [clojure.math.combinatorics :as combo]))

(def input
  (clojure.string/split-lines
   (slurp "input/day2.txt")))

;; Part 1 ---------------------------------------------------------------------------------

(defn wordCheck [word]
  (let [counts (vals (frequencies word))
        numTwos (some #{2} counts)
        numThrees (some #{3} counts)
        limit-to-1 #(if % 1 0)]
    [(limit-to-1 numTwos) (limit-to-1 numThrees)]))

(defn combiner [acc word]
  (map + acc (wordCheck word)))

(def part1
  (let [[twos threes] (reduce combiner [0 0] input)]
    (* twos threes)))

;; ------------------------------

(defn occurrenceFrequencies [word]
  (str "Returns a set with the frequencies of letters in the word. "
       "For example, 'matt' has single and double letters, so it "
       "would return #{1 2}")
    (-> word frequencies vals set))

(defn combiner [acc word]
  (let [freqs (occurrenceFrequencies word)
        keywordOf #(keyword (str %))
        inc' (fnil inc 0)
        combiner' #(update %1 (keywordOf %2) inc')]
    (reduce combiner' acc freqs)))

(def part1'
  (let [{twos :2 threes :3} (reduce combiner {} input)]
  (* twos threes)))


;; Part 2 ---------------------------------------------------------------------------------


(defn numDiffChars [words]
  (let [numDiffChars #(count (set %))
        hasDiffChars #(not= 1 (numDiffChars %))
        pairedChars (apply map vector words)]
    (count (filter hasDiffChars pairedChars))))

(defn string-pairs-differing-by-1-char [words]
  (let [pairs (combo/combinations words 2)
        has-1-diff-char #(= 1 (numDiffChars %))]
    (filter has-1-diff-char pairs)))

(defn only-matching-chars [[w1 w2]]
  (let [pairedChars (map vector w1 w2)
        onlyMatchingPairs (filter #(= (first %) (second %)) pairedChars)]
    (map first onlyMatchingPairs)))

(def part2 (apply str (->>
                       input
                       string-pairs-differing-by-1-char
                       first
                       only-matching-chars)))

;; ---------------------------------------------------------------------------------

(def test_
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 6448)
    (checker part2 "evsialkqyiurohzpwucngttmf")))

(def test-input ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
