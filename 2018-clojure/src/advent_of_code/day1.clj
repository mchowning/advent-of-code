(ns advent-of-code.day1)

(def lines
  (map read-string
       (clojure.string/split-lines
        (slurp "input/day1-1.txt"))))

(defn accumulateLines [lines]
  (reduce + lines))

(defn process-part2-lines [lines]
  (first-duplicate (reductions + (cycle lines))))

(defn first-duplicate [totals]
  (reduce #(if (%1 %2) (reduced %2) (conj %1 %2))
          #{0}
          totals))

(defn funcForLine [line]
  #(+ % line))

;; ----------------------------------------------------------

(def part2 (process-part2-lines input))
(def part1 (accumulateLines input))
(def testDay1
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 470)
    (checker part2 790)))
