(ns advent-of-code.day1)

(defn funcForLine [line]
  #(+ % (read-string line)))

(defn accumulateLines [lines]
  (reduce + (map read-string lines)))

(defn process-input [filename]
  (clojure.string/split-lines
   (slurp
    (str "input/" filename))))

(def input (process-input "day1-1.txt"))

(defn process-part2-lines [lines]
  (loop
   [[f & fs] (cycle (map funcForLine lines))
    seen #{0}
    total 0]
    (let [newTotal (f total)]
      (if (contains? seen newTotal)
        newTotal
        (recur fs (conj seen newTotal) newTotal)))))

(def part2 (process-part2-lines input))
(def part1 (accumulateLines input))
(def testDay1
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    (checker part1 470)
    (checker part2 790)))
