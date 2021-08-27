(ns advent-of-code.day4
  ;; (:require [clojure.math.combinatorics :as combo]
            ;; [clojure.algo.generic.functor :refer [fmap]]
            ;; :verbose
  ;; )
)

(defn read-input [raw-input]
  (let [lines (clojure.string/split-lines raw-input)]
    lines))

(def input (read-input (slurp "input/day4.txt")))

(defn read-line [line]
  (let [[_ year month day hour minute rest] (re-matches #"^\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.*)$" line)
        date-object {:year year :month month :day day :hour hour :minute minute}
        [_ match-guard-num] (re-matches #"^Guard #(\d+) begins shift$" rest)
        match-falls-asleep (re-matches #"^falls asleep" rest)
        match-wakes-up (re-matches #"^wakes up" rest)]
    (merge {:date date-object}
     (cond
             match-guard-num {:type :type/begins-shift :guard-num match-guard-num}
             match-falls-asleep {:type :type/falls-asleep}
             match-wakes-up {:type :type/wakes-up}))))


;; Part 1 ---------------------------------------------------------------------------------


(defn fix-day [{date :date
                type :type
                guard-num :guard-num}]
  (let [{hour :hour
         day :day} date
        mod-day (if (= hour 23)
                  (inc day)
                  day)
        mod-date (dissoc (assoc date :day mod-day) :hour)]
    {:date mod-date :type type :guard-num guard-num}))
;; FIXME now we have a bunch of `nil` guard-nums

(def part1
  (map read-line test-input))


;; Test ----------------------------------------------------------------------------------


(def test
  (let [checker #(if (= %1 %2)
                   (println "Success!")
                   (println (str "Failure! Expected " %2 ", but got " %1)))]
    ;; (checker part1 101781)
    ;; (checker (:num (first part2)) 909)
))

(def test-input (read-input
                 "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"))
