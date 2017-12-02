(ns advent17.core
  (:gen-class)
  (:require [advent17.one]
            [advent17.two]
            [advent17.three]
            [advent17.four]
            [advent17.five]
            [advent17.six]
            [advent17.seven]
            [advent17.eight]
            [advent17.nine]
            [advent17.ten]))

(def days {
  "one" advent17.one/day
  "two" advent17.two/day
  "three" advent17.three/day
  "four" advent17.four/day
  "five" advent17.five/day
  "six" advent17.six/day
  "seven" advent17.seven/day
  "eight" advent17.eight/day
  "nine" advent17.nine/day
  "ten" advent17.ten/day})

(defn -main
  [& args]
  ((days (first args))))
