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
            [advent17.ten]
            [advent17.eleven]
            [advent17.twelve]
            [advent17.thirteen]
            [advent17.fourteen]
            [advent17.fifteen]
            [advent17.sixteen]))

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
  "ten" advent17.ten/day
  "eleven" advent17.eleven/day
  "twelve" advent17.twelve/day
  "thirteen" advent17.thirteen/day
  "fourteen" advent17.fourteen/day
  "fifteen" advent17.fifteen/day
  "sixteen" advent17.sixteen/day})

(defn -main
  [& args]
  ((days (first args))))
