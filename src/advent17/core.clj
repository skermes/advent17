(ns advent17.core
  (:gen-class)
  (:require [advent17.one]
             [advent17.two]))

(def days {
  "one" advent17.one/day
  "two" advent17.two/day})

(defn -main
  [& args]
  ((days (first args))))
