(ns advent17.nine
  (:require [clojure.string :as s]))

(def example-inputs [
  "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  "{{<a!>},{<a!>},{<a!>},{<ab>}}"
  "{{<!>},{<!>},{<!>},{<a>}}"
  "{}"
  "{{}}"
  "{{{},{},{{}}}}"])

(defn puzzle-input [] (slurp "input/nine-input.txt"))

(defn remove-escapes [text]
  (s/replace text #"!." ""))

(defn remove-garbage [text]
  (s/replace text #"<.*?>" ""))

(defn score-groups
  ([text] (score-groups text 0 0))
  ([[first-char & rest-chars] depth score]
    (cond (nil? first-char) score
          (= \{ first-char) (recur rest-chars (+ depth 1) score)
          (= \} first-char) (recur rest-chars (- depth 1) (+ score depth))
          :else (recur rest-chars depth score))))

(defn count-garbage-content [text]
  (reduce (fn [acc garbage] (+ acc (- (count garbage) 2)))
          0
          (re-seq #"<.*?>" text)))

(defn part-one [] (-> (puzzle-input) remove-escapes remove-garbage score-groups))

(defn part-two [] (-> (puzzle-input) remove-escapes count-garbage-content))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))

