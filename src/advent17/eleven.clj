(ns advent17.eleven
  (:require [clojure.string :as s]))

(def example-moves ["ne,ne,ne"
                    "ne,ne,sw,sw"
                    "ne,ne,s,s"
                    "se,sw,se,sw,sw"])

(defn parse-moves [text] (s/split text #","))

(defn hex-move [[x y] direction]
  (let [y-shift (if (odd? x) 0 -1)]
    (condp = direction
           "n" [x (- y 1)]
           "s" [x (+ y 1)]
           "ne" [(+ x 1) (+ y y-shift)]
           "se" [(+ x 1) (+ y 1 y-shift)]
           "nw" [(- x 1) (+ y y-shift)]
           "sw" [(- x 1) (+ y 1 y-shift)])))

(defn hex-moves [pos directions]
  (reduce hex-move pos directions))

; (defn direction [[from-x from-y] [to-x to-y]]
;   (cond (and (= from-x to-x) (= from-y to-y)) nil
;         (= from-x to-x) (if (< from-y to-y) "s" "n")
;         (< from-x to-x) (if ())))

(defn part-one [] (map (partial hex-moves [0 0]) (map parse-moves example-moves)))

(defn part-two [] "day eleven part two")

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
