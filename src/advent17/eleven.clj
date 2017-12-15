(ns advent17.eleven
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def example-moves ["ne,ne,ne"
                    "ne,ne,sw,sw"
                    "ne,ne,s,s"
                    "se,sw,se,sw,sw"])

(defn puzzle-input [] (slurp "input/eleven.txt"))

(defn parse-moves [text] (s/split (s/trim text) #","))

;; Doing the coordinates kind of wrong here to try to make them fit in the
;; framework of
;; http://devmag.org.za/2013/08/31/geometry-with-hex-coordinates/

(def unit-vectors {"n"  [0 1 -1]
                   "s"  [0 -1 1]
                   "ne" [1 0 -1]
                   "se" [1 -1 0]
                   "nw" [-1 1 0]
                   "sw" [-1 0 1]})

(defn hex-move [pos direction]
  (mapv + pos (unit-vectors direction)))

(defn hex-moves [pos directions]
  (reduce hex-move pos directions))

(defn all-hex-moves [pos directions]
  (reductions hex-move pos directions))

(defn abs [n]
  (max n (- n)))

;; http://devmag.org.za/2013/08/31/geometry-with-hex-coordinates/
(defn hex-distance-to-origin [[x y z]]
  (/ (+ (abs x) (abs y) (abs z)) 2))

(defn part-one [] (->> (puzzle-input)
                       parse-moves
                       (hex-moves [0 0 0])
                       hex-distance-to-origin))

(defn part-two [] (->> (puzzle-input)
                       parse-moves
                       (all-hex-moves [0 0 0])
                       (map hex-distance-to-origin)
                       (apply max)))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
