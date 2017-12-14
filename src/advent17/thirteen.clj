(ns advent17.thirteen
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def example-input "0: 3
1: 2
4: 4
6: 4")

(defn puzzle-input [] (slurp "input/thirteen.txt"))

(defn parse-line [line]
  (let [[depth-str range-str] (s/split line #": ")]
    [(Integer/parseInt depth-str) (Integer/parseInt range-str)]))

(defn parse-input [text]
  (map parse-line (s/split-lines text)))

(defn layer-cycle-time [layer-range]
  (- (* layer-range 2) 2))

(defn caught-by [start-time [layer-depth layer-range]]
  (= 0 (mod (+ layer-depth start-time) (layer-cycle-time layer-range))))

(defn caught-by-any [start-time layers]
  (shared/any (map (partial caught-by start-time) layers)))

(defn severity [[layer-depth layer-range]]
  (* layer-depth layer-range))

(defn first-uncaught-start [layers]
  (shared/firstp #(not (caught-by-any % layers)) (range)))

(defn part-one [] (->> (puzzle-input)
                       parse-input
                       (filter (partial caught-by 0))
                       (map severity)
                       (reduce +)))

(defn part-two [] (->> (puzzle-input)
                       parse-input
                       first-uncaught-start))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
