(ns advent17.ten
  (:require [clojure.string :as s]))

(def example-input "3,4,1,5")

(def puzzle-input "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108")

(def example-knots {:knots [0 1 2 3 4] :position 0 :skip 0})

(def puzzle-knots {:knots (vec (range 256)) :position 0 :skip 0})

(defn parse-input [text]
  (map #(Integer/parseInt %) (s/split text #",")))

(defn parse-input-ascii [text]
  (map int text))

(defn part-two-lengths [text]
  (conj (vec (parse-input-ascii text)) 17 31 73 47 23))

(defn twist-indices [{knots :knots position :position} length]
  (map #(mod % (count knots)) (range position (+ position length))))

(defn twisted-knots [{knots :knots position :position :as knot-state} length]
  (if (= length 0)
      knots
      (let [indices (twist-indices knot-state length)
            twisted-vals (reverse (map #(nth knots %) indices))]
        (apply assoc (vec (cons knots (interleave indices twisted-vals)))))))

(defn advance-knot-state [{position :position skip :skip :as knot-state} length]
  (let [new-pos (+ position length skip)
        new-skip (+ skip 1)
        new-knots (twisted-knots knot-state length)]
    {:knots new-knots :position new-pos :skip new-skip}))

(defn twisted-front-multiply [knot-state lengths]
  (let [{knots :knots} (reduce advance-knot-state knot-state lengths)]
    (* (nth knots 0) (nth knots 1))))

(defn sparse-hash [knot-state lengths]
  (let [iterated-lengths (take (* (count lengths) 64) (cycle lengths))]
    (:knots (reduce advance-knot-state knot-state iterated-lengths))))

(defn dense-hash [s-hash]
  (map #(apply bit-xor %) (partition 16 s-hash)))

(defn knot-hash [d-hash]
  (s/join (map #(format "%02x" %) d-hash)))

(defn part-one [] (twisted-front-multiply puzzle-knots (parse-input puzzle-input)))

(defn part-two []
  (->> puzzle-input
       part-two-lengths
       (sparse-hash puzzle-knots)
       dense-hash
       knot-hash))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))

