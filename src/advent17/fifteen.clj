(ns advent17.fifteen
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def a-seed "" 703)
(def b-seed "" 516)

(def a-factor "" 16807)
(def b-factor "" 48271)

(def a-picky-divisor "" 4)
(def b-picky-divisor "" 8)

(def divisor "" 2147483647)

(def n-values "" 40000000)
(def picky-n-values "" 5000000)

(defn prng-next [prev factor]
  (rem (* prev factor) divisor))

(defn prng-values [seed factor]
  (drop 1 (reductions prng-next seed (repeat factor))))

(defn picky-prng-values [seed factor required-divisor]
  (filter #(= (mod % required-divisor) 0) (prng-values seed factor)))

(defn last-sixteen-bits [n]
  (- n (bit-shift-left (bit-shift-right n 16) 16)))

(defn zip2 [coll-a coll-b]
  (map #(vector %1 %2) coll-a coll-b))

(defn equal-values [coll]
  (filter #(apply = %) coll))

(defn part-one [] (->> (zip2 (map last-sixteen-bits (prng-values a-seed a-factor))
                             (map last-sixteen-bits (prng-values b-seed b-factor)))
                       (take n-values)
                       equal-values
                       count))

(defn part-two []
  (let [a-values (picky-prng-values a-seed a-factor a-picky-divisor)
        b-values (picky-prng-values b-seed b-factor b-picky-divisor)]
    (->> (zip2 (map last-sixteen-bits a-values) (map last-sixteen-bits b-values))
         (take picky-n-values)
         equal-values
         count)))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
