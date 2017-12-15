(ns advent17.shared
  (:require [clojure.string :as s]))

(defn firstp [pred [head & tail]]
  (cond (nil? head) nil
        (pred head) head
        :else (recur pred tail)))

(defn anti-filter [pred coll]
  (filter #(not (pred %)) coll))

(defn any [coll]
  (reduce #(or %1 %2) coll))

(def starting-knots {:knots (vec (range 256)) :position 0 :skip 0})

(defn str->twist-lengths [string]
  (conj (vec (map int string)) 17 31 73 47 23))

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

(defn sparse-hash [knot-state lengths]
  (let [iterated-lengths (take (* (count lengths) 64) (cycle lengths))]
    (:knots (reduce advance-knot-state knot-state iterated-lengths))))

(defn dense-hash [s-hash]
  (map #(apply bit-xor %) (partition 16 s-hash)))

(defn knot-hash-str [d-hash]
  (s/join (map #(format "%02x" %) d-hash)))

(defn knot-hash [string]
  (->> string
       str->twist-lengths
       (sparse-hash starting-knots)
       dense-hash
       knot-hash-str))
