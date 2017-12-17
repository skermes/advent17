(ns advent17.sixteen
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def example-input "s1,x3/4,pe/b")
(def example-programs ["a" "b" "c" "d" "e"])

(defn puzzle-input [] (s/trim (slurp "input/sixteen.txt")))
(def puzzle-programs ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"])

(def total-dances "" 1000000000)
;; Number of times to run the full puzzle dance to return to the original position.
;; Determined by playing around with print-positions-with-history
(def dance-cycle-length "" 63)

(defmulti parse-move first)

(defmethod parse-move \s [move-str]
  {:move :spin :count (Integer/parseInt (subs move-str 1))})

(defmethod parse-move \x [move-str]
  (let [[a-str b-str] (s/split (subs move-str 1) #"/")]
    {:move :exchange :a (Integer/parseInt a-str) :b (Integer/parseInt b-str)}))

(defmethod parse-move \p [move-str]
  (let [[a b] (s/split (subs move-str 1) #"/")]
    {:move :partner :a a :b b}))

(defn parse-moves [text]
  (map parse-move (s/split text #",")))

(defn index-of
  ([coll x] (index-of coll x 0))
  ([[head & tail] x idx]
    (cond (nil? head) nil
          (= head x) idx
          :else (recur tail x (+ idx 1)))))

(defmulti dance (fn [programs move] (:move move)))

(defmethod dance :spin [programs move]
  (let [spin-index (- (count programs) (:count move))
        [first-half second-half] (split-at spin-index programs)]
    (vec (concat second-half first-half))))

(defmethod dance :exchange [programs move]
  (assoc programs (:a move) (nth programs (:b move))
                  (:b move) (nth programs (:a move))))

(defmethod dance :partner [programs move]
  (let [a-index (index-of programs (:a move))
        b-index (index-of programs (:b move))]
    (assoc programs a-index (:b move)
                    b-index (:a move))))

(defn dance-moves [programs moves]
  (reduce dance programs moves))

(defn all-dance-positions [programs moves]
  (reductions dance programs moves))

(defn part-one [] (->> (puzzle-input)
                       parse-moves
                       (dance-moves puzzle-programs)
                       s/join))

(defn print-positions-with-history
  ([positions] (print-positions-with-history positions {} 0))
  ([[head & tail] history i]
    (if (nil? head)
        (let [long-repeats (filter (fn [[k v]] (> (- (last v) (first v)) 10)) history)]
          (println long-repeats)
          (println i "total positions")
          (println (count history) "unique positions")
          (println (count long-repeats) "long repeats"))
        (let [pos-history (get history head [])]
          ; (println i head pos-history)
          (recur tail (assoc history head (conj pos-history i)) (+ i 1))))))

(defn part-two []
  (let [original-moves (parse-moves (puzzle-input))
        dances-remaining (mod total-dances dance-cycle-length)
        total-moves (take (* dances-remaining (count original-moves)) (cycle original-moves))]
    (s/join (dance-moves puzzle-programs total-moves))))

(defn day []
  (println (time (part-one)))
  (println)
  (println (time (part-two))))
