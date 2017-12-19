(ns advent17.nineteen
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [advent17.shared :as shared]))

;; Not doing this as a multiline string because my editor strips trailing
;; whitespace which breaks the diagram
(def example-input "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ ")
(def example-end-letter "" \F)

(defn puzzle-input [] (slurp "input/nineteen.txt"))
;; Determined by looking at puzzle
(def puzzle-end-letter "" \P)

(defn parse-input [text]
  (mapv vec (s/split-lines text)))

(defn starting-col [top-row]
  (shared/index-of top-row \|))

;; These are in [row col] not [x y]
(def directions {:up [-1 0]
                 :down [1 0]
                 :left [0 -1]
                 :right [0 1]})

(def turns {:up [:left :right]
            :down [:left :right]
            :left [:up :down]
            :right [:up :down]})

(defn starting-state [diagram]
  {:position [0 (starting-col (first diagram))]
   :direction :down
   :finished false
   :letters []
   :steps 0})

(defn char-at [diagram [row col]]
  (nth (nth diagram row) col))

(defn new-position [position direction]
  (mapv + position (direction directions)))

(defn not-empty-move? [diagram state direction]
  (not (= \space (char-at diagram (new-position (:position state) direction)))))

(defn turn-corner [diagram {direction :direction :as state}]
  (let [new-direction (shared/firstp (partial not-empty-move? diagram state) (turns direction))]
    (assoc state :direction new-direction
                 :position (new-position (:position state) new-direction))))

(defn move [diagram end-letter state]
  (let [current-char (char-at diagram (:position state))]
    (cond (= current-char end-letter)
            (assoc state :letters (conj (:letters state) current-char)
                         :finished true)
          (= current-char \+)
            (turn-corner diagram state)
          (shared/re-matches? #"[A-Z]" (str current-char))
            (assoc state :position
                         (new-position (:position state) (:direction state))
                         :letters (conj (:letters state) current-char))
          :else
            (assoc state :position
                         (new-position (:position state) (:direction state))))))

(defn move-until-end [diagram end-letter state]
  (if (:finished state)
      state
      (let [moved (move diagram end-letter state)]
        (recur diagram end-letter (assoc moved :steps (+ 1 (:steps moved)))))))

(defn part-one []
  (let [diagram (parse-input (puzzle-input))
        state (starting-state diagram)]
    (s/join (:letters (move-until-end diagram puzzle-end-letter state)))))

(defn part-two []
  (let [diagram (parse-input (puzzle-input))
        state (starting-state diagram)]
    (:steps (move-until-end diagram puzzle-end-letter state))))

(defn day []
  (println (time (part-one)))
  (println)
  (println (time (part-two))))
