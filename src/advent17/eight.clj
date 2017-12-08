(ns advent17.eight
  (:require [clojure.string :as s]))

(def example-input "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(defn puzzle-input [] (slurp "input/eight-input.txt"))

(def operators {"inc" +
                "dec" -})

(def comparators {"==" =
                  "!=" (fn [& args] (not (apply = args)))
                  ">" >
                  "<" <
                  ">=" >=
                  "<=" <=})

(defn parse-instruction [line]
  (let [tokens (s/split line #"\s+")]
    {:store-register (nth tokens 0)
     :operator (operators (nth tokens 1))
     :operand (Integer/parseInt (nth tokens 2))
     :compare-register (nth tokens 4)
     :comparator (comparators (nth tokens 5))
     :target (Integer/parseInt (nth tokens 6))}))

(defn instructions [text]
  (map parse-instruction (s/split-lines text)))

(defn new-machine [] {:max-register-ever 0})

(defn get-reg [machine register]
  (get machine register 0))

(defn instruction-compare [machine instruction]
  ((:comparator instruction)
   (get-reg machine (:compare-register instruction))
   (:target instruction)))

(defn instruction-calculate [machine instruction]
  ((:operator instruction)
   (get-reg machine (:store-register instruction))
   (:operand instruction)))

(defn eval-instruction [machine instruction]
  (if (instruction-compare machine instruction)
      (let [new-value (instruction-calculate machine instruction)
            new-max-ever (max new-value (:max-register-ever machine))]
        (assoc machine (:store-register instruction) new-value
                       :max-register-ever new-max-ever))
      machine))

(defn eval-instructions [machine instructions]
  (reduce eval-instruction machine instructions))

(defn max-register [machine]
  (apply max (vals (dissoc machine :max-register-ever))))

(defn part-one [] (max-register (eval-instructions (new-machine) (instructions (puzzle-input)))))

(defn part-two [] (:max-register-ever (eval-instructions (new-machine) (instructions (puzzle-input)))))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
