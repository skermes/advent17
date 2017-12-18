(ns advent17.eighteen
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [advent17.shared :as shared]))

(def example-input "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(def puzzle-input "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 826
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19")

(defn re-matches? [re string]
  (not (nil? (re-matches re string))))

(defn parse-value [value]
  (if (re-matches? #"-?\d+" value)
      (Integer/parseInt value)
      value))

(defn tokens [line]
  (s/split line #" "))

(defmulti parse-instruction #(first (tokens %)))

(defmethod parse-instruction "snd" [line]
  (let [[_ value] (tokens line)]
    {:instruction :sound :value (parse-value value)}))

(defmethod parse-instruction "set" [line]
  (let [[_ register value] (tokens line)]
    {:instruction :set :register register :value (parse-value value)}))

(defmethod parse-instruction "add" [line]
  (let [[_ register value] (tokens line)]
    {:instruction :add :register register :value (parse-value value)}))

(defmethod parse-instruction "mul" [line]
  (let [[_ register value] (tokens line)]
    {:instruction :multiply :register register :value (parse-value value)}))

(defmethod parse-instruction "mod" [line]
  (let [[_ register value] (tokens line)]
    {:instruction :modulo :register register :value (parse-value value)}))

(defmethod parse-instruction "rcv" [line]
  (let [[_ register] (tokens line)]
    {:instruction :recover :value register}))

(defmethod parse-instruction "jgz" [line]
  (let [[_ value offset] (tokens line)]
    {:instruction :jump-greater-than-zero
     :value (parse-value value)
     :offset (parse-value offset)}))

(defn parse-input [text]
  (mapv parse-instruction (s/split-lines text)))

(defn new-machine [instructions]
  {:registers {}
   :instructions instructions
   :pointer 0
   :last-sound-played nil
   :recovered-value nil})

(defn get-value [machine value]
  (if (string? value)
      (get (:registers machine) value 0)
      value))

(defmulti eval-instruction (fn [machine instruction] (:instruction instruction)))

(defmethod eval-instruction :sound [machine {value :value}]
  (assoc machine
         :last-sound-played (get-value machine value)))

(defmethod eval-instruction :set [machine {register :register value :value}]
  (assoc-in machine [:registers register] (get-value machine value)))

(defmethod eval-instruction :add [machine {register :register value :value}]
  (assoc-in machine [:registers register]
                    (+ (get-value machine register) (get-value machine value))))

(defmethod eval-instruction :multiply [machine {register :register value :value}]
  (assoc-in machine [:registers register]
                    (* (get-value machine register) (get-value machine value))))

(defmethod eval-instruction :modulo [machine {register :register value :value}]
  (assoc-in machine [:registers register]
                    (mod (get-value machine register) (get-value machine value))))

(defmethod eval-instruction :recover [machine {value :value}]
  (if (zero? (get-value machine value))
      machine
      (assoc machine :recovered-value (:last-sound-played machine))))

(defmethod eval-instruction :jump-greater-than-zero [machine {value :value offset :offset}]
  (let [actual-offset (if (> (get-value machine value) 0) (get-value machine offset) 1)]
    (assoc machine :pointer (+ (:pointer machine) actual-offset))))

(def sets-pointer? #{:jump-greater-than-zero})

(defn do-instruction [{pointer :pointer :as machine} i]
  (let [instruction (get-in machine [:instructions pointer])
        evaled-machine (eval-instruction machine instruction)]
    (if (sets-pointer? (:instruction instruction))
        evaled-machine
        (assoc evaled-machine :pointer (+ pointer 1)))))

(defn machine-states [machine]
  (reductions do-instruction machine (range)))

(defn part-one [] (->> puzzle-input
                       parse-input
                       new-machine
                       machine-states
                       (shared/firstp #(not (nil? (:recovered-value %))))
                       :recovered-value))

(defn part-two [] "day eighteen part two")

(defn day []
  (pprint/pprint (time (part-one)))
  (println)
  (println (time (part-two))))
