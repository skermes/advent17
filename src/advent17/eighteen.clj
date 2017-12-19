(ns advent17.eighteen
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [advent17.shared :as shared]))

(def example-input "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

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
    {:instruction :send :value (parse-value value)}))

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
    {:instruction :receive :register register}))

(defmethod parse-instruction "jgz" [line]
  (let [[_ value offset] (tokens line)]
    {:instruction :jump-greater-than-zero
     :value (parse-value value)
     :offset (parse-value offset)}))

(defn parse-input [text]
  (mapv parse-instruction (s/split-lines text)))

(defn new-machine [instructions program-id]
  {:registers {"p" program-id}
   :instructions instructions
   :pointer 0
   :outgoing-messages []
   :incoming-messages []
   :blocked false
   :instruction-counter 0
   :all-received-messages []
   :all-sent-messages []})

(defn get-value [machine value]
  (if (string? value)
      (get (:registers machine) value 0)
      value))

(defmulti eval-instruction (fn [machine instruction] (:instruction instruction)))

(defmethod eval-instruction :send [{msgs :outgoing-messages :as machine} {value :value}]
  (assoc machine
         :outgoing-messages (conj msgs (get-value machine value))
         :all-sent-messages (conj (:all-sent-messages machine) (get-value machine value))))

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

(defmethod eval-instruction :receive [machine {register :register}]
  (let [{[first-msg & rest-msgs] :incoming-messages pointer :pointer} machine]
    (if (nil? first-msg)
        (assoc machine :blocked true :pointer pointer)
        (-> machine
            (assoc-in [:registers register] first-msg)
            (assoc :incoming-messages (vec rest-msgs))
            (assoc :pointer (+ pointer 1))
            (assoc :all-received-messages (conj (:all-received-messages machine) first-msg))))))

(defmethod eval-instruction :jump-greater-than-zero [machine {value :value offset :offset}]
  (let [actual-offset (if (> (get-value machine value) 0) (get-value machine offset) 1)]
    (assoc machine :pointer (+ (:pointer machine) actual-offset))))

(def sets-pointer? #{:jump-greater-than-zero :receive})

(defn set-pointer [machine instruction]
  (if (sets-pointer? (:instruction instruction))
      machine
      (assoc machine :pointer (+ (:pointer machine) 1))))

(defn set-instruction-counter [machine]
  (assoc machine :instruction-counter (+ (:instruction-counter machine) 1)))

(defn do-instruction [{pointer :pointer counter :instruction-counter :as machine}]
  (let [instruction (get-in machine [:instructions pointer])]
    (-> machine
        (eval-instruction instruction)
        (set-pointer instruction)
        set-instruction-counter)))

(defn set-message-queues [[machine-0 machine-1]]
  [(assoc machine-0 :incoming-messages (:outgoing-messages machine-1)
                    :outgoing-messages [])
   (assoc machine-1 :incoming-messages (:outgoing-messages machine-0)
                    :outgoing-messages [])])

(defn reset-machine [machine]
  (assoc machine :blocked false :instruction-counter 0))

(defn run-until-blocked [machine]
  (if (:blocked machine)
      machine
      (recur (do-instruction machine))))

(defn run-until-deadlock [machines]
  (let [blocked-machines (map run-until-blocked machines)
        total-instructions (reduce + (map :instruction-counter blocked-machines))]
        ; If each machine ran one instruction, they all blocked immediately, i.e., deadlock
    (if (= total-instructions (count machines))
        machines
        (recur (map reset-machine (set-message-queues blocked-machines))))))

;; Part one and part two are different enough that I don't want to bother
;; getting both of them to run in one codebase.
; (defn machine-states [machine]
;   (reductions do-instruction machine (range)))
;
; (defn part-one [] (->> puzzle-input
;                        parse-input
;                        new-machine
;                        machine-states
;                        (shared/firstp #(not (nil? (:recovered-value %))))
;                        :recovered-value))

(defn part-two []
  (let [instructions (parse-input puzzle-input)
        machines [(new-machine instructions 0) (new-machine instructions 1)]]
    (count (:all-sent-messages (nth (run-until-deadlock machines) 1)))))

(defn day []
  ; (pprint/pprint (time (part-one)))
  ; (println)
  (pprint/pprint (time (part-two))))
