(ns advent17.seventeen
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def example-input "" 3)
(def puzzle-input "" 382)

(def iterations "" 2017)
(def long-iterations "" 50000000)

(defn spinlock-start-state [steps] {:values [0] :position 0 :steps steps})

(defn spinlock-insert [{values :values position :position steps :steps} new-value]
  (let [new-value-position (+ (mod (+ position steps) (count values)) 1)
        [values-before values-after] (split-at new-value-position values)]
    {:values (concat values-before [new-value] values-after)
     :position new-value-position
     :steps steps}))

(defn spinlock [spinlock-state]
  (reductions spinlock-insert spinlock-state (drop 1 (range))))

(defn value-after-last-value [spinlock-state]
  (nth (:values spinlock-state) (+ (:position spinlock-state) 1)))

(defn fast-spinlock-start-state [steps] {:value-after-zero nil
                                         :position 0
                                         :length 1
                                         :steps steps})

(defn fast-spinlock [state iterations]
  (if (zero? iterations)
      (:value-after-zero state)
      (let [new-position (+ (mod (+ (:position state) (:steps state)) (:length state)) 1)
            new-val (if (= new-position 1) (:length state) (:value-after-zero state))]
        (recur (assoc state :value-after-zero new-val
                            :position new-position
                            :length (+ (:length state) 1))
               (- iterations 1)))))

(defn part-one [] (->> puzzle-input
                       spinlock-start-state
                       spinlock
                       (take (+ iterations 1))
                       last
                       value-after-last-value))

(defn part-two [] (-> puzzle-input
                      fast-spinlock-start-state
                      (fast-spinlock long-iterations)))

(defn day []
  (println (time (part-one)))
  (println)
  (println (time (part-two))))
