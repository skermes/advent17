(ns advent17.six
  (:require [advent17.shared :as shared]
            [clojure.string :as s]))

(def puzzle-input "4  10  4 1 8 4 9 14  5 1 14  15  0 15  3 5")

(def example-input "0 2 7 0")

(defn initialize-banks [text]
  (->> (s/split text #"\s+") (mapv #(Integer/parseInt %))))

(defn distribute-leftovers [idx nblocks nbanks]
  (let [leftover-blocks (rem nblocks nbanks)]
    (->> (range (+ idx 1) (+ idx leftover-blocks 1))
         (mapv #(mod % nbanks)))))

(defn redistributed-block-count [everyone-blocks banks-with-leftovers bank]
  (if (contains? banks-with-leftovers bank)
      (+ everyone-blocks 1)
      everyone-blocks))

(defn redistributed-blocks [idx nblocks nbanks]
  (let [everyone-blocks (-> (/ nblocks nbanks) Math/floor Math/round)
        leftover-blocks (rem nblocks nbanks)
        banks-with-leftovers (set (distribute-leftovers idx nblocks nbanks))]
    (->> (range 0 nbanks)
         (mapv (partial redistributed-block-count everyone-blocks banks-with-leftovers)))))

(defn target-bank [banks]
  (let [maxbank (apply max banks)]
    (shared/firstp #(= (nth banks %) maxbank) (range 0 (count banks)))))

(defn banks-after-redistribution [banks]
  (let [target (target-bank banks)]
    (mapv + (assoc banks target 0)
            (redistributed-blocks target (nth banks target) (count banks)))))

(defn redistributions-until-repeat
  ([banks] (redistributions-until-repeat banks {}))
  ([banks prior-banks]
    (if (contains? prior-banks banks)
        {:final (count prior-banks) :prev (get prior-banks banks)}
        (recur (banks-after-redistribution banks) (assoc prior-banks banks (count prior-banks))))))

(defn part-one [] (:final (redistributions-until-repeat (initialize-banks puzzle-input))))

(defn part-two []
  (let [answer (redistributions-until-repeat (initialize-banks puzzle-input))]
    (- (:final answer) (:prev answer))))

(defn day []
  (time (println (part-one)))
  (println)
  (time (println (part-two))))

