(ns advent17.twelve
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def example-input "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn puzzle-input [] (slurp "input/twelve.txt"))

(defn parse-line [line]
  (let [[vertex neighbors] (s/split line #" <-> ")]
    [vertex (s/split neighbors #", ")]))

(defn parse-input [text]
  (mapcat parse-line (s/split-lines text)))

(defn build-graph [input]
  (apply assoc (cons {} input)))

(defn connected-vertices
  ([graph vertex] (connected-vertices graph [vertex] #{}))
  ([graph [next-visit & to-visit] visited]
    (if (nil? next-visit)
        visited
        (let [additional-visits (shared/anti-filter visited (graph next-visit))]
          (recur graph (concat to-visit additional-visits) (conj visited next-visit))))))

(defn count-connected-subgraphs
  ([graph] (count-connected-subgraphs graph 0))
  ([graph counted]
    (if (empty? graph)
        counted
        (let [vertex (first (keys graph))
              subgraph-vertices (connected-vertices graph vertex)
              graph-without-subgraph (apply (partial dissoc graph) subgraph-vertices)]
          (recur graph-without-subgraph (+ counted 1))))))


(defn part-one [] (-> (puzzle-input)
                      parse-input
                      build-graph
                      (connected-vertices "0")
                      count))

(defn part-two [] (-> (puzzle-input)
                      parse-input
                      build-graph
                      count-connected-subgraphs))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
