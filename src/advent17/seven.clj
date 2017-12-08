(ns advent17.seven
  (:require [clojure.string :as s]))

(def example-input "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(defn puzzle-input [] (slurp "input/seven-input.txt"))

(def split-re "" #"[\s,\->\(\)]+")

(defn structure-split-input [[vertex weight & kids]]
  {:name vertex :weight (Integer/parseInt weight) :children (if (nil? kids) '() kids)})

(defn collect-vertices [graph new-vertex]
  (assoc graph (:name new-vertex) new-vertex))

(defn parsed-input-lines [text]
  (->> text
       s/split-lines
       (map #(s/split % split-re))
       (map structure-split-input)
       (reduce collect-vertices {})))

(defn anti-filter [pred coll]
  (filter #(not (pred %)) coll))

(defn root-vertex [graph]
  (let [all-children (set (mapcat :children (vals graph)))]
    (first (anti-filter all-children (keys graph)))))

(defn get-children [vertex graph]
  (map graph (:children vertex)))

(defn subtree-weight [vertex graph]
  (reduce + (cons (:weight vertex) (map #(subtree-weight % graph) (get-children vertex graph)))))

(defn balanced? [vertex graph]
  (if (empty? (:children vertex))
      true
      (apply = (map #(subtree-weight % graph) (get-children vertex graph)))))

(defn smallest-unbalanced-subtree [vertex graph]
  (if (balanced? vertex graph)
      nil
      ;; The puzzle description asserts that there's only one unbalanced node
      (let [unbalanced-kid (first (anti-filter #(balanced? % graph) (get-children vertex graph)))]
        (if (nil? unbalanced-kid)
            vertex
            (smallest-unbalanced-subtree unbalanced-kid graph)))))

(defn rebalanced-weight [graph]
  ;; This is super gross but I think I just chose a bad graph representation and
  ;; I don't want to go back and change it.
  (let [problem-parent (smallest-unbalanced-subtree (graph (root-vertex graph)) graph)
        weights (group-by #(subtree-weight % graph) (get-children problem-parent graph))
        problem (first (first (filter #(= (count %) 1) (vals weights))))
        target-subtree-weight (first (filter #(> (count (weights %)) 1) (keys weights)))]
    (- target-subtree-weight (- (subtree-weight problem graph) (:weight problem)))))

(defn part-one [] (root-vertex (parsed-input-lines (puzzle-input))))

(defn part-two [] (rebalanced-weight (parsed-input-lines (puzzle-input))))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))

