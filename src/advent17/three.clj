(ns advent17.three
  (:require [advent17.shared :as shared]))

(def puzzle-input 312051)

(defn squares [] (map #(* % %) (range)))

(defn odd-squares [] (filter odd? (squares)))

(defn shell [n]
  (->> (odd-squares)
       (shared/firstp #(>= % n))
       Math/sqrt
       Math/round))

;; It's a little hard to explain this except by looking at the excel sheet that
;; I worked it out in.
(defn spiral->manhattan [n]
  (let [spiral-shell (shell n)
        corner-distance (- spiral-shell 1)
        delta (- (* spiral-shell spiral-shell) n)]
    (- corner-distance (min (mod delta (- spiral-shell 1))
                            (mod (- delta) (- spiral-shell 1))))))

(defn doubled-range [] (mapcat #(list % %) (range)))

(defn spiral-arm-lengths [] (drop 2 (doubled-range)))

(defn spiral-arm-directions [] (cycle [[1 0] [0 -1] [-1 0] [0 1]]))

(defn spiral-arm-vectors []
  (map (fn [l d] [l d]) (spiral-arm-lengths) (spiral-arm-directions)))

(defn extend-spiral-coords [coords [length [dx dy]]]
  (let [[x y] (last coords)]
    (->> (range 1 (+ length 1))
         (map (fn [distance] [(+ x (* distance dx)) (+ y (* distance dy))]))
         (concat coords))))

(defn spiral-coords [arm-count]
  (reduce extend-spiral-coords [[0 0]] (take arm-count (spiral-arm-vectors))))

(defn adjacent-coords [[x y]]
  (let [offsets [[-1 -1] [0 -1] [1 -1]
                 [-1 0]  [0 0]  [1 0]
                 [-1 1]  [0 1]  [1 1]]]
    (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) offsets)))

(defn adjacent-sum [field coord]
  (reduce (fn [sum coord] (+ sum (get field coord 0)))
          0
          (adjacent-coords coord)))

(defn update-spiral-field [field coord]
  (let [new-val (adjacent-sum field coord)]
    (assoc field coord new-val :max new-val)))

(defn incremental-spiral-fields [arm-count]
  (reductions update-spiral-field
              {[0, 0] 1 :max 1}
              (spiral-coords arm-count)))

(defn part-one [] (spiral->manhattan puzzle-input))

(defn part-two [] (:max (shared/firstp #(> (:max %) puzzle-input)
                                       (incremental-spiral-fields 20))))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
