(ns advent17.fourteen
  (:require [clojure.string :as s]
            [advent17.shared :as shared]))

(def puzzle-input "uugsqrei")
(def puzzle-rows 128)

(def set-bits {\0 []          ; 0000
               \1 [3]         ; 0001
               \2 [2]         ; 0010
               \3 [2 3]       ; 0011
               \4 [1]         ; 0100
               \5 [1 3]       ; 0101
               \6 [1 2]       ; 0110
               \7 [1 2 3]     ; 0111
               \8 [0]         ; 1000
               \9 [0 3]       ; 1001
               \a [0 2]       ; 1010
               \b [0 2 3]     ; 1011
               \c [0 1]       ; 1100
               \d [0 1 3]     ; 1101
               \e [0 1 2]     ; 1110
               \f [0 1 2 3]}) ; 1111

(defn count-hash-bits [hash-str]
  (->> hash-str (map #(count (set-bits %))) (reduce +)))

(defn row-strings [input-str]
  (map #(str input-str "-" %) (range)))

(defn grid-bits-set [input-str row-count]
  (->> (row-strings input-str)
       (take row-count)
       (map shared/knot-hash)
       (map count-hash-bits)
       (reduce +)))

(defn adjust-bit-indices [idx bits]
  (map #(+ (* idx 4) %) bits))

(defn hash-bit-indices [hash-str]
  (->> hash-str
       (map set-bits)
       (map-indexed adjust-bit-indices)
       (apply concat)))

(defn hash-row-coords [row hash-str]
  (map #(vector % row) (hash-bit-indices hash-str)))

(defn grid-coords-set [input-str row-count]
  (->> (row-strings input-str)
       (take row-count)
       (map shared/knot-hash)
       (map-indexed hash-row-coords)
       (apply concat)
       set))

(defn adjacent-coords [[x y]]
  [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]])

(defn valid-adjacent-coords [coords coord]
  (filter coords (adjacent-coords coord)))

(defn connected-coords
  ([coords start] (connected-coords coords #{start} #{}))
  ([coords to-visit visited]
    (if (empty? to-visit)
        visited
        (let [next-visit (first to-visit)
              adjacent (valid-adjacent-coords coords next-visit)
              additional-visits (shared/anti-filter (apply conj to-visit visited) adjacent)]
          (recur coords
                 (apply conj (disj to-visit next-visit) (set additional-visits))
                 (conj visited next-visit))))))

(defn count-connected-groups
  ([coords] (count-connected-groups coords 0))
  ([coords counted]
    (if (empty? coords)
        counted
        (let [coord (first coords)
              group-coords (connected-coords coords coord)
              coords-without-group (apply disj coords group-coords)]
          (recur coords-without-group (+ counted 1))))))

(defn part-one [] (grid-bits-set puzzle-input puzzle-rows))

(defn part-two [] (count-connected-groups (grid-coords-set puzzle-input puzzle-rows)))

(defn day []
  (println (part-one))
  (println)
  (println (part-two)))
