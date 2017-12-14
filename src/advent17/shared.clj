(ns advent17.shared)

(defn firstp [pred [head & tail]]
  (cond (nil? head) nil
        (pred head) head
        :else (recur pred tail)))

(defn anti-filter [pred coll]
  (filter #(not (pred %)) coll))

(defn any [coll]
  (reduce #(or %1 %2) coll))
