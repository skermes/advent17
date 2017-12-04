(ns advent17.shared)

(defn firstp [pred [head & tail]]
  (cond (nil? head) nil
        (pred head) head
        :else (firstp pred tail)))
