(ns joy.unfix.infix)

(def && #(and % %2))
(def || #(or  % %2))

(def ^:dynamic *ops* '[- + * / < > && || =])
(def rank (zipmap *ops* (iterate inc 1)))
(def op? rank)
    
(defn- infix* 
  [[a b & [c d e & more]]]
  (cond
   (vector? a) (recur (list* (infix* a) b c d e more))
   (vector? c) (recur (list* a b (infix* c) d e more))
   (op? b)     (if (and d (< (rank b 0) (rank d 0)))
                 (recur (list a b (infix* (list* c d e more))))
                 (recur (list* (list b a c) d e more)))
   :else a))
    
(defn infix [& args]
  (infix* args))
