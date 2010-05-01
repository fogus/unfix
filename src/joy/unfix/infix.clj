(ns joy.unfix.infix)

(def AND #(and %1 %2))
(def *rank* (zipmap [- + * / AND =] (iterate inc 1))) ;; Define order of precedence
    
(defn infix* [[a b & [c d e & more]]]
  (cond
   (vector? a) (recur (list* (infix* a) b c d e more)) ;; Vector on the LHS
   (vector? c) (recur (list* a b (infix* c) d e more)) ;; Vector on the RHS
   (ifn? b)    (if (and d (< (*rank* b 0) (*rank* d 0)))
                 (recur (list a b (infix* (list* c d e more)))) ;; Left-association
                 (recur (list* (b a c) d e more))) ;; Right-association
   :else a)) ;; Return the left-most element
    
(defn infix [& args]
  (infix* args))
