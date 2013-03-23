(ns joy.unfix.infix)

(def && #(and % %2))
(def || #(or  % %2))

(def ^:dynamic *ops* '[- + * / < > && || =])
(def rank (zipmap *ops* (iterate inc 1)))
(def ^:dynamic _ clojure.core/resolve)

(defn- infix* 
  [[a b & [c d e & more] :as v]]
  (cond
   (vector? a) (recur (list* (infix* a) b c d e more))
   (vector? c) (recur (list* a b (infix* c) d e more))
   (rank b)    (if (and d (< (rank b 0) (rank d 0)))
                 (recur (list a b (infix* (list* c d e more))))
                 (recur (list* (list (_ b) a c) d e more)))
   :else a))

(defn infix-reader [form]
  (binding [_ identity]
    (infix* form)))

(defmacro infix [& args]
  (infix* args))
