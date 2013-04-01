(ns joy.unfix.infix)

(def && #(and   %1 %2))
(def || #(or    %1 %2))
(def != #(not=  %1 %2))

(def ^:dynamic *ops* '[- + * / < > && || = !=])
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


(defn- fix
  [[a b & [c d e & m]]]
  (cond
   (vector? a) (recur (list* (fix a) b c d e m))
   (vector? c) (recur (list* a b (fix c) d e m))
   (ifn? b) (recur (list* (b a c) d e m))
   (recur (list a b (fix (list* c d e m))))
   :else a))

(defn fix
  ([x op y] (op x y))
  ([x op1 y op2 z] (op1 x (op2 y z))))  ;; r->l like apl

;; l->r like smalltalk

;; math-like precedence

(comment

  (fix 1 + 2)

  (fix 1 + 2 + 3)
  (fix 1 + 2 * 3)
  (fix 1 * 2 + 3)
)