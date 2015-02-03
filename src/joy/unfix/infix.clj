(ns joy.unfix.infix)

(def && #(and   %1 %2))
(def || #(or    %1 %2))
(def != #(not=  %1 %2))

(def ^:dynamic *ops* '[|| && < > = != + - * /])
(def rank (zipmap *ops* (iterate inc 1)))
(def ^:dynamic _ clojure.core/resolve)

(defn- infix* 
  [[a b c & [d e & more] :as v]]
  ;(println "DEBUG: " v)
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


;; r->l like apl

(defn r->lfix
  ([a op b]              (op a b))
  ([a op1 b op2 c]       (op1 a (op2 b c)))
  ([a op1 b op2 c op3 d] (op1 a (op2 b (op3 c d)))))

(r->lfix 1 + 2)
;;=> 3

(r->lfix 1 + 2 + 3)
;;=> 6

(r->lfix 1 + 2 * 3)
;;=> 7

(r->lfix 10 * 2 + 3)
;;=> 50 ( 23 in js )


;; l->r like smalltalk

(defn l->rfix
  ([a op b]              (op a b))
  ([a op1 b op2 c]       (op2 c (op1 a b)))
  ([a op1 b op2 c op3 d] (op3 d (op2 c (op1 a b)))))

(l->rfix 1 + 2 + 3)
;;=> 6

(l->rfix 1 + 2 * 3)
;;=> 9 ( 7 in JS )

;; math-like precedence

(def order {+ 0   - 0
            * 1   / 1})

(defn infix3
  [a op1 b op2 c]
  (if (< (get order op1) (get order op2))
    (r->lfix a op1 b op2 c)
    (l->rfix a op1 b op2 c)))

(infix3 1 + 2 * 3)
;;=> 7

(infix3 10 * 2 + 3)
;;=> 23


