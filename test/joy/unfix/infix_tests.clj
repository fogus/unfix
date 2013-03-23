(ns joy.unfix.infix-tests
  (:require [clojure.test :as test])
  (:use joy.unfix.infix))

(defn- all-eq? [a b c] 
  (infix 
    a = b && 
    b = c && 
    a = c))

(test/deftest simple-infix
  (test/is (= 3
              (infix 21 / [1 + 2 * 3])))
  (test/is (= 9
              (infix [1 + 2] * 3)))
  (test/is (= 7
              (infix 1 + 2 * 3))))

(test/deftest compound-infix
  (test/is (all-eq? 3 3 3))
  (test/is (not (all-eq? 1 2 3))))

(test/deftest test-infix-reader
  (test/is (= '(+ 2 (* 3 4))
              (read-string "#joy/infix [2 + 3 * 4]"))))
