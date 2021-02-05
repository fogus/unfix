(ns joy.unfix.postfix-test
  (:use [clojure.test :only [deftest is]])
  (:use [joy.unfix.postfix :only [postfix]]))

(deftest test-postfix
  (is (= [14] (postfix 5 1 2 + 4 * + 3 -)))
  (is (= [9]  (postfix 3 2 1 + *)))
  (is (= [7]  (postfix 1 2 3 * +))))
