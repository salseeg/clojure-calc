(ns calc.core-test
  (:require [clojure.test :refer :all]
            [calc.core :refer :all]))

(deftest eval-test
  (testing "Simple eval"
    (is (= 1 (evaluate {} '1)))
    (is (= 2 (evaluate {} '(+ 1 1))))
    (is (= 3 (evaluate {} '(- 4 1))))
    (is (= 7 (evaluate {} '(/ 14 2))))
    (is (= 56 (evaluate {} '(* 7 8))))
    (is (= 23 (abs -23)))
    (is (= 23 (evaluate {} '(abs -23))))
    (is (= 23 (evaluate {} '(abs 23))))
    (is (= 5.0 (evaluate {} '(sqrt 25))))
    (is (= 625.0 (evaluate {} '(power 5 4))))
 )

  (testing "Vars using eval"
    (is (= 1 (evaluate {:x 1} 'x)))
    (is (= 2 (evaluate {:x 1} '(+ x 1))))
    (is (= 3 (evaluate {:y 1} '(- 4 y))))
    (is (= 7 (evaluate {:x 14 :y 2} '(/ x y))))
    (is (= 56 (evaluate {:x 7 :y 8} '(* x y)))))
    (is (= 5.0 (evaluate {:x 25} '(sqrt x))))
)

(deftest optimize-test
  (testing "Calculation tree optimization"
    (is (= 'x (optimize 'x)))

    (is (= 'x (optimize '(+ x 0))))
    (is (= 'x (optimize '(+ 0 x))))
    (is (=  0 (optimize '(+ 0 0))))

    (is (= 'x (optimize '(- x 0))))
    (is (=  0 (optimize '(- x x))))

    (is (= 'x (optimize '(* x 1))))
    (is (= 'x (optimize '(* 1 x))))
    (is (=  1 (optimize '(* 1 1))))

    (is (= 'x (optimize '(/ x 1))))
    (is (=  1 (optimize '(/ x x))))

    (is (= '(/ x y) (optimize '(/ x (* y (/ z z))))))
    (is (= '(/ x 0) (optimize '(/ x (* y (- z z))))))

    (is (= 10 (optimize '(+ 10 (* x 0)))))
    (is (= '(+ x y) (optimize '(+ x (- y 0)))))

    (is (= 'x (optimize '(power x 1))))
    (is (=  1 (optimize '(power x 0))))
    (is (=  1 (optimize '(power 1 x))))
    (is (=  0 (optimize '(power 0 x))))

    (is (=  0 (optimize '(sqrt 0))))
    (is (=  1 (optimize '(sqrt 1))))

    (is (= '(abs x) (optimize '(abs x))))
  ))

(deftest to-javascript-test
  (testing "js code  generation"
    (is (= "function example(x) { return (1 + (x * x)); }" (->javascript "example" '(+ 1 (* x x)))))
    (is (= "function example(x, y) { return (1 + (x * y)); }" (->javascript "example" '(+ 1 (* x y)))))
    (is (= "function example(x, y, z) { return (z + (x * y)); }" (->javascript "example" '(+ z (* x y)))))
    (is (=
         "function example(x, y, z) { return (z + (x * Math.sqrt(Math.abs(y)))); }"
         (->javascript "example" '(+ z (* x (sqrt(abs y)) )) )))
))