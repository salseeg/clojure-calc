(ns calc.core
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       Auxiliary
;
(defn traverse
  "Traverses code tree applying fn-map function according to operation"
  [fn-map tree]
  (if (list? tree)
    (let
      [ op   (first tree)
        tail (rest tree)
        args (map #(traverse fn-map %) tail)
        fun  ((keyword op) fn-map)]
      (fun args))
    ((:value fn-map)  tree)))

(defn unique-vars
  "Returns set of symbols used in code"
  [code]
  (set
   (traverse
    { :value #(if (symbol? %) [%] [])
;      :power flatten
;      :sqrt flatten
;      :abs flatten
      :+ flatten
      :- flatten
      :/ flatten
      :* flatten }
    code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Extra calculation functions
;
(defn abs
  [x]
  (if (> 0 x)
    (- x)
    x ))

(defn sqrt
  [x]
  (Math/sqrt x))

(defn power
  [x y]
  (Math/pow x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       Evaluate
;
(defn let-vector
  "Forms a vector for let statement"
  [vars-map]
  (into []
    (mapcat
      (fn
        [[key value]]
        [(symbol key) value])
      vars-map)))

(defn all-vars-defined?
  "Checks if all vars used in code are defined and vice versa"
  [vars code]
  (= (unique-vars code) (set (map symbol (keys vars)))))

(defn evaluate
  [vars code]
  (if (all-vars-defined? vars code)
    (eval (conj () code (let-vector vars) 'let))
    (throw (new Exception "Need all vars defined, as well as no unused vars"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;       Optimize
;
(defn optimize-add [args]
  (let [[a b] args]
    (cond
      (= a 0) b
      (= b 0) a
      :else (conj args '+))))

(defn optimize-sub [args]
  (let [[a b] args]
    (cond
      (= b 0) a
      (= b a) 0
      :else (conj args '-))))

(defn optimize-mul [args]
  (let [[a b] args]
    (cond
      (= a 1) b
      (= b 1) a
      (= b 0) 0
      (= a 0) 0
      :else (conj args '*))))

(defn optimize-div [args]
  (let [[a b] args]
    (cond
      (= b 1) a
      (= b a) 1
      (= a 0) 0
      :else (conj args '/))))

(defn optimize [list]
  (traverse
    { :value identity
      :/ optimize-div
      :* optimize-mul
      :- optimize-sub
      :+ optimize-add }
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;         To JavaScript
;
(defn js-2-arity
  "Convers 2 args operation into js syntax"
  [operation args]
  (let
    [[a b] args]
    (str "(" a " " operation " " b ")")))

(defn javascript-expression
  [code]
  (traverse
    { :value str
      :+ #(js-2-arity "+" %)
      :- #(js-2-arity "-" %)
      :/ #(js-2-arity "/" %)
      :* #(js-2-arity "*" %) }
    code))

(defn ->javascript
  [name list]
  (let
    [var-list   (string/join ", " (unique-vars list))
     expression (javascript-expression list)]
    (str "function " name "(" var-list ") { return " expression "; }")))
