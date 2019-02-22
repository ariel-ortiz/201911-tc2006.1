;;; Class Exercise: Symbolic Differentiation
;;; Full description at:
;;;    http://34.212.143.74/s201911/tc2006/notes/symbolic_differentiation.html

(declare variable?
         same-variable?
         sum?
         augend
         addend
         make-sum
         product?
         multiplicand
         multiplier
         make-product)

(defn deriv
  "Derives exp with respect to var."
  [exp var]
  (cond

    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var)
      1
      0)

    (sum? exp)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var))))

(defn variable?
  "Is exp a variable?"
  [exp]
  (symbol? exp))

(defn same-variable?
  "Are v1 and v2 the same variable?"
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn sum?
  "Is exp a sum?"
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '+ (first exp))))

(defn augend
  "Returns the augend of a sum."
  [exp]
  (nth exp 1))

(defn addend
  "Returns the addend of a sum."
  [exp]
  (nth exp 2))

(defn make-sum
  "Construct the sum of a1 and a2."
  [a1 a2]
  (list '+ a1 a2))
