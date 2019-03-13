; Implementation of the Metacircular Evaluator
; (a.k.a. Lisp interpreter in Clojure).

(import 'clojure.lang.IFn)

(declare $eval)

;;---------------------------------------------------------
;; Defines a Closure type that implements the IFn
;; interface which provides instances of the type the
;; ability to be called using the apply function:
;;
;;     (def c (->Closure {'a 5, '+ +} '(x) '(+ a x)))
;;     => #'user/c
;;
;;     (apply c '(10))
;;     => 15
;;
(deftype Closure
  [env params body]

  IFn

  (applyTo [self args]
    ($eval body
           (merge env (zipmap params args)))))

;;; Helper functions

;;---------------------------------------------------------
(defn third
  "Return the third element of lst."
  [lst]
  (nth lst 2))

;;---------------------------------------------------------
(defn fourth
  "Return the fourth element of lst."
  [lst]
  (nth lst 3))

;;---------------------------------------------------------
(defn $eval
  "Evaluates expr using the bindings contained in the env map."
  [expr env]

  (cond

    ; Variable references
    (symbol? expr)
    (if (contains? env expr)
      (env expr)
      (throw (RuntimeException.
               (str "Unbound variable: " expr))))

    ; Check for special forms
    (list? expr)
    (case (first expr)

      nil
      ()

      quote
      (second expr)

      if
      (if ($eval (second expr) env)
        ($eval (third expr) env)
        ($eval (fourth expr) env))

      lambda
      (->Closure env (second expr) (third expr))

      ; Ordinary function application
      (apply ($eval (first expr) env)
             (map #($eval % env) (rest expr))))

    ; Anything that is not a symbol or a list evaluates to itself.
    :else
    expr))
