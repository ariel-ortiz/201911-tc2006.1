;==========================================================
; Problem 1
; do special form
;
; Type the authors' student IDs and names here.
;==========================================================

; Implementation of the Metacircular Evaluator
; (a.k.a. Lisp interpreter in Clojure).

(require '[clojure.test :refer [deftest is run-tests]])
(import 'clojure.lang.IFn)

(declare $eval)

;;---------------------------------------------------------
;; Defines a Closure type that implements the IFn
;; interface which provides instances of the type the
;; ability to be called using the apply function:
;;
;;     (def c (->Closure (atom {'a 5, '+ +}) '(x) '(+ a x)))
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
           (merge @env (zipmap params args)))))

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
  "Evaluates expr using the bindings contained in the env
  map."
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
      (->Closure (atom env) (second expr) (third expr))

      label
      (let [closure ($eval (third expr) env)]
        (swap! (.env closure)
               #(assoc % (second expr) closure))
        closure)      

      ; Ordinary function application
      (apply ($eval (first expr) env)
             (map #($eval % env) (rest expr))))

    ; Anything that is not a symbol or a list $evals to itself.
    :else
    expr))

;==========================================================
(deftest test-var-ref
  (is (= 15 ($eval 'c
                   {'a 4, 'b 8, 'c 15})))
  (is (thrown? RuntimeException
        ($eval 'x
               {'a 4, 'b 8, 'c 15}))))

;==========================================================
(deftest test-itself
  (is (= 42 ($eval 42 {})))
  (is (= true ($eval true {})))
  (is (= false ($eval false {})))
  (is (= nil ($eval nil {})))
  (is (= "hello" ($eval "hello" {}))))

;==========================================================
(deftest test-empty-list
  (is (= () ($eval () {}))))

;==========================================================
(deftest test-quote
  (is (= 'a
        ($eval '(quote a) {})))
  (is (= '(1 2 3)
        ($eval '(quote (1 2 3)) {})))
  (is (= '42 ($eval '(quote 42) {}))))

;==========================================================
(deftest test-if
  (is (= 1 ($eval '(if true 1 2) {})))
  (is (= 2 ($eval '(if false 1 2) {}))))

;==========================================================
(deftest test-function-invocation
  (is (= 3
        ($eval '(f 1 2)
         {'f +})))
  (is (= 'a
        ($eval '(g (quote (a b c d e)))
         {'g first})))
  (is (= '(a b c)
        ($eval '(cons x y)
         {'cons cons, 'x 'a, 'y '(b c)})))
  (is (= 55
        ($eval '(+ 1 2 3 4 5 6 7 8 9 10)
         {'+ +})))
  (is (= '(a b c)
        ($eval '(apply cons (quote (a (b c))))
         {'apply apply, 'cons cons}))))

;==========================================================
(deftest test-lambda
  (let [c ($eval '(lambda (x)
                    (* x 2))
            {'* *})]
    (is (instance? Closure c))
    (is (= @(.env c) {'* *}))
    (is (= (.params c) '(x)))
    (is (= (.body c) '(* x 2)))
    (is (= 42 (apply c '(21)))))
  (is (= 8
        ($eval '((lambda (f x) (f (f (f x))))
                 (lambda (x) (* x 2))
                 1)
         {'* *}))))

;==========================================================
(deftest test-label
  (is (= '(a a b b c c)
        ($eval
          '((label dup (lambda (lst)
                         (if (eq lst ())
                           ()
                           (cons (car lst)
                                 (cons (car lst)
                                       (dup (cdr lst)))))))
            (quote (a b c)))
          {'eq   =
           'cons cons
           'car  first
           'cdr  rest})))
  (is (= '(1 4 9 16)
        ($eval
          '((label mapcar (lambda (fun lst)
                            (if (eq lst ())
                              ()
                              (cons (fun (car lst))
                                    (mapcar fun (cdr lst))))))
            (lambda (x) (* x x))
            (quote (1 2 3 4)))
          {'eq   =
           'cons cons
           'car  first
           'cdr  rest
           '*    *}))))

;==========================================================
(deftest test-do
  (is (nil? ($eval '(do)
                   {})))
  (is (= 3
         ($eval '(do (+ 1 2))
                {'+ +})))
  (is (= 10
         ($eval '(do (+ 2 5)
                     (- 2 5)
                     (/ 2 5)
                     (* 2 5))
                {'+ +
                 '- -
                 '/ /
                 '* *})))
  (is (= "7-32/510"
         (with-out-str
           ($eval '(do (pr (+ 2 5))
                       (pr (- 2 5))
                       (pr (/ 2 5))
                       (pr (* 2 5)))
                  {'pr pr
                   '+ +
                   '- -
                   '/ /
                   '* *}))))
  (is (= (let [nl (System/lineSeparator)]
           (str "1" nl "2" nl "3" nl))
         (with-out-str   
           ($eval '(do (prn (+ -2 (+ 1 2)))
                       (prn (+ 1 1))
                       (prn 3)
                       (+ 2 2))
                  {'+ +
                   'prn prn}))))
  (is (= (let [nl (System/lineSeparator)]
           (str "One potato," nl
                "two potatoes," nl
                "three potatoes," nl
                "four." nl))
         (with-out-str
           ($eval '(do (println "One potato,")
                       (println "two potatoes,")
                       (println "three potatoes,")
                       (println "four."))
                  {'println println})))))

;==========================================================
(run-tests)
