
;;; Helper functions
(defn third
  [lst]
  (nth lst 2))

(defn fourth
  [lst]
  (nth lst 3))

(defn $eval
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
      (second expr))))




