(defmacro $comment
  "Replaces (comments out) the x with nil."
  [x]
  nil)

(defmacro debug
  "Prints debug information."
  [x]
  `(let [t# ~x]
     (printf "Debug %s => %s%n" (quote ~x) t#)
     t#))

(defn fact
  "Recursive version of factorial of n used to
  demonstrate the debug macro."
  [n]
  (if (zero? n)
    1
    (do (debug n)
        (debug (* n (fact (dec n)))))))

(defmacro $and
  "Returns true if x and y are true, false
  otherwise. Uses short circuit evaluation."
  [x y]
  `(if ~x
     ~y
     false))
