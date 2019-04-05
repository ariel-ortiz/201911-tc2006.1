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
  ([] true)
  ([x] x)
  ([x & y]
   `(let [t# ~x]
      (if t#
        ($and ~@y)
        t#))))

(defn split-start-end
  [lst start end]
  (->>
    lst
    (drop-while #(not= % start))
    rest
    (take-while #(not= % end))))

(defmacro IF
  "Provides a conditional statement that is
  syntactically a bit more similar to those
  found in languages like Pascal or Fortran.
  It has the following form:

    (IF condition
      :THEN exp1 exp2 ...
      :ELSE exp3 exp4 ...)"
  [condition & args]
  `(if ~condition
     (do ~@(split-start-end args :THEN :ELSE))
     (do ~@(split-start-end args :ELSE :THEN))))