(defn duplicate1
  "Returns a list composed of the same elements as lst but
  each appearing twice. Uses recursion."
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (duplicate1 (rest lst))))))

(defn duplicate2
  "Returns a list composed of the same elements as lst but
  each appearing twice. Uses loop/recur."
  [lst]
  (loop [lst    lst
         result ()]
    (if (empty? lst)
      (reverse result)
      (recur (rest lst)
             (cons (first lst)
                   (cons (first lst)
                         result))))))
(defn duplicate3
  "Returns a list composed of the same elements as lst but
  each appearing twice. Uses the sequence API."
  [lst]
  (mapcat #(list % %) lst))

(defn fib1
  "Returns the n-th element of the Fibonacci sequence.
  Assumes that n is an integer greator or equal to zero.
  Uses recursion."
  [n]
  (if (< n 2)
    n
    (+' (fib1 (- n 1))
        (fib1 (- n 2)))))

(defn fib2
  "Returns the n-th element of the Fibonacci sequence.
  Assumes that n is an integer greator or equal to zero.
  Uses loop/recur."
  [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b
             (+' a b)
             (inc i)))))

(defn fib3
  "Returns the n-th element of the Fibonacci sequence.
  Assumes that n is an integer greator or equal to zero.
  Uses the sequence API."
  [n]
  ;;; To be defined
  nil)
