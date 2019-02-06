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
  (first
    (nth
      (iterate
        (fn [[a b]]
          [b (+' a b)])
        [0 1])
      n)))

(defn insert
  "Returns a list with the same elements as lst
  (which must be sorted) but inserting n in its
  corresponding place."
  [n lst]
  (let [[a b] (split-with #(> n %) lst)]
    (concat a (list n) b)))

(defn my-sort
  "Sorts the elements of lst in ascending order.
  Uses the insertion sort algorithm."
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)
      result
      (recur (rest lst)
             (insert (first lst) result)))))

(defn qsort
  "Sorts the elements of lst in ascending order.
  Uses the quick sort algorithm."
  [lst]
  (if (empty? lst)
    ()
    (let [fst     (first lst)
          rst     (rest lst)
          smaller (filter #(< % fst) rst)
          bigger  (remove #(< % fst) rst)]
      (concat (qsort smaller)
              (list fst)
              (qsort bigger)))))

(defn primer-factors
  "Returns a list containing the prime factors
  of n in ascending order. Assumes that n > 0."
  [n]
  (loop [q      n
         d      2
         result ()]
    (if (> d q)
      (reverse result)
      (if (zero? (rem q d))
        (recur (quot q d)
               d
               (cons d result))
        (recur q
               (inc d)
               result)))))
