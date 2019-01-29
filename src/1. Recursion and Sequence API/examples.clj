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
