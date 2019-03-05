(defn fibo
  [n]
  (if (< n 2)
    n
    (+ (fibo (- n 1))
       (fibo (- n 2)))))

;;; Sequential map
;(time (doall (map fibo (repeat 2 42))))

;;; Parallel pmap
;(time (doall (pmap fibo (repeat 2 42))))

(def num-rects 1000000000)
(def width (/ 1.0 num-rects))

(defn pi-sequential
  []
  (loop [sum 0.0
         i   0]
    (let [mid (* (+ i 0.5) width)
          height (/ 4.0 (+ 1.0 (* mid mid)))]
      (if (= i num-rects)
         (* width sum)
         (recur (+ sum height)
                (inc i))))))

(defn pi-parallel
  [[start end]]
  (loop [sum 0.0
         i   start]
    (let [mid (* (+ i 0.5) width)
          height (/ 4.0 (+ 1.0 (* mid mid)))]
      (if (= i end)
        (* width sum)
        (recur (+ sum height)
         (inc i))))))

(defn make-range
  [total chunks]
  (let [size (/ total chunks)]
    (->>
      (range 0 total size)
      (map #(list % (+ % size))))))

(println (time
           (pi-sequential)))

(println (time
           (reduce + (pmap pi-parallel
                           (make-range num-rects 8)))))

