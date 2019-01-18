(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn f2c
  "Takes x degrees Fahrenheit and converts
  them to degrees Celsius."
  [x]
  (/ (* (- x 32.0)
        5)
     9))

(defn sign
  "Returns -1 if n is negative, 1 if n is positive
  greater than zero, or 0 if n is zero."
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(run-tests)
