;==========================================================
; Solution.
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================
(defn mode
  "Takes a sequence lst of unordered numbers as its input
  and returns its mode. If there is a tie between two or 
  more of the most repeated numbers, the function returns 
  a vector with all these numbers in ascending order. If 
  lst is an empty sequence, it returns nil."
  [lst]
  (if (empty? lst)
    nil
    (let [sorted (sort-by (comp - count)
                          (partition-by identity (sort lst)))
          size (count (first sorted))
          result (->> sorted
                      (take-while #(= size (count %)))
                      (map first)
                      vec)]
      (if (= 1 (count result))
        (first result)
        result))))

;==========================================================
(deftest test-mode
  (is (nil? (mode [])))
  (is (= 2
         (mode [2])))
  (is (= 3
         (mode [3 2 3])))
  (is (= [2 3]
         (mode [2 3 3 1 2 2 3])))
  (is (= [1 2 3 4 5 6 7 8 9 10 11]
         (mode [8 10 5 6 3 2 11 4 7 9 1])))
  (is (= 31
         (mode [64 1 31 69 47 87 68 71 29 49
                57 18 8 31 65 83 11 42 79 55])))
  (is (= 40
         (mode [68 48 49 64 61 72 68 45 50 98
                61 21 81 22 11 99 43 84 38 29
                73 72 76 89 40 40 46 80 86 33
                80 12 68 87 96 89 82 40 21 26
                40 28 38 4 20 63 16 40 71 28
                20 35 22 52 4 19 95 78 1 86
                66 4 53 17 13 40 47 1 69 49
                68 26 82 82 17 48 70 10 84 8
                58 59 31 43 20 79 93 47 51 48
                58 32 34 11 80 30 3 96 85 80])))
  (is (= [1 29 73]
         (mode [92 8 44 1 26 3 47 65 76 26
                29 95 27 27 80 3 46 36 64 23
                70 42 49 86 88 56 63 62 46 30
                97 99 100 72 31 26 94 29 97 85
                73 52 19 96 2 85 100 85 45 100
                35 29 29 43 70 21 74 52 37 31
                73 80 1 56 90 81 44 84 35 42
                19 9 45 84 99 96 83 73 25 81
                21 69 39 93 16 66 7 97 64 83
                73 61 63 4 92 1 1 52 41 8
                38]))))

;==========================================================
(run-tests)

