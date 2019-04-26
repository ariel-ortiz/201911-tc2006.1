(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

(logic/defne sumo
  "Logical function that succeeds if the sum of all
  elements in lst is equal to result."
  [lst result]
  ([[] 0])
  ([[head . tail] result]
   (logic/fresh [temp]
     (sumo tail temp)
     (fd/+ head temp result))))

(logic/defne largesto
  "Logical function that succeeds if the largest element
  in lst is equal to result."
  [lst result]
  ([[x] x])
  ([[head . tail] head]
   (logic/fresh [temp]
     (largesto tail temp)
     (fd/> head temp)))
  ([[head . tail] temp]
   (largesto tail temp)
   (fd/>= temp head)))