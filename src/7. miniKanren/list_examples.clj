(require '[clojure.core.logic :as logic])

(logic/defne lasto
  "Logical function that succeeds if the last element
  of lst is x."
  [lst x]
  ([[x] x])
  ([[h . t] x]
   (lasto t x)))

(logic/defne dupo
  "Logical function that succeeds if every element of
  lst is duplicates in result."
  [lst result]
  ([[] []])
  ([[head . tail] [head head . temp]]
   (dupo tail temp)))
