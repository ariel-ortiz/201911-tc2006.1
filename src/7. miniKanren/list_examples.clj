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

(logic/defne reverseo
  "Logical function that succeeds if the reverse of
  lst is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (logic/fresh [temp]
     (logic/appendo temp [head] result)
     (reverseo tail temp))))

(logic/defne twino
             "Logical function that succeeds if lst is a sequence
             of two equal items."
  [lst]
  ([[x x]]))

(logic/defne anti-twino
  "Logical function that succeeds if lst is a sequence
  of two items that are not equal."
  [lst]
  ([[x y]]
   (logic/!= x y)))

(logic/defne listo
  "Logical function that succeeds if each element of lst
   is contained within its own list inside result."
  [lst result]
  ([[] []])
  ([[head . tail] [[head] . temp]]
   (listo tail temp)))
