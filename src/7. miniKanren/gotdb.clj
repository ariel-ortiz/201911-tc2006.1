(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.pldb :as pldb])

(pldb/db-rel character name gender father mother)

;;; The following database contains Game of Thrones
;;; spoilers for season 6 and 7.
(def got-db
  (pldb/db
    ;;;        name      gender  father   mother
    [character :eddard   :male   :rickard :lyarra ]
    [character :brandon  :male   :rickard :lyarra ]
    [character :lyanna   :female :rickard :lyarra ]
    [character :benjen   :male   :rickard :lyarra ]
    [character :robb     :male   :eddard  :catelyn]
    [character :sansa    :female :eddard  :catelyn]
    [character :arya     :female :eddard  :catelyn]
    [character :bran     :male   :eddard  :catelyn]
    [character :rickon   :male   :eddard  :catelyn]
    [character :jon      :male   :rhaegar :lyanna ]
    [character :rhaegar  :male   :aerys   :rhaella]
    [character :elia     :female :aerys   :rhaella]
    [character :viserys  :male   :aerys   :rhaella]
    [character :daenerys :female :aerys   :rhaella]))
