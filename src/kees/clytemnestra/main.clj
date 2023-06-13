(ns kees.clytemnestra.main
  (:require [kees.clytemnestra.impl :as impl]))

(defn piet
  "Run a piet program from an  image file."
  [{:keys [file codel-size verbose?]
    :or {codel-size 1 verbose? true}}]
  (let [canvas (impl/make-canvas file codel-size)
        result (impl/interpreter canvas verbose?)]
    (println result)
    result))
