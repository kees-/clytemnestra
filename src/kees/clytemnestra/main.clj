(ns kees.clytemnestra.main
  (:require [kees.clytemnestra.impl :as impl]))

(defn piet
  "Run a piet program from an image file."
  [{:keys [file codel-size verbose? limit]
    :or {codel-size 1 verbose? true limit 0}}]
  (when-not file
    (throw (Exception. "Didn't supply a file")))
  (let [canvas (impl/make-canvas file codel-size)
        result (impl/interpreter canvas verbose? limit)]
    (when verbose?
      (println "End state:" result))
    result))
