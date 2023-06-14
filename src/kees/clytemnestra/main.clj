(ns kees.clytemnestra.main
  (:require [kees.clytemnestra.impl :as impl]
            [kees.clytemnestra.helpers :as helpers]))

(defn sanitize-state
  "Prepare final state for printing end result."
  [state]
  (-> state
      (update :output #(reduce str (reverse %)))
      (dissoc :value :terminate? :block)))

(defn piet
  "Run a piet program from an image file."
  [{:keys [file codel-size verbose? limit]
    :or {codel-size 1 verbose? true limit 0}}]
  (when-not file
    (throw (Exception. "Didn't supply a file")))
  (let [canvas (impl/make-canvas file codel-size)
        result (sanitize-state (impl/interpreter canvas verbose? limit))]
    (when verbose?
      (println (helpers/canvas->term-color canvas))
      (println "End state:" result))
    (println (:output result))
    result))

(defn print-program
  "Print the program as truecolor block to terminal."
  [{:keys [file codel-size]}]
  (let [canvas (impl/make-canvas file codel-size)]
    (println (helpers/canvas->term-color canvas))))
