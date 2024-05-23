(ns app.utils.dev
  (:require
   [clojure.string :as str]))

;; https://stackoverflow.com/a/46586391
(defn fn-name
  "Print the name for a function that was dynamically passed."
  [f]
  (as-> (.-name f) $
    (demunge $)
    (str/split $ #"/")
    ((juxt butlast last) $)
    (update $ 0 #(str/join "." %))
    (str/join "/" $)))
