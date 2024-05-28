(ns app.utils.window
  (:require
   [clojure.string :as str]))

(defn location-path
  "Returns the window location in a vector with paths segments split into a list."
  ([] (location-path js/window.location.pathname))
  ([path]
   (->> (str/split path #"/")
        (rest)
        (into []))))
