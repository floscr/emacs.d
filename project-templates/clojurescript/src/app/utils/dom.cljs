(ns app.utils.dom
  (:require
   [goog.dom.dataset :as goog.dataset]
   [camel-snake-kebab.core :refer [->camelCaseString]]
   [clojure.string :as str]))

(defn data-attr
  "Retrieves data attr keyword from element.

  Converts key to camelCase which is the default."
  [el key]
  (goog.dataset/get el (->camelCaseString key)))

(defn data-attr-id
  "Retrieves data attr keyword uuid from element.

  Converts key to camelCase which is the default.
  Returns nil when key is not available or not parseable as uuid."
  [el key]
  (try
    (-> (data-attr el key)
        (uuid))
    (catch js/Error. _ nil)))

(defn prevent-default
  "Prevent default on event.
  Returns event for composability."
  [event]
  (.preventDefault event)
  event)

(defn stop-propagation
  "Stop event bubbling.
  Returns event for composability."
  [event]
  (.stopPropagation event)
  event)

(defn left-click? [event]
  (= (.-button event) 0))

(defn right-click? [event]
  (= (.-button event) 2))

(defn event-form-data [event]
  (->> (.-target event)
       (js/FormData.)
       (js/Array.from)
       (js->clj)
       (map (fn [[k v]]
              (let [val (str/trimr v)]
                (when-not (empty? val)
                  [k v]))))
       (into {})))
