(ns app.utils.classlist
  (:refer-clojure :exclude [remove]))

(defn add [cls]
  (js/document.documentElement.classList.add cls))

(defn remove [cls]
  (js/document.documentElement.classList.remove cls))

(defn toggle [cls]
  (js/document.documentElement.classList.remove cls))
