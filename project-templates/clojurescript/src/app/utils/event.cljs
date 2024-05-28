(ns app.utils.event)

(defn enter? [^js/KeyboardEvent event]
  (= 13 (.-keyCode event)))

(defn escape? [^js/KeyboardEvent event]
  (= 27 (.-keyCode event)))
