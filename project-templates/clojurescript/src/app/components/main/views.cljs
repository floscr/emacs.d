(ns app.components.main.views
  (:require
   [app.state.context :as state.context]
   [uix.core :as uix :refer [$ defui]]))

(defui main [_]
  ($ :p "Hello world"))

(defui provider []
  ($ state.context/provider {:middleware state.context/middleware}
     ($ main)))
