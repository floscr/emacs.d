(ns app.core
  (:require
   [app.styles]
   [promesa.core]
   [uix.core :as uix :refer [$ defui]]
   [app.components.main.views :as main]
   [uix.dom]))

(defui app []
  ($ main/provider))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))
