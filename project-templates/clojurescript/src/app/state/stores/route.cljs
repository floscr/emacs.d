(ns app.state.stores.route
  (:require
   [app.state.atom :as state.atom]
   [app.state.stores.ui :as stores.ui]
   [lentes.core :as l]
   [uix.core :as uix]))

;; Lenses ----------------------------------------------------------------------

(def path-lens (l/in [:route :path]))

;; Fx --------------------------------------------------------------------------

(defn history-push! [title path]
  (js/window.history.pushState nil (or title js/document.title) path) #js { :push false})

;; Setters ---------------------------------------------------------------------

(defn update-state [state {:keys [path]}]
  (->> state
       (l/put path-lens path)
       (stores.ui/close-sidebar)))

(defn push! [state {:keys [title path] :as payload}]
  (history-push! title path)
  (update-state state payload))

(defn use-location []
  (uix/use-layout-effect
   (fn []
     (let [listener (js/window.addEventListener
                     "popstate"
                     #(state.atom/dispatch [update-state {:path js/window.location.pathname}]))]
       #(js/window.removeEventListener "popstate" listener)))
   []))

;; Initial ---------------------------------------------------------------------

(defonce initial {:route {:path js/window.location.pathname}})
