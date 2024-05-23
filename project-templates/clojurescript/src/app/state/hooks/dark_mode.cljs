(ns app.state.hooks.dark-mode
  (:require
   [app.state.context :as state.context]
   [app.state.stores.ui :as stores.ui]
   [uix.core :as uix])
  (:refer-clojure :exclude [use]))

(defn use []
  (let [{:keys [state]} (state.context/use)
        dark-mode? (stores.ui/dark-mode? state)]
    (uix/use-layout-effect
     (fn []
       (if dark-mode?
         (js/document.documentElement.classList.add "dark")
         (js/document.documentElement.classList.remove "dark")))
     [dark-mode?])))
