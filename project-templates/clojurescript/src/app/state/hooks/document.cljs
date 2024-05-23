(ns app.state.hooks.document
  (:require
   [app.state.context :as state.context]
   [app.state.fx.document :as fx.document]
   [app.state.stores.documents :as stores.documents]
   [app.state.stores.route :as stores.route]
   [lentes.core :as l]
   [uix.core :as uix])
  (:refer-clojure :exclude [use]))

(defn use [route-path]
  (let [{:keys [state]} (state.context/use)
        auth-token (get-in state [:auth :token])
        route-path (or route-path (stores.documents/location->file! (l/focus stores.route/path-lens state)))
        doc (stores.documents/route-document state)]
        ;; doc (stores.documents/document-with-transactions state "Main/shoppinglist.org")

    (uix/use-effect
     (fn []
       (when-not doc
         (fx.document/add-document+ route-path auth-token)))
     [auth-token route-path doc])
    doc))
