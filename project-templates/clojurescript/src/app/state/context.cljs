(ns app.state.context
  (:require
   [app.repl :as repl]
   [app.state.atom :as state.atom]
   [app.state.middleware.core :as middleware]
   [app.state.middleware.middlewares.logger :as middlewares.logger]
   [app.utils.dev :refer [fn-name]]
   [app.utils.env :as env]
   [uix.core :as uix :refer [$ defui]]
   [uix.dom])
  (:refer-clojure :exclude [use]))

;; Initial State ---------------------------------------------------------------

(defn initial []
  {})

;; Middleware ------------------------------------------------------------------

(defonce silence-busy-actions-in-logger
  #{})

(defonce middleware
  (concat
   []
   (when env/debug?
     [(update middlewares.logger/middleware :update (fn [update-fn]
                                                      (fn [{:keys [action] :as params} prev-state state]
                                                        (when-not (silence-busy-actions-in-logger action)
                                                          (update-fn params prev-state state)))))])))

;; Reducer ---------------------------------------------------------------------

(defonce context (uix/create-context (initial)))

(defonce context-provider (.-Provider context))

(defonce no-persist {:get (constantly nil)
                     :set identity})

(defn reducer [state [action payload] & {:keys [middleware]}]
  (if-let [f (if (fn? action)
               action
               (get {} action))]
    (let [{updated-state :state
           :as no-map-updated-state} (f state payload)]
      (if (map? no-map-updated-state)
        (middleware/apply-middleware! {:action action
                                       :payload payload}
                                      state (or updated-state no-map-updated-state)
                                      middleware)
        (do (js/console.error "Action did not return any state" (keyword (fn-name action)))
            state)))
    (do
      (js/console.error (str "No action found " action) payload)
      state)))

(defui provider [{:keys [children middleware]
                  :or {middleware []}}]
  (let [initial-state (or (middleware/get-middleware middleware)
                          (initial))
        [state dispatch] (uix/use-reducer
                          (fn [acc cur]
                            (reducer acc cur {:middleware middleware}))
                          initial-state)]
    (reset! state.atom/!dispatch dispatch)
    (repl/update-state-atom! state dispatch)
    ($ context-provider {:value {:state state
                                 :dispatch dispatch}}
       children)))

(defn use []
  (if-let [ctx (uix/use-context context)]
    ctx
    (throw (js/Error. "useMyContext must be used within a MyProvider"))))
