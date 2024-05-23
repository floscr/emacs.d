(ns app.state.stores.auth
  (:require
   [app.utils.cookies :as cookies]))

;; Getters ---------------------------------------------------------------------

(defn get-auth-token [state]
  (get-in state [:auth :token]))

(defn set-auth-token [state token]
  (assoc-in state [:auth :token] token))

;; Fx --------------------------------------------------------------------------

(defn authorize-from-cookie! [state]
  (if-let [token (some-> (cookies/parse-cookie)
                         (last)
                         :value)]
    (assoc state :token token)
    (assoc state :error {:kind :no-cookie-auth
                         :msg "Couldn't authorize."})))

;; Initial ---------------------------------------------------------------------

(def initial {})
