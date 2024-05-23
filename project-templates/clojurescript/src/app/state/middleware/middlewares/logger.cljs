(ns app.state.middleware.middlewares.logger
  (:require
   [clojure.data :refer [diff]]
   [org-mode.dev :refer [fn-name]]))

(defn log-state [{:keys [payload action]} prev-state state]
  (when (not= prev-state state)
    (js/console.log
     "%cState update"  "font-weight:bold;" (keyword (fn-name action))
     "\n\npayload:" payload
     "\nprev:" prev-state
     "\nnext:" state
     "\ndiff:" (let [[prev-diff next-diff _] (diff prev-state state)]
                 (if (and (not prev-diff) (not next-diff))
                   ":same-state"
                   [prev-diff next-diff])))))

(def middleware
  {:update log-state})
