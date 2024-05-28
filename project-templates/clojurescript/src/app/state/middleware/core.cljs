(ns app.state.middleware.core)

(defn apply-middleware!
  "Apply (side-effectful) middleware to the state in `middleware`
  A middleware fn would look like this:
  (middleware `prev-state` `next-state`)
  When a middleware fn returns `nil` (just a side-effect) we keep the accumulated state."
  [args prev-state next-state middlewares]
  (reduce
   (fn [state-acc {:keys [update]}]
     (or (update args prev-state state-acc) state-acc))
   next-state middlewares))

(defn get-middleware [middleware]
  (when-let [get-fn (->> middleware
                         (filter :get)
                         (first)
                         :get)]
    (get-fn)))
