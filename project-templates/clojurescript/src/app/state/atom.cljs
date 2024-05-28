(ns app.state.atom)

(defonce !dispatch (atom (fn [action payload]
                           (js/console.error "Dispatch not set" action payload))))

(defn dispatch [xs]
  (@!dispatch xs))

(defn dispatch-transition [xs]
  (cond
    js/document.startViewTransition (js/document.startViewTransition #(@!dispatch xs))
    :else (@!dispatch xs)))
