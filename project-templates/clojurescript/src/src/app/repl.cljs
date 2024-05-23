(ns app.repl
  (:require))

(defonce !state (atom []))
(defonce !dispatch (atom nil))

(defn update-state-atom! [state dispatch]
  (reset! !state state)
  #_
  (when-not (= (last @!state) state)
    (reset! !state (conj @!state state)))
  (reset! !dispatch dispatch))

(defn cur-state []
  (last @!state))

(defn pop-state! []
  (when-let [s (try
                 (some-> @!state
                         pop)
                 (catch js/Error _ nil))]
    (reset! !state s)
    (@!dispatch [:state/set (last s)])))

(comment
  @!state
  (dissoc @!state :documents)
  (last @!state)
  (pop-state!))
