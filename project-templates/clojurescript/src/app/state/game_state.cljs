(ns app.state.game-state
  (:refer-clojure :exclude [next]))

;; Key -------------------------------------------------------------------------

(def k :game-state)

(def title :title)
(def tutorial :tutorial)
(def playing :playing)
(def paused :paused)
(def game-over :game-over)

;; Getters ---------------------------------------------------------------------

(defn game-over? [state]
  (= (k state) game-over))

(defn paused? [state]
  (= (k state) paused))

;; Main ------------------------------------------------------------------------

(def initial [tutorial :tile-selection])
