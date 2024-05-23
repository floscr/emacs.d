(ns app.state.stores.ui
  (:require
   [lentes.core :as l]))

;; Variables -------------------------------------------------------------------

(def command-prompt-dialog-key ::comand-prompt)

(def node-properties-dialog-key ::node-properties)

;; Lenses ----------------------------------------------------------------------

(def dialog-open-lens [:ui :dialog])

(def sidebar-open-lens (l/in [:ui :sidebar/open?]))

(def dark-mode-lens (l/in [:ui :dark-mode?]))

;; Setters ---------------------------------------------------------------------

(defn toggle-sidebar-open? [state]
  (l/over sidebar-open-lens not state))

(defn close-sidebar [state]
  (l/over sidebar-open-lens (constantly false) state))

(defn toggle-dark-mode [state]
  (l/over dark-mode-lens not state))

(defn open-dialog [state {:keys [id meta]}]
  (assoc-in state dialog-open-lens {:id id :meta meta}))

(defn close-dialog [state]
  (assoc-in state dialog-open-lens nil))

;; Getters ---------------------------------------------------------------------

(defn sidebar-open? [state]
  (l/focus sidebar-open-lens state))

(defn dark-mode? [state]
  (l/focus dark-mode-lens state))

(defn dialog-open? [state]
  (get-in state (conj dialog-open-lens)))

;; Initial ---------------------------------------------------------------------

(def initial {:ui {:sidebar/open? false
                   :dark-mode? false
                   :dialog nil}})
