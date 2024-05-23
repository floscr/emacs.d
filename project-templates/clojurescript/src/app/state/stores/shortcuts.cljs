(ns app.state.stores.shortcuts
  (:require
   ["mousetrap" :as mousetrap]
   [app.state.atom :refer [dispatch]]
   [app.state.stores.documents :as stores.documents]
   [app.utils.dom :refer [data-attr data-attr-id]]
   [uix.core :as uix]
   [app.state.stores.ui :as stores.ui]
   [app.utils.dom :as dom]))

;; Lenses---------------------------------------------------------------------

(defonce layer-key ::layer)

(defonce before-popover-key ::layer-before-popover)

(defonce before-dialog-key ::layer-before-dialog)

;; Layers ----------------------------------------------------------------------

(defonce popover-layer
  {:id :popover
   :bindings {}})

(def ui-bindings {#js ["command+k" "ctrl+k"]
                  (fn [e]
                    (dom/prevent-default e)
                    (dispatch [stores.ui/open-dialog {:id stores.ui/command-prompt-dialog-key}]))})

(defonce document-layer
  {:id :file
   :bindings (merge
              ui-bindings)})

(defonce agenda-layer
  {:id :agenda
   :bindings (merge
              ui-bindings
              {"backspace" (fn [e]
                             (let [el (.-target e)
                                   node-id (data-attr-id el "node-id")
                                   file-path (data-attr el "file-path")]
                               (when (and node-id file-path)
                                 (dispatch [stores.documents/remove-node {:file-path file-path
                                                                          :node-id node-id}]))))
               "enter" #(prn "Enter")})})

(def command-prompt-layer {:id :command-prompt
                           :bindings {#js ["command+k" "ctrl+k"]
                                      (fn [e]
                                        (dom/prevent-default e)
                                        (dispatch [stores.ui/close-dialog]))}})

;; Helpers ---------------------------------------------------------------------

(defn bind [shortcut f]
  (.bind mousetrap shortcut f))

(defn unbind [shortcut]
  (.unbind mousetrap shortcut))

;; Setters ---------------------------------------------------------------------

(defn activate! [state {:keys [bindings] :as layer}]
  (doseq [[k v] bindings]
    (bind k v))
  (assoc state layer-key layer))

(defn deactivate! [state]
  (when-let [{:keys [bindings]} (get state layer-key)]
    (doseq [[k _v] bindings]
      (.unbind mousetrap k))
    (assoc state layer-key nil)))

(defn activate-popover-layer! [state]
  (when-let [layer (get state layer-key)]
    (-> (deactivate! state)
        (assoc before-popover-key layer))))

(defn restore-before-popover-layer! [state]
  (when-let [prev-layer (get state before-popover-key)]
    (cond-> (or (deactivate! state) state)
      prev-layer (-> (activate! prev-layer)
                     (dissoc before-popover-key)))))

(defn activate-dialog-layer! [state]
  (when-let [layer (get state layer-key)]
    (-> (deactivate! state)
        (assoc before-dialog-key layer))))

(defn restore-before-dialog-layer! [state]
  (when-let [prev-layer (get state before-dialog-key)]
    (cond-> (or (deactivate! state) state)
      prev-layer (-> (activate! prev-layer)
                     (dissoc before-dialog-key)))))

;; Hooks -----------------------------------------------------------------------

(defn use-layer [layer]
  (uix/use-effect
   (fn []
     (dispatch [activate! layer])
     #(dispatch [deactivate! layer]))
   [layer]))

;; Initial ---------------------------------------------------------------------

(defonce initial {layer-key nil})
