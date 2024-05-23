(ns app.state.middleware.middlewares.local-storage-persist
  (:require
   [cljs.reader :as edn]))

(def local-storage-key "org")

(defn update-local-storage! [_prev-state state]
  (js/localStorage.setItem local-storage-key (str state)))

(defn parse-persistence-layer [string]
  (-> (edn/read-string {} string)))

(defn get-local-storage! []
  (try
    (->> (js/localStorage.getItem local-storage-key)
         (parse-persistence-layer))
    (catch js/Error e
      (js/console.error "Could not read state from local-storage" (js/localStorage.getItem local-storage-key) e))))

(defn remove-local-storage! []
  (js/localStorage.removeItem local-storage-key))

(def local-storage-middleware
 {:get get-local-storage!
  :update update-local-storage!})
