(ns app.state.fx.document
  (:require
   [app.state.atom :as state.atom]
   [app.state.stores.documents :as stores.documents]
   [lambdaisland.fetch :as fetch]
   [org-mode.fp.promise :as fpp]
   [org-mode.fp.transit :as shared.transit]
   [promesa.core :as p])
  (:refer-clojure :exclude [use]))

(def auth-token "eyJhbGciOiJIUzI1NiJ9.eyJ1c2VybmFtZSI6ImFkbWluIiwicGFzc3dvcmQiOiIkMmEkMTEkby5UTldsalExdS9CTHZ1VUx4Vy9jLml5L1JEZWJnRWJaOW9SV1RXa1pKT0VUdTBpb0xhQ08ifQ.9Mc0AqSkzu37w7tiSvGFYSCjU0J-G3zjRNLmWP044Wk")

(defn fetch+ [route-path _auth-token]
  (p/let [file-path (fpp/from-nillable route-path {:error :could-not-convert-route-to-path
                                                   :path route-path})
          resp (fetch/get (str "/api/transaction")
                          {:headers {"Authorization" (str "Token " auth-token)
                                     "ops" (str [{:file-path file-path}])}})]
    (->> (:body resp)
         (shared.transit/read))))

(defn add-document+ [route-path _auth-token]
  (-> (fetch+ route-path auth-token)
      (p/then (fn [docs]
                (let [document (-> (first docs)
                                   :document)]
                  (state.atom/dispatch [stores.documents/add-document-from-edn document]))))
      (p/catch js/console.error)))

(defn fetch-ops+ [ops _auth-token]
  (p/let [resp (fetch/get (str "/api/transaction")
                          {:headers {"Authorization" (str "Token " auth-token)
                                     "ops" (str ops)}})]
    (:body resp)))

(defn load-documents-from-ops [ops auth-token]
  (-> (fetch-ops+ ops auth-token)
      (p/then (fn [ops-resp]
                (let [documents (map :document ops-resp)]
                  (state.atom/dispatch [stores.documents/add-documents-from-edn documents]))))
      (p/catch js/console.error)))
