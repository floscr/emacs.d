(ns app.state.fx.document-test
  (:require
   [app.state.atom :as state.atom]
   [app.state.fx.document :as sut]
   [cljest.core :as cljest :refer [describe is it]]
   [cljest.helpers.core :as h]
   [lambdaisland.fetch :as fetch]
   [promesa.core :as p]
   [app.state.stores.documents :as stores.documents]))

(describe "document-hooks-test"
  (it "fetches document"
      (let [file-path "Main/inbox.org"
            resp {:headers {"document-hash" "1"}
                  :body "* Test"}]
        (h/with-mocks [fetch/get (constantly (p/resolved resp))]
          (-> (sut/fetch+ file-path "auth")
              (p/then (fn [x]
                        (is (= x {:hash "1"
                                  :body "* Test"
                                  :file-path file-path}))))))))

  (it "fails at missing route-path"
      (h/with-mocks [fetch/get (constantly (p/resolved nil))]
        (-> (sut/fetch+ nil "auth")
            (p/catch #(is (= (:error %) :could-not-convert-route-to-path))))))

  (it "adds document to the store"
    (let [state (atom nil)
          file-path "Main/inbox.org"
          resp {:headers {"document-hash" "1"}
                :body "* Test"}]
      (h/with-mocks [fetch/get (constantly (p/resolved resp))
                     state.atom/dispatch (fn [[f payload]]
                                           (reset! state (f @state payload)))]
        (-> (sut/add-document+ file-path "auth")
            (p/then (fn [x]
                      (let [{:keys [parsed hash] :as doc} (stores.documents/document-at-path @state file-path)]
                        (is (some? doc))
                        (is (some? parsed))
                        (is (= hash "1"))))))))))
