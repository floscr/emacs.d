(ns app.state.stores.documents
  (:require
   [app.state.stores.route :as stores.route]
   [cats.core :as m]
   [flatland.ordered.map :refer [ordered-map]]
   [lambdaisland.fetch :as fetch]
   [lentes.core :as l]
   [medley.core :refer [dissoc-in]]
   [org-mode.fp.seq :refer [vec-concat]]
   [org-mode.fp.transit :as shared.transit]
   [org.parsers.inline.core :as org.inline]
   [org.state.ops :as org.ops]
   [org.writers.org.heading.core :as w.o.heading]
   [org.zipper :as ozip]))

;; Lenses ----------------------------------------------------------------------

(def documents-lens (l/in [:documents]))

(def document-at-path-lens #(comp documents-lens (l/key %)))

(def parsed-path-lens #(comp (document-at-path-lens %) (l/key :parsed)))

(def editing-node-id-lens (l/in [:documents/editing-node-id]))

(def ops-lens (l/in [:documents/ops]))

(def editing-node-lens (l/in [:node :editing?]))

;; Helpers ---------------------------------------------------------------------

(defn location->file! [path]
  (some->> path
           (re-find #"/app/(file|agenda)/(.+)")
           (last)))

;; Getters ---------------------------------------------------------------------

(defn document-at-path
  "Return document without any transactions."
  [state path]
  (l/focus (document-at-path-lens path) state))

(defn document-with-transactions
  "Returns a document with all the pending transactions applied."
  [state path]
  (let [grouped-ops (org.ops/group-ops-by-path (l/focus ops-lens state))
        current-document (document-at-path state path)
        ops (l/focus (l/key path) grouped-ops)
        doc (if (empty? ops)
              current-document
              (->> (org.ops/apply-ops-to-document {:document current-document
                                                   :ops ops})
                   (m/extract)))]
    (when doc
      (assoc doc :file-path path))))

(defn route-document [state]
  (let [path (location->file! (l/focus stores.route/path-lens state))]
    (document-at-path state path)))

(defn editing-node-id [state]
  (l/focus editing-node-id-lens state))

;; Setters ---------------------------------------------------------------------

(defn add-ops
  "Concat `ops` to `state`."
  [state ops]
  (l/over ops-lens #(vec-concat % ops) state))

  ;; TODO Use file api
(defn document-parsed [document]
  (get document :parsed))

  ;; TODO Use file api
(defn document-hash [document]
  (get-in document [:source :hash]))

  ;; TODO Use file api
(defn document-path [document]
  (get-in document [:source :path]))

(defn add-document-from-edn
  "Adds document from parsed `body` under `file-path` with `hash` of the original file."
  [state document]
  (let [file-path (document-path document)
        hash (document-hash document)
        parsed (document-parsed document)]
    (assoc-in state [:documents file-path] {:parsed (update parsed :zipper (comp ozip/zipper-no-root first))
                                            :hash hash})))

(defn add-documents-from-edn [state documents]
  (reduce add-document-from-edn state documents))

(def token "eyJhbGciOiJIUzI1NiJ9.eyJ1c2VybmFtZSI6ImFkbWluIiwicGFzc3dvcmQiOiIkMmEkMTEkby5UTldsalExdS9CTHZ1VUx4Vy9jLml5L1JEZWJnRWJaOW9SV1RXa1pKT0VUdTBpb0xhQ08ifQ.9Mc0AqSkzu37w7tiSvGFYSCjU0J-G3zjRNLmWP044Wk")

(defn transition-document
  "Adds a transition to a document that will be displayed in the state."
  [state {:keys [ops]}]
  (let [_auth-token (get-in state [:auth :token])]
    (fetch/post "/api/transaction"
                {:headers {"Authorization" (str "Token " token)
                           "content-type" "application/transit+json"
                           "accept" "application/transit+json"}
                 :body {:ops (doall ops)}})
    (add-ops state ops)))

(defn create-temp-node
  "Creates a temporary node at `pos` to edit."
  [state {:keys [file-path node-id]}]
  (let [node (w.o.heading/create-heading-with-id
              {:editing? true
               :todo "TODO"})
        op {:id (random-uuid)
            :type :create
            :file-path file-path
            :node node
            :node-id node-id
            :at :child}]
    (->> (add-ops state [op])
         (l/put editing-node-id-lens (:id node)))))

(defn remove-node
  "Creates a temporary node at `pos` to edit."
  [state {:keys [file-path node-id]}]
  (let [op {:id (random-uuid)
            :type :remove
            :file-path file-path
            :node-id node-id}]
    (transition-document state {:ops [op]})))

(defn persist-editing-node [state {:keys [content]}]
  (let [editing-ops (->> (l/focus ops-lens state)
                         (filter #(get-in % [:node :editing?])))
        editing-op-ids (-> (map :id editing-ops)
                           (set))
        update-op-fn (fn [op]
                       (-> (dissoc-in op [:node :editing?])
                           (assoc-in [:node :content] (org.inline/->tokens content))))
        transaction-ops (map update-op-fn editing-ops)]
    (transition-document state {:ops transaction-ops})
    (->> state
         (l/put editing-node-id-lens nil)
         (l/over ops-lens (fn [ops]
                            (map #(if (editing-op-ids (:id %)) (update-op-fn %) %) ops))))))

(defn remove-editing-node-id [state]
  (when-let [node-id (l/focus editing-node-id-lens state)]
    (->> state
         (l/put editing-node-id-lens nil)
         (l/over ops-lens #(into [] (org.ops/remove-ops-with-node-id node-id %))))))

;; Initial ---------------------------------------------------------------------

(defonce initial
  {:documents {}
   :documents/editing-node-id nil
   :documents/ops []})
