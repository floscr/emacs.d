(ns app.utils.css.core
  (:require
   [app.utils.css.shadow.css.spacing :as css.spacing]
   [app.utils.css.shadow.css.build :as css.build]
   [cats.monad.either :as either]
   [cljss.collect :as c]
   [cljss.core :as cljss]
   [cljss.media :refer [build-media]]
   [cljss.utils :as utils]
   [clojure.set :refer [map-invert]]
   [clojure.string :as str]
   [flatland.ordered.map :refer [ordered-map]]
   [clojure.walk :as walk]))

(def build-state (css.build/start))

(defn tailwind-class-name->css-map-m [tailwind-class-name]
  (let [keywords (str/split tailwind-class-name #":")
        getter #(get (:aliases build-state) (keyword %))]
    (cond
      ;; Single rule matching
      ;; e.g.: flex
      (= (count keywords) 1) (if-let [rule (getter (first keywords))]
                               (either/right rule)
                               (either/left {:error :unknown-rule
                                             :class-name tailwind-class-name}))
      ;; Selector:Rule matching
      ;; e.g.: hover:flex
      :else (let [[selector rules :as selector-rule] (map getter keywords)]
              (if (and selector rules)
                (either/right selector-rule)
                (either/left {:error (if selector
                                       :unknown-rule
                                       :unknown-selector)
                              :class-name tailwind-class-name}))))))

(comment
  (tailwind-class-name->css-map-m "hover:flex")
  (tailwind-class-name->css-map-m "missing:flex")
  (tailwind-class-name->css-map-m "hover:missing")
  nil)

(defn read-tailwind-class-names-string [class-str]
  (let [class-names (->> (str/split class-str #" ")
                         (eduction
                          (map str/trim)
                          (remove empty?))
                         (reduce (fn [acc cur]
                                   (-> (tailwind-class-name->css-map-m cur)
                                       (either/branch
                                        #(update acc :missing conj %)
                                        #(update acc :rules conj %))))
                                 {:missing [] :rules []}))]
    class-names))

(def spacing-by-vals (map-invert css.spacing/spacing-vars))

(defn fmap [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn map-leaves
  [f form]
  (walk/postwalk (fn [m]
                   (if (map? m)
                     (fmap #(if (map? %) % (f %)) m)
                     m))
                 form))

(defn css-values->variables [m]
  (map-leaves
   (fn [x]
       (if-let [var (spacing-by-vals x)]
         (str "var(" var ")")
         x))
   m))

(defn tailwind-class-names-string->clojure-styles-map [class-str]
  (let [{:keys [missing rules]} (read-tailwind-class-names-string class-str)
        styles-map (reduce
                    (fn [acc cur]
                      (cond
                        (map? cur) (merge acc cur)
                        :else (let [[selector rule] cur]
                                (update acc selector (fnil merge {}) rule))))
                    {}
                    rules)]
    {:rules (css-values->variables styles-map)
     :missing missing}))

(comment
  (def e "flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50")
  (tailwind-class-names-string->clojure-styles-map "hover:p-3 hover:m-3")
  (tailwind-class-names-string->clojure-styles-map e)

  (ordered-map (map (fn [x] [x x]) (range 30)))
  (into {} (map (fn [x] [x x]) (range 30)))
  nil)



(defn tailwind-str->css-map [s]
  (let [class-names (->> (str/split s #" ")
                         (map (fn [x]
                                (cond
                                  (= (str/trim x) "") nil
                                  :else (let [xs (str/split x #":")
                                              f #(or (get (:aliases build-state) (keyword %))

                                                     (println "Missing keyword" %))
                                              rule (cond
                                                     (seq (rest xs)) (mapv f xs)
                                                     :else (f x))]
                                          (when (and rule f)
                                            (cond
                                              (vector? (first rule)) (into {} rule)
                                              :else rule)))))))]
    class-names))


(def tailwind-media-query->cljss-media-query
  {">sm" [[:max-width "640px"]]
   "<sm" [[:min-width "640px"]]
   "@media (min-width: 640px)" [[:min-width "640px"]]
   "@media (min-width: 768px)" [[:min-width "768px"]]
   "@media (min-width: 1024px)" [[:min-width "1024px"]]
   "@media (min-width: 1280px)" [[:min-width "1280px"]]
   "@media (min-width: 1536px)" [[:min-width "1536px"]]})

(defn reduce-into [xs]
  (reduce
   (fn [acc cur]
     (let [mq (tailwind-media-query->cljss-media-query (first cur))]
       (cond
         mq (update-in acc [::cljss/media mq] (fnil conj {}) (second cur))
         :else (into acc [cur]))))
   {} xs))

(defn- style-arg->style-map [x]
  (cond
    (keyword? x) nil
    (string? x) (tailwind-str->css-map x)
    (vector? x) {(first x) (into {} (mapcat style-arg->style-map (rest x)))}
    :else x))

(defn merge-css [styles]
  (->> (mapcat style-arg->style-map styles)
       (reduce-into)
       (#(dissoc % nil))))

(defn pseudo? [[rule value]]
  (and (re-matches #"&(:|\[(data|aria)-).*" (name rule))
       (map? value)))

(defn build-styles [cls styles]
  (c/reset-env! {:cls cls})
  (let [pseudo  (filterv pseudo? styles)
        nested  (->> styles
                     (filterv (comp not pseudo?))
                     (filterv utils/nested?))
        [mstatic mvals] (some-> styles :cljss.core/media build-media)
        styles  (dissoc styles :cljss.core/media)
        styles  (filterv #(and (not (pseudo? %)) (not (utils/nested? %))) styles)
        [static vals] (c/collect-styles cls styles)
        pstyles (->> pseudo
                     (reduce
                      (fn [coll [rule styles]]
                        ;; Allow multi rule assignment
                        ;; (Might run into issue when the ", &" substr is found somewhere, would need proper parsing)
                        (let [rules* (str/split rule #", &")
                              rule (first rules*)
                              trimmed-rules (or (rest rules*) [])
                              fixed-rules (into [rule] (map #(str "&" %) trimmed-rules))
                              selector (->> (map #(str cls (subs (name %) 1)) fixed-rules)
                                            (str/join ", ."))]
                          (conj coll (c/collect-styles selector styles))))
                      []))
        nstyles (->> nested
                     (reduce
                      (fn [coll [rule styles]]
                        (conj coll (c/collect-styles (str cls " " rule) styles)))
                      []))
        vals    (->> pstyles
                     (mapcat second)
                     (into vals)
                     (concat mvals)
                     (into []))
        vals (->> nstyles
                  (mapcat second)
                  (into vals))
        static (into [static] (map first pstyles))
        static (into static (map first nstyles))
        static (if mstatic
                 (conj static mstatic)
                 static)]
    [cls static vals]))

(comment

  (build-styles
   "hello"
   {"&:first-child, &:focus" {:color "red"}
    "a" {:color "blue"}})

  (tailwind-str->css-map "col-span-3 lg:col-span-4 lg:border-l")
  (tailwind-str->css-map "flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50")

  (merge-css ["border-b transition-colors hover:bg-muted/50"])

  (merge-css ["h-4 w-4 shrink-0 rounded-sm border border-primary border-solid shadow flex"])

  (tailwind-str->css-map
   "grid lg:grid-cols-5")

  (style-arg->style-map [":hover" "col-span-3 lg:col-span-4 lg:border-l"])

  (tailwind-str->css-map
   "grid lg:grid-cols-5")
  nil)

(defmacro css
  "Takes var name, a vector of arguments and a hash map of styles definition.
   Generates class name, static and dynamic parts of styles.
   Returns a function that calls `cljss.core/css` to inject styles at runtime
   and returns generated class name."
  [var args & styles]
  (let [cls-name# (cljss/var->cls-name var)
        [_ static# vals#] (build-styles cls-name# (merge-css styles))]
    `(defn ~var ~args
       (cljss.core/css ~cls-name# ~static# ~vals#))))
