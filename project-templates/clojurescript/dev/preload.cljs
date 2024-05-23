(ns preload
  (:require
   [app.styles :as styles]
   [cljss.core :as css]
   [uix.dev]
   [devtools.core :as devtools]))

(enable-console-print!)

(devtools/set-pref! :dont-display-banner true)
(devtools/set-pref! :min-expandable-sequable-count-for-well-known-types 0)

(uix.dev/init-fast-refresh!)

(defn ^:dev/after-load refresh []
  (css/remove-styles!)
  (styles/inject-init-styles!)
  (uix.dev/refresh!))
