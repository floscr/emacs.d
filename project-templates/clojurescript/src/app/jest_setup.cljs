(ns app.jest-setup
  (:require
   [cljest.setup]
   ["@testing-library/jest-dom"]))

;; Fix fetch missing when using lambdai
(when-not (exists? js/fetch)
  (set! js/fetch (fn [])))

(.defineProperty js/Object js/document "cookie" #js {})
