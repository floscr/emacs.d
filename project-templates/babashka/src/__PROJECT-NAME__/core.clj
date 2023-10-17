#!/usr/bin/env bb
(ns __PROJECT-NAME__.core
  (:require))

;; Main ------------------------------------------------------------------------

(defn -main [& args]
  (println args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main)
  nil)
