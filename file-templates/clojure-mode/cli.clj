# -*- mode: snippet -*-
# name: Clojure Cli Template
# group: file templates
# key: __cli
# --
(ns `(my-clojure/current-namespace)`
  (:require
   [babashka.cli :as cli]))

;; Commands --------------------------------------------------------------------

(defn main [opts]
  (println "Hello world" opts))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main :args->opts [:out-file]}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))