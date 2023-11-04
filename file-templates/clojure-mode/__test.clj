# -*- mode: snippet -*-
# name: Clojure Test Template
# group: file templates
# key: __t
# --
(ns `(my-clojure/current-namespace)`
  (:require
    [`(if (eq 'clojure-mode major-mode) "clojure.test" "cljs.test")` :refer [deftest is testing]]
    [`(my-clojure/target-test-namespace)` :as sut]))

(deftest ${2:sut-test}
  (testing "${3:testing}"
    (is (= (sut/${4:fn}) ${5:"expected"}))))