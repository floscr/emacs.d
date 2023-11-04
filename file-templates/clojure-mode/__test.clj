# -*- mode: snippet -*-
# group: file templates
# key: __t
# --
(ns `(my-clojure/current-namespace)`
  (:require
    [clojure.test :refer [deftest is testing]]
    [`(my-clojure/target-test-namespace)` :as sut]))

(deftest ${2:sut-test}
  (testing "${3:testing}"
    (is (= (sut/${4:fn}) ${5:"expected"}))))