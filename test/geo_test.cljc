(ns geo-test
  (:require [clojure.test :refer [deftest is testing]]
            [geo]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? geo))))
