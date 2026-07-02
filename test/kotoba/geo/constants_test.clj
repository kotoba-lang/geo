(ns kotoba.geo.constants-test
  "Asserts `kotoba.geo.constants/constants` (the portable cljc mirror)
  matches `resources/kotoba/geo/constants.edn` (the EDN authority)
  byte-for-byte, the same authority-vs-mirror pattern used by
  `kotoba-lang/kami-scene-contracts`. JVM-only (uses `clojure.java.io`)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [kotoba.geo.constants :as k]))

(deftest constants-match-edn-authority
  (let [authority (edn/read-string (slurp (io/resource "kotoba/geo/constants.edn")))]
    (is (= authority k/constants))))
