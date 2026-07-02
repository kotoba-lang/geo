(ns kotoba.geo.mesh-test
  "Parity port of kami-engine/kami-geo/src/mesh.rs `#[cfg(test)]` modules
  (`antimeridian_tests` and `tests`) — Rust source deleted pre-commit;
  recovered from git history."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.geo.mesh :as mesh]
            [kotoba.geo.projection :as proj]))

;; ---------------------------------------------------------------------
;; split-antimeridian (was `antimeridian_tests` mod)
;; ---------------------------------------------------------------------

(deftest no-crossing-passes-through
  (let [ring [[0.0 0.0] [10.0 0.0] [10.0 10.0] [0.0 10.0]]
        out (mesh/split-antimeridian ring)]
    (is (= 1 (count out)) "non-crossing ring should be 1 subring")))

(deftest dateline-crossing-splits-into-two
  ;; Fiji-style ring straddling +180 / -180.
  (let [ring [[179.0 -10.0] [-179.0 -10.0] [-179.0 10.0] [179.0 10.0]]
        out (mesh/split-antimeridian ring)]
    (is (>= (count out) 2) "dateline-crossing ring must split")
    (doseq [sub out
            [lon _lat] sub]
      (is (and (>= lon (- -180.0 1e-6)) (<= lon (+ 180.0 1e-6)))
          (str "subring lon " lon " out of range")))))

(deftest antarctica-like-long-strip
  ;; Simplified Antarctica outline: long east-to-west strip across dateline.
  (let [ring [[-170.0 -60.0] [170.0 -60.0] [170.0 -85.0] [-170.0 -85.0]]
        out (mesh/split-antimeridian ring)]
    (is (>= (count out) 2)
        (str "Antarctica-like strip must split at dateline (got " (count out) " subrings)"))))

;; ---------------------------------------------------------------------
;; Mesh generation (was `tests` mod)
;; ---------------------------------------------------------------------

(deftest tile-quad-mesh
  (let [m (mesh/tile-quad)]
    (is (= (count (:vertices m)) (* 4 8)))
    (is (= (count (:indices m)) 6))))

(deftest ribbon-two-points
  (let [center (proj/world-px 0.0 0.0)
        coords [[0.0 0.0] [1.0 0.0]]
        m (mesh/line-to-ribbon coords 0.0 center 10.0 0.0)]
    (is (= (count (:vertices m)) (* 4 8)))
    (is (= (count (:indices m)) 6))))

(deftest polygon-triangle
  (let [center (proj/world-px 0.0 0.0)
        coords [[0.0 0.0] [10.0 0.0] [5.0 10.0]]
        m (mesh/polygon-to-fill coords 0.0 center 0.0)]
    (is (= (count (:vertices m)) (* 3 8)))
    (is (= (count (:indices m)) 3))))

(deftest polygon-earcut-concave
  (let [center (proj/world-px 0.0 0.0)
        ;; Concave arrow-like shape.
        coords [[0.0 0.0] [10.0 0.0] [5.0 5.0] [10.0 10.0] [0.0 10.0]]
        m (mesh/polygon-to-fill-earcut coords 0.0 center 0.0)]
    (is (= (count (:vertices m)) (* 5 8)))
    ;; 5-gon -> 3 triangles -> 9 indices.
    (is (= (count (:indices m)) 9))))

(deftest circles-batched
  (let [center (proj/world-px 0.0 0.0)
        pts [[0.0 0.0] [1.0 1.0]]
        m (mesh/points-to-circles pts 0.0 center 4.0 0.0 8)]
    ;; 2 points × (1 center + 8 rim) × 8 floats.
    (is (= (count (:vertices m)) (* 2 9 8)))
    ;; 2 points × 8 triangles × 3 idx.
    (is (= (count (:indices m)) (* 2 8 3)))))

(deftest extrude-square-has-roof-and-walls
  (let [center (proj/world-px 0.0 0.0)
        ring [[0.0 0.0] [0.001 0.0] [0.001 0.001] [0.0 0.001]]
        m (mesh/polygon-to-extrude-earcut ring 0.0 center 0.0 10.0)]
    ;; Roof = 4 verts, walls = 4 edges × 4 verts = 16. Total 20 verts × 8 floats.
    (is (= (count (:vertices m)) (* 20 8)))
    ;; Roof 2 tri × 3 + Walls 4 edges × 2 tri × 3 = 6 + 24 = 30 indices.
    (is (= (count (:indices m)) 30))))

;; ---------------------------------------------------------------------
;; Extra coverage for globe / DEM paths (not in the Rust `#[test]`s, but
;; exercised here since they were previously only covered indirectly via
;; the renderer integration, which is out of scope for this port).
;; ---------------------------------------------------------------------

(deftest globe-tile-patch-basic
  (testing "globe-tile-patch produces a well-formed sphere patch mesh"
    (let [coord (proj/tile-coord 4 8 6)
          m (mesh/globe-tile-patch coord 100.0 4)]
      (is (pos? (count (:vertices m))))
      (is (zero? (mod (count (:vertices m)) 8)))
      (is (pos? (count (:indices m))))
      (is (zero? (mod (count (:indices m)) 3))))))

(deftest flat-tile-patch-from-dem-fallback
  (testing "falls back to tile-quad when the DEM grid is too small"
    (let [coord (proj/tile-coord 4 8 6)
          m (mesh/flat-tile-patch-from-dem coord 8 [] 0 0 1.0)]
      (is (= m (mesh/tile-quad))))))
