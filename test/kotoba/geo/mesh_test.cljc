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

;; ---------------------------------------------------------------------------
;; A globe patch must be in register with the raster tile it is textured with.

(def ^:private rad->deg (/ 180.0 Math/PI))

(defn- vertex-rows
  "The patch's vertices as rows of `[x y z]`, one row per v step."
  [m segs]
  (let [vs (vec (partition 8 (:vertices m)))]
    (mapv (fn [iy] (vec (take 3 (nth vs (* iy (inc segs)))))) (range (inc segs)))))

(defn- latitude-of [[x y z]]
  (* rad->deg (Math/asin (/ y (Math/sqrt (+ (* x x) (* y y) (* z z)))))))

(deftest globe-tile-patch-latitude-follows-mercator-not-a-lerp
  ;; `globe-tile-patch` exists to carry one raster tile's image. A raster
  ;; tile's pixel rows are linear in MERCATOR y; latitude is the inverse
  ;; Gudermannian of that. Interpolating latitude linearly between the tile's
  ;; own north and south -- which this did -- puts the image up to 24 degrees
  ;; out of register at z0, 15 at z1 and 0.5 at z3. The result looks like a
  ;; projection choice rather than a defect, which is why it survived.
  (let [checked (atom 0)
        segs 4]
    (doseq [z [0 1 2 3 5]
            :let [n (Math/pow 2.0 z)]
            ty (range (min 3 (long n)))]
      (let [coord {:x 0 :y ty :z z}
            ;; terrain-scale 0 -> an exact sphere, so the only thing under
            ;; test is where each row sits, not the relief on top of it.
            rows (vertex-rows (mesh/globe-tile-patch-terrain coord 1.0 segs 0.0) segs)]
        (doseq [iy (range (inc segs))]
          (let [v (/ (double iy) segs)
                expect (proj/mercator-tile-y->lat (+ ty v) n)
                got (latitude-of (nth rows iy))]
            (swap! checked inc)
            (is (< (Math/abs (- got expect)) 1.0e-9)
                (str "z" z " tile-y " ty " v " v ": vertex latitude " got
                     " but that raster row is at " expect))))))
    (is (<= 40 @checked) (str "only " @checked " rows compared"))))

(deftest globe-tile-patch-edges-still-meet-the-tile-bounds
  ;; The fix must not move the SEAMS: v=0 and v=1 have to stay exactly on the
  ;; tile's own north and south, or adjacent patches crack apart.
  (let [segs 4]
    (doseq [z [1 2 4] ty (range (min 3 (bit-shift-left 1 z)))]
      (let [coord {:x 0 :y ty :z z}
            [_ south _ north] (proj/tile-lng-lat-bounds coord)
            rows (vertex-rows (mesh/globe-tile-patch-terrain coord 1.0 segs 0.0) segs)]
        (is (< (Math/abs (- (latitude-of (first rows)) north)) 1e-9)
            (str "z" z " y" ty " north edge moved"))
        (is (< (Math/abs (- (latitude-of (last rows)) south)) 1e-9)
            (str "z" z " y" ty " south edge moved"))))))

(deftest mercator-tile-y-is-exposed-and-takes-a-fraction
  ;; It was private; `mesh` needs it per vertex. A whole-number row must give
  ;; the tile bound it is the bound of.
  (doseq [z [0 2 5] :let [n (Math/pow 2.0 z)] y (range (min 3 (long n)))]
    (let [[_ south _ north] (proj/tile-lng-lat-bounds {:x 0 :y y :z z})]
      (is (< (Math/abs (- (proj/mercator-tile-y->lat y n) north)) 1e-12))
      (is (< (Math/abs (- (proj/mercator-tile-y->lat (inc y) n) south)) 1e-12))))
  (testing "and the midpoint is NOT the average of the bounds"
    (let [[_ south _ north] (proj/tile-lng-lat-bounds {:x 0 :y 0 :z 0})
          mid (proj/mercator-tile-y->lat 0.5 1.0)]
      (is (< (Math/abs mid) 1e-12) "z0 midpoint is the equator")
      (is (< (Math/abs (- mid (/ (+ north south) 2.0))) 1e-9)
          "at z0 they coincide -- pick a tile where they do not")))
  (testing "a tile where the lerp and the projection genuinely differ"
    (let [n 2.0
          [_ south _ north] (proj/tile-lng-lat-bounds {:x 0 :y 0 :z 1})
          mid (proj/mercator-tile-y->lat 0.5 n)
          lerp (/ (+ north south) 2.0)]
      (is (> (Math/abs (- mid lerp)) 20.0)
          (str "expected a large disagreement at z1; got " (Math/abs (- mid lerp)))))))
