(ns cesiumdemo.util)

(defn jitter+ [n]
  (+ n (* (rand) 0.25)))

(defn jitter- [n]
  (- n (* (rand) 0.25)))

(defn midpoint [[t0 x0 y0 z0] [t1 x1 y1 z1]]
  [(+ t0 (/ (- t1 t0) 2.0))
   (+ x0 (/ (- x1 x0) 2.0))
   (+ y0 (/ (- y1 y0) 2.0))
   (+ z0 (/ (- z1 z0) 2.0))])

;;naive linear interpolation.  simplistic.
;;probably faster to use double arrays.
(defn lerpn [n xs]
  (if (zero? n)
    xs
    (recur (dec n)
           (apply concat
                  (for [[l r] (partition 2 1 xs)]
                    [l (midpoint l r) r])))))

;;helper to give us a nice quad representation.
;;czml/cesium defaults to using flat arrays,
;;but it's nicer to parse them out.
(defn ensure-partitions [n xs]
  (let [x (first xs)]
    (cond (and (coll? x) (= (count x) n))
          xs
          :else (partition n xs))))

;;meant to be used with ->>
;;if pred is truthy, applies f to rest,
;;otherwise passes xs through)
(defn apply-if [pred f & xs]
  (if pred
    (apply f xs)
    xs))

(defn catvec [xs]
  (->> xs
       (apply concat)
       vec))

(defn assoc-some [m & xs]
  (->> xs
       (partition   2)
       (filter second)
       (reduce (fn [acc [k v]]
                 (assoc! acc k v))
               (transient m))
       persistent!))


;;create a sequence of relative positions from one.
;;scales the relative time dimension by an optional scaling
;;parameter, in seconds.  Cesium also has the annoying
;;habit of wanting to use textual julian dates for everything.
;;So it's probably easier to go the "epoch" route and
;;define relative dates.  This is a fundamental building
;;block for entities...
(defn ->sampled-vec
  [txyz-coll & {:keys [type scale lerp-level interp-degree interp-algorithm epoch interval degree]
                :or {lerp-level 0
                     ;interp-degree 5
                     ;interp-algorithm "LAGRANGE"
                     degree 3} :as opts}]
  (let [scaler (if (and scale (not= scale 1))
                 (fn [[t y x z]]
                   [(* scale t) y x z])
                 identity)
        type (or type (-> txyz-coll meta :czml/type) :cartesian)]
    (-> {type (as-> (ensure-partitions (inc degree) txyz-coll) <parts>
                (if (and lerp-level (pos? lerp-level))
                  (lerpn lerp-level <parts>)
                  <parts>)
                (map scaler <parts>)
                (catvec <parts>))} ;;maybe not ideal, but fast enough for us!
        (assoc-some
         :epoch                  epoch
         :interpolationAlgorithm interp-algorithm
         :interpolationDegree    interp-degree))))

;;kind of lame, but let's do it.
(defn ->sampled-degrees [txyz-coll & {:keys [scale lerp-level interp-degree interp-algorithm epoch interval] :as opts}]
  (->sampled-vec txyz-coll
   :type :cartographicDegrees
   :scale scale :lerp-level lerp-level :interp-degree interp-degree :interp-algorithm interp-algorithm
   :epoch epoch :interval interval :degree 3))

(defn interpolate [m & {:keys [interp-algorithm interp-degree]
                        :or   {interp-degree 5
                               interp-algorithm "LAGRANGE"}}]
  (assoc m  :interpolationAlgorithm interp-algorithm
            :interpolationDegree    interp-degree))

(defn ->sampled-rgba [txyz-coll &
                      {:keys [scale lerp-level interp-degree interp-algorithm epoch interval]
                       :as opts}]
  (->sampled-vec txyz-coll
                 :type :rgba
                 :scale scale :lerp-level lerp-level :interp-degree interp-degree :interp-algorithm interp-algorithm
                 :epoch epoch :interval interval :degree 3))
