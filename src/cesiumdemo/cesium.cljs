(ns cesiumdemo.cesium
  (:require  [reagent.core :as r]
             #_[cesiumdemo.colors :as colors]))


(def ^:const +wgs-84+
  Cesium.Ellipsoid.WGS84)

;;aux functions
(defn radians [x] (Cesium.Math.toRadians x))

;;this doesn't work per the tutorial...
(def +default-offset+
  (Cesium.HeadingPitchRange.
   (radians 90)
   (radians -45)))

;;basic cesium stuff
(defprotocol ICartesian
  (-as-cartesian [c])) 

#_(defn degree-heights->carts)

;;this lets us use vectors
;;interchangably with cartesian
;;types in cesium.
(extend-protocol ICartesian
  Cesium.Cartesian3
  (-as-cartesian [c] c)
  Cesium.Cartesian2
  (-as-cartesian [c] c)
  Cesium.Cartesian4
  (-as-cartesian [c] c)
  cljs.core.PersistentVector
  (-as-cartesian [c]
    (case (count c)
      2 (let [[x y] c]
          (Cesium.Cartesian2. x y))
      3 (let [[x y z] c]
          (Cesium.Cartesian3. x y z))
      4 (let [[a b c d] c]
          (Cesium.Cartesian3. a b c d)))))  

;;probably create a point protocol and
;;extend it to vectors, like as-cartesian
(defn ->cartesian 
  ([x y]
   (Cesium.Cartesian2. x y))
  ([x y z]
   (Cesium.Cartesian3. x y z))
  ([x y z w]
   (Cesium.Cartesian4. x y z w)))

(defn degrees->cartesian 
  ([lat long height ellipsoid]
   (Cesium.Cartesian3.fromDegrees lat long height ellipsoid))
  ([lat long height]
   (degrees->cartesian lat long height +wgs-84+))  
  ([lat long]
   (degrees->cartesian lat long 0 +wgs-84+)))

(defn radians->cartesian 
  ([lat long height ellipsoid]
   (Cesium.Cartesian3.fromRadians lat long height ellipsoid))
  ([lat long height]
   (degrees->cartesian lat long height +wgs-84+))  
  ([lat long]
   (degrees->cartesian lat long 0 +wgs-84+)))

;;Also want coercions for degree sequences
;;to cart 3 coord....
(defn degrees->cart3 [xs]
  (js/Cesium.Cartesian3.fromDegreesArray
   (clj->js (vec xs))))  

;;more generic, data-driven version
;;since the only version that supports
;;this is cart3....we can get away.
;;Most of the cesium examples show
;;the Array methods (i.e. ingest and
;;return js Arrays).
(defn degrees->cartesians 
  "Given a sequence of [lat1, long1, lat2, long2] pairs
   or [lat1, long1, height1, lat2, long2, height2] triples
   coerces to a Cartesian3 based on the specified encoding
   and optional ellipsoid.  lat/long are encoded as degrees"
  ([n xs]
   (degrees->cartesians n +wgs-84+ xs))
  ([n ellipsoid xs]
   (let [f (case n 
             2  (fn [lat long]
                  (degrees->cartesian lat long 0 ellipsoid))
             3  (fn [lat long height]
                  (degrees->cartesian lat long 0 ellipsoid)))]
     (->> xs 
          (partition n)
          (map #(apply f %))))))

(defn radians->cartesians 
  "Given a sequence of [lat1, long1, lat2, long2] pairs
   or [lat1, long1, height1, lat2, long2, height2] triples
   coerces to a Cartesian3 based on the specified encoding
   and optional ellipsoid.  lat/long are encoded as radians"
  ([n xs]
   (radians->cartesians n +wgs-84+ xs))
  ([n ellipsoid xs]
   (let [f (case n 
             2  (fn [lat long]
                  (radians->cartesian lat long 0 ellipsoid))
             3  (fn [lat long height]
                  (radians->cartesian lat long 0 ellipsoid)))]
     (->> xs 
          (partition n)
          (map #(apply f %))))))

;;Generate a random lat/long
;;along the respective domains
;;[-90 89]
;;[-180 179]
(defn random-point []
  [(- (rand-int 90) 89)
   (- (rand-int 180) 179)])

;;n random coordinates.
(defn random-coords [n]
  (repeatedly n (fn []
                   (random-point))))

;;Generic color stuff
;;probably useful to have an as-color....
;;We can get a simple color bridge, going
;;from 
(def red (Cesium.Color.RED.withAlpha 0.5))
(def black Cesium.Color.BLACK)


;;we can define a protocol here...
;;might be useful if we wrap the viewer object
;;and delegate.

;;Entity Operations
(defn entities [v]
  (-> v .-entities))

(defn select-entity! [v e]
  (-> v .-selectedEntity (set! e)))

(defn track-entity! [v e]
  (-> v .-trackedEntity (set! e)))

(defn add-entity! [v x]
  (let [es (.-entities v)]
    (.add es x)))

(defn add-entities! [v xs]
  (reduce (fn [acc e]
            (doto acc (.add e)))
          (.-entities v)
          xs))

;;note: this doesn't work with polygons, as the
;;tutorial depicts!  We either need a protocol or
;;another function to help us out here...
(defn set-position! [e pos]
  (-> e .-position (set! pos)))

(defn set-height! [e h]
  (-> e .-polygon .-height (set! h)))

(defn set-material! [e m]
  (-> e .-polygon .-material (set! m)))

(defn extrude-height! [e h]
  (-> e .-polygon .-extrudedHeight (set! h)))

(defn set-description! [e d]
  (-> e .-description (set! d)))

;; var heading = Cesium.Math.toRadians(90);
;; var pitch = Cesium.Math.toRadians(-30);

;;Primitive Operations



;;View API (zooming, flying, selecting)
(defn zoom-to!
  ([v]
   (.zoomTo v (.-entities v)))
  ([v e]
   (.zoomTo v e))
  ([v e offset]
   (.zoomTo v e offset)))

;;look at decoupling the
;;promise, or messing
;;with the promise chain
;;via a macro or something.

(defn fly-to!
  ([v e]
   (-> v
      (.flyTo e)
      (.then (fn [result]
               (when result
                 (select-entity! v e))))))
  ([v e offset]
   (-> v
       (.flyTo e offset)
       (.then (fn [result]
                (when result
                  (select-entity! v e)))))))

(defn ->point [lat long]
    {
     :position (Cesium.Cartesian3.fromDegrees lat long),
     :point {
             :pixelSize 5,
             :color Cesium.Color.RED,
             :outlineColor Cesium.Color.WHITE,
             :outlineWidth 2}})    
 
(defn ->random-points [n]
  (for [[lat lng] (random-coords n)]
    (->point lat lng)))


(defn load-czml! [coll]
  (let [p (js/Cesium.CzmlDataSource.load coll)]
    (.add  (.-dataSources (@ces/view :current))
           p)))
