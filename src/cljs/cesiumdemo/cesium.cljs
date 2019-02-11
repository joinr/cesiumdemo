(ns cesiumdemo.cesium
  (:require  [reagent.core :as r]
             [cljcolor.core :as color]))


;;we'll maintain some state for our globe....
(defonce view
  (r/atom {}))

;;var viewer = new Cesium.Viewer('cesiumContainer');
;;need to add options....
(defn ->viewer [el {:keys [] :as opts}]
  (js/Cesium.Viewer. el (clj->js opts)))

;;aux functions
(defn radians [x] (Cesium.Math.toRadians x))

;;this doesn't work per the tutorial...
(def +default-offset+
  (Cesium.HeadingPitchRange.
   (radians 90)
   (radians -45)))

(extend-protocol color/IColorVector
  Cesium.Color
  (color-vec [c] [(.-r c) (.-g c) (.-b c) (.-a c)]))

(defprotocol ICartesian
  (-as-cartesian [c]))

;;probably create a point protocol and
;;extend it to vectors, like as-cartesian
(defn ^Cesium.Cartesian3 ->cartesian3 [x y z]
  (js/Cesium.Cartesian3. x y z))

;;Also want coercions for degree sequences
;;to cart 3 coord....
(defn degrees->cart3 [xs]
  (js/Cesium.Cartesian3.fromDegreesArray
   (clj->js (vec xs))))

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

;;Entity Operations
(defn entities []
  (->> @view :current .-entities))

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

;;basic reagent component.
(defn cesium-viewer [{:keys [name opts]}]
  (let [vw (keyword (str name "-view"))]
    (r/create-class
     {:display-name (str name)
      :reagent-render (fn [] [:div])
      :component-did-mount
      (fn [this]
        (let [v (->viewer (r/dom-node this) opts)
              _ (swap! view assoc :current v)]
          v))
      #_:component-did-update
      #_(fn [this]
          (when-let [view (get @app-state vw)]
            (.update view)))
      #_:component-will-update
      #_(fn [this]
          (let [view (chart {:el (r/dom-node this)})
                _    (swap! app-state assoc :view view)]
            (.update view)))})))
