(ns cesiumdemo.cesium
  (:require  [reagent.core :as r]
             #_[cljsjs.Cesium :as cesium])
  #_(:import [Cesium.Viewer]))


;;we'll maintain some state for our globe....
(defonce view
  (r/atom {}))

;;var viewer = new Cesium.Viewer('cesiumContainer');
;;need to add options....
(defn ->viewer [el {:keys [] :as opts}]
  (js/Cesium.Viewer. el (clj->js opts)))

;;probably create a point protocol and
;;extend it to vectors...

(defn ^Cesium.Cartesian3 ->cartesian3 [x y z]
  (js/Cesium.Cartesian3. x y z))

(defn degrees->cart3 [xs]
  (js/Cesium.Cartesian3.fromDegreesArray
   (clj->js (vec xs))))

(def wyoming-coords
  [-109.080842,45.002073,
   -105.91517,45.002073,
   -104.058488,44.996596,
   -104.053011,43.002989,
   -104.053011,41.003906,
   -105.728954,40.998429,
   -107.919731,41.003906,
   -109.04798,40.998429,
   -111.047063,40.998429,
   -111.047063,42.000709,
   -111.047063,44.476286,
   -111.05254,45.002073])

(defn random-point []
   [(- (rand-int 90) 89)
    (- (rand-int 180) 179)])

(defn random-coords [n]
  (repeatedly n (fn []
                   (random-point))))

;;probably useful to have an as-color....
(def red (Cesium.Color.RED.withAlpha 0.5))
(def black Cesium.Color.BLACK)

(def wyoming-spec
  {:name  "Wyoming"
   :polygon
   {:hierarchy (degrees->cart3 wyoming-coords)
    :height    0
    :material  red
    :outline    true,
    :outlineColor  black}})

;;alternate for polygon hierarchy, according to
;;https://groups.google.com/forum/#!topic/cesium-dev/z24cXD5mBgs
(def wyoming-spec-constant
  {:name  "Wyoming"
   :polygon
   {:hierarchy (Cesium.ConstantProperty.
                (Cesium.PolygonHierarchy.
                 (clj->js (degrees->cart3 wyoming-coords))))
    :height    0
    :material  red
    :outline    true,
    :outlineColor  black}})


(defn entities []
  (->> @view :current .-entities))

(defn select-entity! [v e]
  (-> v .-selectedEntity (set! e)))

(defn track-entity! [v e]
  (-> v .-trackedEntity (set! e)))

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

(defn set-polyogon-position! [e pos])
  

(defn add-entity! [v x]
  (let [es (.-entities v)]
    (.add es x)))

;; var heading = Cesium.Math.toRadians(90);
;; var pitch = Cesium.Math.toRadians(-30);
;; viewer.zoomTo(wyoming, new Cesium.HeadingPitchRange(heading, pitch));

(defn radians [x] (Cesium.Math.toRadians x))

(def +default-offset+
  (Cesium.HeadingPitchRange.
   (radians 90)
   (radians -45)))

(defn zoom-to!
  ([v]
   (.zoomTo v (.-entities v)))
  ([v e]
   (.zoomTo v e))
  ([v e offset]
   (.zoomTo v e offset)))

;; viewer.flyTo(wyoming).then(function(result){
;;     if (result) {
;;         viewer.selectedEntity = wyoming;
;;     }
;; });

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
  
(defn set-height! [e h]
  (-> e .-polygon .-height (set! h)))

(defn set-material! [e m]
  (-> e .-polygon .-material (set! m)))

(defn extrude-height! [e h]
  (-> e .-polygon .-extrudedHeight (set! h)))

;;todo replace with hiccup variant...
(def wyoming-description
  "<img
  width='50%'
  style='float:left; margin: 0 1em 1em 0;'
  src='//cesiumjs.org/tutorials/Visualizing-Spatial-Data/images/Flag_of_Wyoming.svg'/>
<p>
  Wyoming is a state in the mountain region of the Western 
  United States.
</p>
<p>
  Wyoming is the 10th most extensive, but the least populous 
  and the second least densely populated of the 50 United 
  States. The western two thirds of the state is covered mostly 
  with the mountain ranges and rangelands in the foothills of 
  the eastern Rocky Mountains, while the eastern third of the 
  state is high elevation prairie known as the High Plains. 
  Cheyenne is the capital and the most populous city in Wyoming, 
  with a population estimate of 62,448 in 2013.
</p>
<p>
  Source: 
  <a style='color: WHITE'
    target='_blank'
    href='http://en.wikipedia.org/wiki/Wyoming'>Wikpedia</a>
</p>';")


(defn set-description! [e d]
  (-> e .-description (set! d)))

(defn tut
  ([spec]
   (let [v (-> @view :current)
         e (-> v (add-entity! (clj->js spec)))
         _ (zoom-to! v e)]
     e))
  ([] (tut wyoming-spec)))

(defn tut2 [e]
  (do (set-height! e 200000)
      (extrude-height! e 250000)
      e))

(defn tut3 [e]
  (set-description! e wyoming-description))

(defn tut4 [e]
  (let [heading (radians 90)
        pitch   (radians -30)]
    (-> @view
        :current
        (zoom-to! e (Cesium.HeadingPitchRange. heading pitch)))))

;;this doesn't work with polygons.
(defn tut5 [e]
  (do (set-position! e (degrees->cart3 [-107.724 42.68]))
      (-> @view :current (track-entity! e))))

(def point-spec
  {:name "Citizens Bank Park",
   :position (Cesium.Cartesian3.fromDegrees -75.166493, 39.9060534),
   :point {
           :pixelSize 5,
           :color Cesium.Color.RED,
           :outlineColor Cesium.Color.WHITE,
           :outlineWidth 2}
   ,
   :label {
           :text "Citizens Bank Park",
           :font "14pt monospace",
           :style Cesium.LabelStyle.FILL_AND_OUTLINE,
           :outlineWidth 2,
           :verticalOrigin Cesium.VerticalOrigin.BOTTOM,
           :pixelOffset    (Cesium.Cartesian2. 0 -9)}})

(defn ->point [lat long]
    {
     :position (Cesium.Cartesian3.fromDegrees lat long),
     :point {
             :pixelSize 5,
             :color Cesium.Color.RED,
             :outlineColor Cesium.Color.WHITE,
             :outlineWidth 2}
     ,})
 
(defn ->random-points [n]
  (for [[lat lng] (random-coords n)]
    (->point lat lng)))


;;tutorial only had // for th url, needed the full
;;https:// for the image to get grabbed.
(def billboard-spec
  (-> point-spec
      (dissoc :point)
      (assoc :billboard
             {:image "https://cesiumjs.org/tutorials/Visualizing-Spatial-Data/images/Philadelphia_Phillies.png"
              :width 64,
              :height 64})))

;;var citizensBankPark = viewer.entities.add();
(defn point-tutorial []
  (tut point-spec))

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
