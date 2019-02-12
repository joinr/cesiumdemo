;;a collection of the entity creation
;;API tutorials from the official
;;cesium website.
(ns cesiumdemo.tutorials
  (:require [cesiumdemo.cesium :as ces 
             :refer [red black degrees->cart3 add-entity! zoom-to! set-height!
                     extrude-height! set-description! radians set-position! track-entity!]]
                     
            [cesiumdemo.widget :as w :refer [view]]))


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


