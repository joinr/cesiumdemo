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


(defn entities []
  (->> @view :current .-entities))

(defn add-entities! [v xs]
  (reduce (fn [acc e]
            (doto acc (.add e)))
          (.-entities v)
           xs))

(defn add-entity! [v x]
  (let [es (.-entities v)]
    (.add es x)))


(defn zoom-to! [v e]
  (.zoomTo v e))


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
