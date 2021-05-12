;;Splitting out our stateful
;;container into another namespace,
;;so that interactive changes at the
;;repl don't reload the viewer.
(ns cesiumdemo.widget
  (:require  [reagent.core :as r]
             [cesiumdemo.cesium :as cesium]))

;;we'll maintain some state for our globe....
(defonce view
  (r/atom {}))

;;var viewer = new Cesium.Viewer('cesiumContainer');
;;need to add options....
(defn ->viewer [el {:keys [] :as opts}]
  (js/Cesium.Viewer. el (clj->js opts)))

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


;;stateful API for working with a "Single" current view....
(defn load-czml! [coll]
  (let [p (js/Cesium.CzmlDataSource.load coll)]
    (.add  (.-dataSources (@view :current))
           p)))

(defn load-kml! [path]
  (let [v (@view :current)
        p (js/Cesium.KmlDataSource.load path #js{:camera (.. v -scene -camera)
                                                 :canvas (.. v -scene -canvas)})]
    (.add  (.-dataSources (@view :current))
           p)))
