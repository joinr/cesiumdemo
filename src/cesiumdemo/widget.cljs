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

(defn ^js/Cesium.Viewer get-view [id]
  (some-> view deref id))

(defn ^js/Cesium.Viewer current-view []
  (get-view :current))

(defn set-extents! [x1 y1 x2 y2]
  (let [extent (js/Cesium.Rectangle.fromDegrees x1 y1 x2 y2)]
    (set! js/Cesium.Camera.DEFAULT_VIEW_RECTANGLE  extent)
    (set! js/Cesium.Camera.DEFAULT_VIEW_FACTOR 0)))

;;var viewer = new Cesium.Viewer('cesiumContainer');
;;need to add options....
(defn ->viewer [el {:keys [] :as opts}]
  (js/Cesium.Viewer. el (clj->js opts)))

(defn ^js/Cesium.Clock clock [& {:keys [id] :or {id :current}}]
  (-> view deref (get id) .-clock))

;;basic reagent component.
(defn cesium-viewer [{:keys [name opts id] :or {id :current}}]
  (let [vw (keyword (str name "-view"))]
    (r/create-class
     {:display-name (str name)
      :reagent-render (fn [] [:div])
      :component-did-mount
      (fn [this]
        (let [v (->viewer (r/dom-node this) opts)
              _ (swap! view assoc id v)]
          v))})))


;;stateful API for working with a "Single" current view....
(defn load-czml! [coll & {:keys [id] :or {id :current}}]
  (let [p (js/Cesium.CzmlDataSource.load coll)
        v  (@view id)]
    (.add  (.-dataSources v)
           p)
    p))

(defn load-kml! [path & {:keys [id] :or {id :current}}]
  (let [v (get @view id)
        p (js/Cesium.KmlDataSource.load path #js{:camera (.. v -scene -camera)
                                                 :canvas (.. v -scene -canvas)})]
    (.add  (.-dataSources v)
           p)
    p))

(defn load-geojson! [path & {:keys [stroke fill strokeWidth id] :as style :or {id :current}}]
  (let [style (if style style
                  {:stroke Cesium.Color.BLACK
                   :fill  (js/Cesium.Color.GREY.withAlpha 0.5),
                   :strokeWidth 3})
        v (@view id)
        p (js/Cesium.GeoJsonDataSource.load path #js{:camera (.. v -scene -camera)
                                                     :canvas (.. v -scene -canvas)
                                                     :stroke (style :stroke)
                                                     :fill (style :fill)
                                                     :strokeWidth (style :strokeWidth)} style)]
    (.add  (.-dataSources v)
           p)
    p))
