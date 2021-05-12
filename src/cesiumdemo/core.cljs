(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
   [cesiumdemo.network :as net]
   [cljs-bean.core :refer [bean ->clj ->js]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {}))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page

(defn cesium-root []
  (let [_ (js/console.log "Starting the cesium-root")]
    (fn []
      [:div {:class "fullSize"}
       [ces/cesium-viewer {:name "cesium"}]])))

(defn page [ratom]
  [:div "Welcome to reagent-figwheel."]
  [cesium-root])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn reload []
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))


(defn ->czml-packets [xs]
  (clj->js
   (concat [{:id "document"
             :name "CZML Point"
             :version "1.0"}]
           xs)))

(def czml
  (clj->js ;;have to recursively do this...
   [{:id "document"
     :name "CZML Point"
     :version "1.0"}
    {:id "point 1"
     :name "point"
     :position {:cartographicDegrees [-76.21848087 39.38973326  0]} ; #_ [39.38973326 -76.21848087 0]
     :point {:color {:rgba [255 255 255 255]}
             :outlineColor {:rgba [255 0 0 255]}
             :outlineWidth 4
             :pixelSize 20}
     }]))

(def compos #{"Army Active" "Army Guard" "Army Reserve"})
(def countries #{"United States"})

(defn origins []
  (->> d/bases
      (filter (fn [{:keys [status component country jointbase]}]
                (and (countries  country)
                     (or (compos component) (not (#{"N/A"} jointbase))))))))

(def colors {"Army Active"  [0   0   0   255]
             "Army Guard"   [192 192 192 255]
             "Army Reserve" [192 192 192 255]
             "AF Active"  [0   0   0   255]})

(defn ->czml-origin [{:keys [lat long sitename component] :as m}]
  {:id   sitename
   :name sitename
   :position {:cartographicDegrees [long lat  #_long 0]}
   :point {:color {:rgba (colors component)}
          ;:outlineColor {:rgba [255 0 0 255]}
          ;:outlineWidth 4
           :pixelSize 10}
   :properties m})

;;define poe, apoe..
(defn ->czml-poe [{:keys [lat long long-name] :as m}]
  {:id   long-name
   :name long-name
   :position {:cartographicDegrees [long lat 0]}
   :point {:color {:rgba [58, 158, 85 255] }
           :outlineColor {:rgba [0 0 0 255]}
           :outlineWidth 1
           :pixelSize 10}
   :properties m})

(defn forts! []
  (ces/load-czml! (->czml-packets (map ->czml-origin (origins)))))

(defn ports! []
  (ces/load-czml! (->czml-packets (map ->czml-poe d/ports))))

(defn states! []
  (ces/load-geojson! "ne_10m_us_states.topojson"))

;;need to define movements.
;;So an entity will show up and start moving.
;;This also implies a movement path.
;;Path thickness should increase as more entities moving along it.

(def connections
  (into {}
        (for [[k {:keys [from to]}] net/nearest-port]
          (let [dest {:lat (to :lat) :long (to :long)}]
            [[k (to :long-name)] {:start from :stop dest :distance (d/dist from to)}]))))

(defn jitter+ [n]
  (+ n (* (rand) 0.25)))

(defn jitter- [n]
  (- n (* (rand) 0.25)))

(defn equipment-movement [from to start stop]
  (let [arc (str from "->" to "-eq" )]
    ;;just draw a straight line between from and to for now.
    {:id   arc
     :name arc
     :polyline {:positions {:cartographicDegrees (mapv jitter+ [(start :long) (start :lat) 200
                                                               (stop  :long) (stop  :lat) 200])}
                :material  {:solidColor {:color {:rgba [255 0 0 200]}}}
                :width 1
                :clampToGround false}}))

(defn pax-movement [from to start stop]
  ;;just draw a straight line between from and to for now.
  (let [arc (str from "->" to "-pax" )]
    ;;just draw a straight line between from and to for now.
    {:id   arc
     :name arc
     :polyline {:positions {:cartographicDegrees (mapv jitter- [(start :long) (start :lat) 100000
                                                               (stop  :long) (stop  :lat) 200])}
                :material  {:solidColor {:color {:rgba [255, 165, 0, 200]}}}
                :width 1
                :clampToGround false}}))


(defn random-movement []
  (let [edge (rand-nth (keys connections))
        [from to] edge
        {:keys [start stop]} (connections edge)]
   [(equipment-movement from to start stop)
    (pax-movement from to start stop)]))

(defn random-movements [n]
  (->czml-packets (apply concat (repeatedly n random-movement))))

(defn layers! []
  (do (forts!)
      (ports!)
      (states!)))

(defn moves! []
  (ces/load-czml! (random-movements 3000)))

(defn tada! [] (do (layers!) (moves!)))


(defn ^:export main []
  (dev-setup)
  (reload))
