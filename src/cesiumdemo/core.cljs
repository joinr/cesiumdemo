(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
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
     :position {:cartographicDegrees [-76.21848087 39.38973326  0] #_[-111.0 40.0 0]} ; #_ [39.38973326 -76.21848087 0]
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

(defn ^:export main []
  (dev-setup)
  (reload))
