(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   #_[cljsjs.Cesium]
   [cesiumdemo.widget :as ces]))
   


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
  [:div
   "Welcome to reagent-figwheel."]
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


(defn ^:export main []
  (dev-setup)
  (reload))
