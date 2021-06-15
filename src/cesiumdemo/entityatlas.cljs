;;purpose here is to consolidate our entity lookup/geoloc
;;services so we can easily find things.  We could
;;outsource this to crux or datascript and push the
;;work off on indexdb or localstorage.  Might also provide
;;an API for adding new units to the atlas....
(ns cesiumdemo.entityatlas
  (:require [cesiumdemo.data :as d]))

(def origins
  (into {} (for [{:keys [origin code]} (vals d/locations)]
             [origin code])))

(defn find-unit [id]
  (d/units id))

(defn geo->location [code]
  (d/locations code))

(defn origin->location [origin]
  (some-> origin origins geo->location))

(defn unit->location [id]
  (some-> id find-unit :LOCATION origin->location))

(defn ensure-jpg [s]
  (clojure.string/replace s  #"png$|svg$|gif$" "jpg"))

(def unit-imagery  (for [{:keys [UIC Patch Icon]} (vals d/units)]
                    {:UIC UIC :Patch (ensure-jpg Patch) :Icon Icon}))

(def known-imagery (filterv #(not= (% :Patch) "USARMY.jpg") unit-imagery))
