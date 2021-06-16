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
  (clojure.string/replace s  #"png|svg|gif$" "jpg"))

(def patch-path
  (memoize (fn [p]
             (ensure-jpg (str "/icons/patches/" p)))))

(def icon-path
  (memoize (fn [p]
             (str "/icons/std/" p))))

(def unit-imagery  (for [{:keys [UIC Patch Icon]} (vals d/units)]
                    {:UIC UIC :Patch (ensure-jpg Patch) :Icon Icon}))

(def known-imagery (filterv #(not= (% :Patch) "USARMY.jpg") unit-imagery))
#_
(defn unit-icon-svg [patch  icon]
  (str
   "data:image/svg+xml,<svg width='400' height='200' xmlns='http://www.w3.org/2000/svg'>"
   "<image href='" (icon-path icon) "' x='0' y ='0' height='200' width='200'/>"
    "<image href='" (patch-path patch) "' x='200' y='0' height='200' width='200'/>"
    "</svg>"))
