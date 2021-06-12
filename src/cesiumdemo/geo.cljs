;;Custom geocoding for offline stuff via
;;an in-memory database.  Maybe also add
;;entity queries to this...for now we
;;just include an atlas of locations.
(ns cesiumdemo.geo
  (:require [cesiumdemo.data :as d]))

(defprotocol IGeoResult
  (-geo-result [obj]))

(defprotocol IGeoCoder
  (-query [this q]))

(defn ->georesult [id lat lng & {:keys [height] :or {height 1000}}]
  #js{:displayName id
      :destination (Cesium.Cartesian3. lat lng height)})

(defn ->geocoder-service [geocoder]
  #js{:geocode
      (fn [input]
        (when-let [results (-query geocoder input)]
          (js/Promise.resolve (-geo-result results))))})
