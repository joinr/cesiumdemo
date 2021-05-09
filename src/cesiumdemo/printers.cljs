(ns cesiumdemo.printers)

;;tbd 
#_(extend-protocol IPrintWithWriter
   Cesium.Cartesian2
   (-pr-writer [new-obj writer _]
     (write-all writer "#Cesium.Cartesian2 \"" (:details new-obj) "\"")))
