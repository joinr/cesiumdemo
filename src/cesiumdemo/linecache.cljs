;;a ns for defining a primitive that defines
;;time-varying cesium polyline collections.
(ns cesiumdemo.linecache
  (:require [helins.interval.map :as imap]
            [clojure.set :as s]))

(defn value [x]
  (some-> x .getValue))

(defn color->material [color]
  (let [material (js/Cesium.Material.fromType "Color")
        _         (set! (.-color (.-uniforms material)) color)]
    material))

(defn line->values [poly & {:keys [show] :or {show false}}]
  #js{:positions (value (aget poly "positions"))
      :width     (value (aget poly "width"))
      :material  (-> (value (aget poly "material")) .-color color->material)
      :arcType   (value (aget poly "arcType"))
      :show      show})

(defprotocol IDynamic
  (show [this t]))

;;lines are a polylinecollection obj.
;;they are indexed by insertion order,
;;so we need to retain a map of id->idx.

;;at any given time (days) t, we need to
;;be able to show only lines that are visible
;;on t.  We should hide any lines that are not
;;visible.  Naive approach is to blast through
;;all lines and determine if their availability
;;interval intersects t, and toggle their show.
;;Given that we the 3K lines, we do this every frame,
;;maybe it's undesirable?

(defn set-line! [pc idx vis]
  (let [p (.get pc idx)]
    (set! (.-show p) vis)
    pc))

(defn show-line! [pc idx]
  (set-line! pc idx true))

(defn hide-line! [pc idx]
  (set-line! pc idx false))

(defn get-line [pc idx]
  (.get pc idx))

(defrecord linecache [^:mutable lines ^:mutable intervals ^:mutable t]
  IDynamic
  (show [this tnext]
    (let [curr     (get intervals t)
          nxt      (get intervals tnext)
          same     (clojure.set/intersection curr nxt)
          ons      (clojure.set/difference  nxt  same)
          offs     (clojure.set/difference  curr same)]
      (doseq [idx ons]
        ;;show polylines
        (set! (.-show (.get lines idx)) true))
      (doseq [idx offs]
        ;;show polylines
        (set! (.-show (.get lines idx)) false))
      (set! t tnext)
      this)))

(defn new-cache []
  (->linecache (js/Cesium.PolylineCollection.) imap/empty 0))

(defn add-line! [pc pline from to]
  (let [lines    (.-lines  pc)
        new-idx  (.-length lines)
        ints     (.-intervals pc)
        intervals (imap/mark ints from (inc to) new-idx)]
    (.add lines (line->values (.-_polyline pline)))
    (set! (.-intervals pc) intervals)
    pc))
