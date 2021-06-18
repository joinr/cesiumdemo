;;pending NS for defining helpers to go from
;;EDN->CZML easily.
(ns cesiumdemo.czml
  (:require [cesiumdemo.time :as time]
            [cesiumdemo.util :as util]
            [goog.object :as gobject]))

;;one thing we want to do is have a simple conversion
;;for our time/epoch stuff.  So we can do date/time manipulation,
;;even interval definition in clojure, then convert to czml and let
;;the conversion make sure our iso8601 dats and times are set where
;;appropriate.

(defprotocol IEncodeCZML
  (-clj->czml [this]))


;; (def isokeys
;;   {:availability  time/interval
;;    :interval      time/interval
;;    :currentTime   time/-isostring
;;    :epoch         time/-isostring
;;    :cartesian           encode-cartesian
;;    :cartographicDegrees encode-cartesian
;;    :rgba
;;    :rgbaf
;;    })

;;if cartesian or cartographicDegrees exists as a sibling of
;;epoch, and we have multiple samples, 

(defn clj->czml
  "Recursively transforms ClojureScript values to CZML, identical to
   clj->js since it returns js objects, but it coerces EDN and js
   values for dates czml compatible ones for us.

  sets/vectors/lists become Arrays, Keywords and Symbol become Strings,
  Maps become Objects. Arbitrary keys are encoded to by `key->js`.
  Options is a key-value pair, where the only valid key is
  :keyword-fn, which should point to a single-argument function to be
  called on keyword keys. Default to `name`."
  [x & {:keys [keyword-fn]
        :or   {keyword-fn name}
        :as options}]
  (letfn [(keyfn [k] (key->js k thisfn))
          (thisfn [x] (cond
                        (nil? x) nil
                        (satisfies? IEncodeCZML x) (-clj->czml x)
                        (satisfies? IEncodeJS x)   (-clj->js x)
                        (keyword? x) (keyword-fn x)
                        (symbol? x) (str x)
                        (map? x) (let [m (js-obj)]
                                   (doseq [[k v] x]
                                     (gobject/set m (keyfn k) (thisfn v)))
                                   m)
                        (coll? x) (let [arr (array)]
                                    (doseq [x (map thisfn x)]
                                      (.push arr x))
                                    arr)
                        :else x))]
    (thisfn x)))


;; ;;we want to provide a really simple cljs way to define
;; ;;valid CZML...

;; ;;property values:

;; 5 => 5
;; {_ 5}      => {_ 5}
;; {_ string} => {_ string}


;; ;;naked collections
;; [x y z w]   =>  {:cartesian [x y z w]}

;; [x y z w
;;  x y z w]   =>  {:cartesian [x y z w
;;                             x y z w]}
;; [[x y z w]
;;  [x y z w]] =>  {:cartesian [x y z w
;;                              x y z w]}

;; ^:degrees|:cartographicDegrees
;;  [[t y x z]
;;   [t y z z]]   =>   {:degrees|:cartographicDegrees [t x y z t x y z]}

;; ^:degrees|:cartographicDegrees
;; [t y x z
;;  t y z z]   =>   {:degrees|:cartographicDegrees [t x y z t x y z]}

;; {:interval [from to]|intervalstring
;;  :number   5} =>  {:interval "2012-04-30T12:00:00Z/14:00:00Z"
;;                   :number 5}

;; ;;collections are inferred to be property intervals.
;; [{:interval [from to]|intervalstring
;;   :number 5}

;;  {:interval [from to]|intervalstring
;;   :number 5}]

;; {:degrees|:cartographicDegrees
;;    [[t y x z]
;;     [t y z z]]}   =>   {:cartographicDegrees [t x y z t x y z]}

;; {:cartesian [[t x y z]
;;              [t x y z]]} =>  {:cartesian [t x y z t x y z]}

;; {:degrees|:cartographicDegrees
;;  [t y x z
;;   t y z z]}     =>  {:cartographicDegrees [t x y z t x y z]}

;; {:cartesian [t x y z
;;              t x y z]}   =>  {:cartesian [t x y z t x y z]}



;; ;;infinite interval, collection implies cartesian property.
;;  [t x y z] => {:cartesian  [t x y z]}
