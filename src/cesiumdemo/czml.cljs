;;pending NS for defining helpers to go from
;;EDN->CZML easily.
(ns cesiumdemo.czml)

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
