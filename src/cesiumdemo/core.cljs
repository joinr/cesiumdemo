(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
   [cesiumdemo.entityatlas :as ent]
   [cesiumdemo.etl :as etl]
   [cesiumdemo.network :as net]
   [cesiumdemo.entityatlas :as ea]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [cesiumdemo.vega :as v]
   [cesiumdemo.time :as time :refer [->jd add-days iso-str interval]]
   [cesiumdemo.util :as util]
   [cesiumdemo.czml :as czml]
   [promesa.core :as p]))

#_(set! *warn-on-infer* true)

(def default-state {:transit-jitter     [2 2 0]
                    :destination-jitter [2.5 2.5 0]
                    :home-icons         true
                    :shared-icons       true
                    :layout             :stacked
                    :random-move-count  500
                    :random-move-length 180})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom (assoc default-state :loaded false)))

(defn no-jitter! []
  (swap! app-state dissoc :transit-jitter :destination-jitter))

(defn no-transit-jitter! []
  (swap! app-state dissoc :transit-jitter))

(defn default-jitter! []
  (swap! app-state merge default-state))

;;Date/Time Junk
;;We have to convert between js/Date and cesium's julian
;;date quite a bit.  For the moment, we do stuff fairly manually.
;;We have the current time the app was launched and then
;;provide some convenience functions around it for adding days,
;;converting to julian, etc.

(def +now+ (new js/Date))

(def shared-clock
  (js/Cesium.ClockViewModel.))

(defn play! []
  (set! (.-shouldAnimate shared-clock) true))

(defn stop! []
  (set! (.-shouldAnimate shared-clock) false))

(defn set-day! [n]
  (set! (.-currentTime shared-clock)
        (add-days (.-startTime shared-clock) n)))

(defn add-jdays [^js/Cesium.JulianDate  from n]
  (let [res (.clone from)
        _   (js/Cesium.JulianDate.addDays res n res)]
    res))

#_(defn ^js/Cesium.JulianDate ->jd [d]
  (js/Cesium.JulianDate.fromDate d))

(def +jnow+ (->jd +now+))

;;state for the current time in days relative to now.
;;We may move this into the app-state or into reframe
;;with some views later. for now we just use global state
;;in ratoms.

(def c-time (reagent/atom 0))
(def c-init (reagent/atom (.-dayNumber (->jd +now+))))
(def c-day  (reagent/atom 0))

(defn current-day [^js/Cesium.JulianDate curr]
  (- (.-dayNumber curr) @c-init))

;;hook up our event listener to the current view's clock.
;;an option here could be to just store the event listener in
;;an atom and swap it out during reload.  This setup might
;;have multiple listeners during dev.
(defn listen-to-time! []
  (.addEventListener (.-onTick (ces/clock))
                     (fn [e] (let [t  (.-currentTime  (ces/clock))
                                   d-prev @c-day
                                   d  (current-day t)]
                               (when (not= d-prev d)
                                 (reset! c-day d))))))

;;coerces a map of maps into czml js objects in the packet format
;;cesium expects.  The packet name will determine, per cesium, which
;;entity collection recieves the updates / changes defined by the entity
;;packets.  nils are skipped.
(defn ->czml-packets
  ([packet-name xs]
   (clj->js
    (concat [{:id "document"
              :name packet-name
              :version "1.0"}]
            (filter identity xs))))
  ([xs] (->czml-packets (gensym "packet") xs)))

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
             "AF Active"    [0   0   0   255]
             :pax           [255 0 0 125]
             :equipment     [255, 165, 0, 125] })

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
   :position {:cartographicDegrees [long lat 100]}
   :point {:color {:rgba [58, 158, 85 255] }
           :outlineColor {:rgba [0 0 0 255]}
           :outlineWidth 1
           :pixelSize 20}
   :properties m})

(defn forts! []
  (ces/load-czml! (->czml-packets "forts" (map ->czml-origin (origins)))))

(defn ports! []
  (ces/load-czml! (->czml-packets "ports" (map ->czml-poe d/ports))))

(defn states! []
  (ces/load-geojson! "ne_10m_us_states.topojson"))

(defn countries! []
  (ces/load-geojson! "all-countries.geo.json" :id :inset
                     :style {:stroke Cesium.Color.BLACK
                             :fill  (js/Cesium.Color.GREY.withAlpha 1.0),
                             :strokeWidth 3}))

;;need to define movements.
;;So an entity will show up and start moving.
;;This also implies a movement path.
;;Path thickness should increase as more entities moving along it.

(def connections
  (into {}
        (for [[k {:keys [from to]}] net/nearest-port]
          (let [dest {:lat (to :lat) :long (to :long)}]
            [[k (to :long-name)] {:start from :stop dest :distance (d/dist from to)}]))))



;;Optimization:  Look at changing the polyline's ArcType to none,
;;possible much simpler line rendering, although it won't clamp to the
;;geodesic surface.
(defn equipment-movement [from to start stop]
  (let [arc (str from "->" to "-eq" (rand))]
    ;;just draw a straight line between from and to for now.
    {:id   arc
     :name arc
     :polyline {:positions {:cartographicDegrees (mapv util/jitter+ [(start :long) (start :lat) 200
                                                               (stop  :long) (stop  :lat) 200])}
                :material  {:solidColor {:color {:rgba  (colors :equipment)}}}
                :width 1
                :clampToGround false
                :arcType "NONE" #_js/Cesium.ArcType.NONE}
     :properties {:move-type "equipment"}}))

(defn pax-movement
  [from to start stop]
   ;;just draw a straight line between from and to for now.
   (let [arc (str from "->" to "-pax" (rand))]
     ;;just draw a straight line between from and to for now.
     {:id   arc
      :name arc
      :polyline {:positions {:cartographicDegrees (mapv util/jitter- [(start :long) (start :lat) 100000
                                                                      (stop  :long) (stop  :lat) 200])}
                 :material  {:solidColor {:color {:rgba (colors :pax)}}}
                 :width 1
                 :clampToGround false
                 :arcType  "NONE" #_js/Cesium.ArcType.NONE}
      :properties {:move-type "pax"}}))

(defn line->dynamic-line [mv start stop]
  (let [t1     (iso-str  start)
        t2     (iso-str  stop)
        t2+    (iso-str  stop)
        t3     (iso-str  (add-days stop 365))
        from   (str (gensym "from"))
        to     (str (gensym "target"))
        dynavail     (interval t1  (time/add-seconds stop -1)) ;#_(add-days t2 -1)
        staticavail (interval t2+ t3)
        {:keys [cartographicDegrees] :as m} (-> mv :polyline :positions)
        source {:id from
                :name from
                :availability dynavail
                :position {:cartographicDegrees (vec (take 3 cartographicDegrees))}
                :properties {:move-type "source"}}
        target {:id   to
                :name to
                :availability dynavail
                :position {:cartographicDegrees (vec (concat (into [t1] (take 3 cartographicDegrees))
                                                             (into [t2] (drop 3 cartographicDegrees))
                                                             (into [t3] (drop 3 cartographicDegrees))))
                           :interpolationAlgorithm "LAGRANGE"}
                :properties {:move-type "target"}}
        id     (mv :id)]
    [source
     target
     (-> mv
      (assoc-in [:polyline :positions] {:references [(str from "#position") (str to "#position")]})
      (assoc :availability dynavail :id (str id  "_dynamic")))
     (assoc mv :availability staticavail :id (str id "_static") :name (str (mv :name) "_static"))]))

;;break the move up into 2 pieces...
;;we have "to" which follows the positions interpolated over time.
;;then have 2 lines per pax/equip move type that are based on the "to" position,
;;but they are only shown while the lines are growing (dynamic availability).
;;Then the lines are switched with static variants for performance.
;;The billboard follows the "to" position the whole way through.
;;"to" can actually be the billboard.
(defn ->move [from transit to tstart dtransit dstop & {:keys [id imagery move-types from-name to-name]}]
  (let [id      (or id (str "-move" (rand)))
        id-pos  (str id "#position")
        bbid    (str id "-bb")
        from-id (str id "-from")
        to-id   (str id "-to")
        vec->lat-long (fn [[lng lat h]] {:lat lat :long lng})

        [pax-src pax-target pax-dynamic pax-static :as pm]
        (when (move-types :pax)
           (->  (pax-movement from-name to-name   (vec->lat-long from) (vec->lat-long  transit))
                (line->dynamic-line tstart (add-days tstart dtransit))))
        [eq-src eq-target eq-dynamic eq-static :as em]
        (when (move-types :equipment)
          (-> (equipment-movement from-name to-name  (vec->lat-long from) (vec->lat-long  transit))
              (line->dynamic-line tstart (add-days tstart dtransit))))

        {:keys [Patch Icon]} (or imagery
                                 (rand-nth ea/known-imagery))

        ttransit  (time/add-days tstart dtransit)
        tstop     (time/add-days tstart  dstop)

       moves (->> [from transit to]
                   (util/ensure-partitions 3)
                   (map (fn [t [lng lat h]]
                          [t lng lat h]) [0 dtransit dstop]))
        [f tr t] moves
        [jx jy jz]  (or (@app-state :transit-jitter) [0 0 0])
        mult  (if (< (rand) 0.5) -1 1)
        mp    (->> (util/midpoint tr t)
                   ;;we can jitter the midpoint...
                   (util/jitter-txyz [jx jy jz]))
        splined-path (util/spline-degrees 2 [tr mp t])
        moves (util/catvec (for [[dt lng lat h] (concat [f] splined-path) #_[f tr mp t]]
                             [(iso-str (time/add-days tstart dt)) lng lat h]))

        dynavail    (time/interval tstart ttransit)
        allavail    (time/interval tstart   (time/add-days tstop 365))
        staticavail (time/interval ttransit (time/add-days tstop 365))
        sharedavail (time/interval ttransit tstop)
        source {:id from-id
                :name from-id
                :availability dynavail
                :position   {:cartographicDegrees from}
                :properties {:move-type "source" :shared true}}
        target {:id to-id
                :name to-id
                :availability allavail
                :position   {:cartographicDegrees moves
                             #_#_:interpolationAlgorithm "LAGRANGE"}
                :properties {:move-type "target" :shared true}}
        from-pos   (str from-id "#position")
        target-pos (str to-id "#position")]
    (concat
     em
     pm
     [source
      target
      {:id   bbid
      :name bbid
       :billboard (when (@app-state :home-icons)
                    {:image (ea/patch-path Patch)
                     :scale 0.25
                     :pixelOffset {:cartesian2 [0 0]}
                     :eyeOffset   {:cartesian [0 0 -10000]}
                     :scaleByDistance {:NearFarScalar [1.5e2, 2.0, 1.5e7, 0.15]}})
       :position {:reference target-pos}
       :availability dynavail
       :properties {:billboard-type "patch" :shared (@app-state :shared-icons)  :shared-avail sharedavail}}
     {:id   (str bbid "src")
      :name (str bbid "src")
      :billboard (when (@app-state :home-icons)
                   {:image (ea/icon-path Icon)
                    :scale 0.85
                    :pixelOffset {:cartesian2 [63 0]}
                    :eyeOffset   {:cartesian [0 0 -10000]}
                    :scaleByDistance {:NearFarScalar [1.5e2, 2.0, 1.5e7, 0.15]}})
      :availability dynavail
      :position {:reference target-pos}
      :properties {:billboard-type "icon" :shared (@app-state :shared-icons)  :shared-avail sharedavail}}]
     (filter identity 
      [(when (move-types  :equipment)
        {:id   (str (gensym "eq") "transit")
         :name "equip transit"
         :path {:material  {:polylineOutline {:color        {:rgba (assoc (colors :equipment) 3 20)},
                                              :outlineColor {:rgba (assoc (colors :equipment) 3 20)}
                                              :outlineWidth 1
                                              }}
                :width    1,
                :leadTime 0
                :resolution (* czml/+day-seconds+ 10)}
         :availability sharedavail
         :position {:reference target-pos}
         :properties {:transit-path true :shared true :shared-avail sharedavail}})
      (when (move-types  :pax)
        {:id   (str (gensym "pax") "transit")
         :name "pax transit"
         :path {:material  {:polylineOutline {:color        {:rgba (assoc (colors :pax) 3 20)},
                                              :outlineColor {:rgba (assoc (colors :pax) 3 20)}
                                              :outlineWidth 1
                                              }}
                :width    1,
                :leadTime 0
                :resolution (* czml/+day-seconds+ 10)}
         :availability sharedavail
         :position {:reference target-pos}
         :properties {:transit-path true :shared true :shared-avail sharedavail}})])
     )))

(defn shrink-icon  [r]
  (if (r :billboard)
    (case (-> r :properties :billboard-type)
      "icon"  (update-in r [:billboard :scale] * 0.25)
      "patch" (update r :billboard
                      (fn [{:keys [scale pixelOffset] :as r}]
                        (assoc r :scale (* 0.25 scale)
                              :pixelOffset {:cartesian2 [60 0]})))
      r)
    r))

(def ger [11.430040468408205	49.80008750153199 10000])
(def nor [0.3138532  49.0677708 10000])
(def brest [-4.4860088 48.3905283 10000])

(defn random-move [& {:keys [tstart origin->poe poe->pod destination]
                      :or {destination brest}}]
  (let [edge      (rand-nth (keys connections))
        [from to] edge
        {:keys [start stop]} (connections edge)
        origin->poe (or origin->poe (rand-nth (range 1 11)))
        poe->pod    (or poe->pod (rand-nth (range 3 30)))
        total       (+ origin->poe poe->pod)
        tstart      (or tstart (time/add-days +now+ (rand-int 180)))
        mult        (if (> (rand) 0.5) -1 1)
        move-type   (rand-nth [#{:pax} #{:equipment} #{:pax :equipment}])
        [jx jy jz]  (or (@app-state :destination-jitter) [0 0 0])]
    (->move [(start :long) (start :lat) 300000]
            [(stop :long) (stop :lat) 100000]
            (util/jitter-xyz [jx jy jz] destination)
            tstart
            origin->poe total :id  (str "beavis" (rand)) :move-types move-type
            :from-name from :to-name to)))

;;load from a data-driven entity move.
(defn entity-move [{:keys [patch dstop start stons src icon cstop transit
                           cstart id pax dtransit dest compo delay ctransit]}]
  (let [tstart      (time/add-days +now+ cstart)
        [jx jy jz]  (or (@app-state :destination-jitter) [0 0 0])]
    (->move start
            transit
            (util/jitter-xyz [jx jy jz] dest)
            tstart
            dtransit
            dstop
            :id  id
            :move-types (->> [(and (pos? pax) :pax) (and (pos? stons) :equipment)]
                             (filter identity)
                             set)
            :from-name (str id "-origin") :to-name (str id "destination")
            :imagery {:Patch patch :Icon icon})))

(defn random-movements
  ([n]
   (apply concat  (repeatedly n random-move)))
  ([start n]
   (apply concat
    (repeatedly n
       #(random-move :tstart
          (add-days start (rand-int (get @app-state :random-move-length 180))))))))

(defn layers! []
  (do (states!)
      (forts!)
      (ports!)
      (countries!)))

(defn timed-random-moves! []
  (let [rands  (random-movements +now+ (get @app-state :random-move-count 500))
        pres   (filter (fn [r]
                         (not (some-> r :properties :transit-path))) rands)
        shared (->> rands
                    (filter (fn [r] (or (some-> r :properties :shared)
                                        (= (r :id) "document"))))
                    (map (fn [r]
                           (assoc r :availability (-> r :properties :shared-avail))))
                    (map shrink-icon))]
    ;;reverse order to ensure we don't skip time!
    (p/do! (ces/load-czml! (->czml-packets "moves" shared) :id :inset)
           (ces/load-czml! (->czml-packets "moves" pres #_rands) :id :current)
           :done)))

(defn ^js/Cesium.DataSourceCollection
  get-layers! [& {:keys [id] :or {id :current}}]
  (-> id ces/get-view .-dataSources))

(defn imagery-layers [& {:keys [id] :or {id :current}}]
  (-> id ces/get-view .-imageryLayers))

(defn layer-names [& {:keys [id] :or {id :current}}]
  (for [^js/Cesium.DataSource v (some-> id ces/get-view .-dataSources .-_dataSources)]
    (.-name v)))

(defn ^js/Cesium.EntityCollection
  get-layer! [k & {:keys [id] :or {id :current}}]
  (-> (get-layers! :id id) (.getByName k) first))


(defn drop-layer! [k & {:keys [id] :or {id :current}}]
  (let [l   (get-layers! :id id)
        tgt (first (.getByName l k))]
    (.remove l tgt true)))

(defn entities-in [layer & {:keys [id] :or {id :current}}]
  (-> (get-layer! layer :id id)
      .-_entityCollection
      .-values
      ))

(defn url->xyz [url]
  (js/Cesium.UrlTemplateImageryProvider. #js{:url url}))

(defn base-layer []
  (.get (imagery-layers) 0))

(defn xyz-provider [url]
  (url->xyz url))

(def local-layers
  {:blue-marble   (xyz-provider "layers/bm5/{z}/{x}/{y}.png")
   :virtual-earth (xyz-provider "layers/bingve/{z}/{x}/{y}.png")})

(defn set-imagery [provider]
  (let [layers   (imagery-layers)
        base     (base-layer)
        _        (.remove layers base)]
    (.addImageryProvider layers provider)))

(defn registered-moves []
  (let [keyf (memoize (fn [e] (-> e meta (get "move-type"))))]
    (->> (get-layer! "moves")
         .-_entityCollection
         vals
         (group-by keyf))))

(defn present [t ents]
  (let [t (->jd t)]
    (filter (fn [^js/Cesium.Entity e] (.isAvailable e t)) ents)))

;;naive stats.
(defn present-on-day [d ents]
  (present (add-days +now+ d) ents ))

(defn daily-stats [t]
  (if (@app-state :closure-trends)
    (get-in @app-state [:closure-trends :trends t])
    (let [{:strs [pax equipment]}  (get @app-state :entities)]
      #js[#js{:c-day t :trend "equipment" :value (count (present-on-day t equipment))}
          #js{:c-day t :trend "pax"       :value (count (present-on-day t pax))}])))


;;when we build a schedule, we want to capture the movement entities in the app state.
;;This speeds up querying.  Alterantely, we could just pre-compute all the availability
;;information too...

(defn derive-movement-stats! []
  (swap! app-state assoc :entities (registered-moves)))


(defn clear-moves! []
  (stop!)
  (drop-layer! "moves")
  (drop-layer! "moves" :id :inset)
  (swap! app-state dissoc :entities)
  (v/rewind-samples! :flow-plot-view "c-day" 0)
  (v/push-extents! :flow-plot-view  0 1)
  (set-day! 0))

(defn random-moves! []
  (p/do! (timed-random-moves!)
         (derive-movement-stats!)))

(defn set-finish! [start stop]
  (set! (.-stopTime shared-clock) (time/-julian (add-days +now+ stop)))
  (set! (.-clockRange shared-clock) js/Cesium.ClockRange.CLAMPED)
  (swap! app-state assoc :extents [start stop])
  (v/push-extents! :flow-plot-view start stop))

(defn timed-entity-moves! [emoves]
  (let [moves  (mapcat entity-move emoves)
        [tmin tmax]   (reduce (fn [[m mx] [s sp]]
                                [(min m s) (max mx sp)])
                                [0 0]
                                (map (juxt :cstart :cstop) emoves))
        pres   (filter (fn [r]
                         (not (some-> r :properties :transit-path))) moves)
        shared (->> moves
                    (filter (fn [r] (or (some-> r :properties :shared)
                                        (= (get r :id) "document"))))
                    (map (fn [r]
                           (assoc r :availability (-> r :properties :shared-avail))))
                    (map shrink-icon))]
    ;;reverse order to ensure we don't skip time!
    (p/do! (ces/load-czml! (->czml-packets "moves" shared) :id :inset)
           (ces/load-czml! (->czml-packets "moves" pres  ) :id :current)
           (set-finish! tmin tmax)
           :done)))

(defn load-moves! [moves]
  (p/do! (do (println [:updating-state!])
             (swap! app-state assoc :entity-moves moves
                    :ltn-trends (etl/cumulative-ltn-trends moves)
                    :closure-trends (etl/cumulative-closure-trends moves)))
         (timed-entity-moves! moves)
         (derive-movement-stats!)))

;;UI / Page
;;=========


;;// then you can use: Cesium.Math.toDegrees(currentCartographic.longitude)
(def bounds {:default   [-125.147327626 24.6163352675 -66.612171376 49.6742238918]
             :shifted   [-115.85193175578075 23.6163352675 -66.612171376 49.6742238918]
             :3d-us     {:position [-63215.181346188605 -9528933.76322208 6760084.984842104], :direction [0.007298723988826753 0.8268915851484712 -0.5623140003937158], :up [0.08733080420213787 0.5596533194968705 0.8241125485111495], :right [0.9961526284990382 -0.05512230389581595 -0.06812835201055821]}
             :3d-us-perspective {:position [-317622.8693122543 -9992858.180467833 4246834.783303648], :direction [0.031052476256112346 0.9868503489240992 -0.15862576255686647], :up [0.0037603323137679672 0.15858582933665222 0.9873380346337804], :right [0.9995106820936202 -0.03125577645796077 0.001213597444119795]}
             :3d-us-small {:position [-762505.075345366 -8709290.1490951 4043268.4155746778], :direction [0.09961461776873652 0.9857124352588807 -0.13582313095638476], :up [0.05184249751899356 0.1311751752861519 0.9900027418344055], :right [0.9936746365776786 -0.10566015504746376 -0.038034829532472586]}
             :inset-atlantic {:position [-5415797.494191807 4642953.337264422 10653133.091939844], :direction [0 0 -1], :up [-2.2204460492503126e-16 1 0], :right [1 2.2204460492503126e-16 0]}
             :us        [-129.2	20.2	-62.7	51.1]
             :us-europe [-125.8	16.7	71.7	55.2]
             :europe    [-12.6	34.7	53.8	60.3]
             :us-asia   [88.7	5.3	-67.7	48.5]})

(def viewer-options
  {:skyBox false
   :baseLayerPicker false
   :imageryProvider (-> local-layers :blue-marble)
   :geocoder false
   :resolutionScale 1.0
   :clockViewModel shared-clock})

(def inset-options
  (merge viewer-options
         {:sceneMode Cesium.SceneMode.COLUMBUS_VIEW
          :animation false
          :fullscreenButton false
          :homeButton        false
          :infoBox          false
          :timeline         false
          :navigationHelpButton false
          :sceneModePicker false
          :mapProjection (js/Cesium.WebMercatorProjection.)
          :resolutionScale 1.0
          }))

;;todo : figure out way to allow online toggle.
(def online-options
  {:skyBox false})

(defn cesium-root
  ([opts]
   (let [_ (js/console.log "Starting the cesium-root")]
     (fn []
       [:div.cesiumContainer opts
        [ces/cesium-viewer {:name "cesium"
                            :opts viewer-options
                            :extents (bounds :3d-us)}]])))
  ([] (cesium-root {:class "fullSize"})))

(defn cesium-inset []
  (let [_ (js/console.log "Starting the cesium-inset")]
    (fn []
      [:div.cesiumContainer {}
       [ces/cesium-viewer {:name "cesium" :opts inset-options :id :inset
                           :extents (bounds :inset-atlantic)}]])))

(defn legend []
  [:div.my-legend {:style {:margin-top "10px"}}
   [:div.legend-title "Legend"]
   [:div.legend-scale
    [:ul.legend-labels
     [:li "PAX Movement"]
     [:img {:src "icons/pax-move.png" :width "32" :height "32"}]
     [:li "Equipment Movement"]
     [:img {:src "icons/eq-move.png" :width "32" :height "32"}]
     [:li "POE"]
     [:img {:src "icons/poe.png" :width "32" :height "32"}]
     [:li "APOE"]
     [:img {:src "icons/apoe.png" :width "32" :height "32"}]
     [:li "Active Origin"]
     [:img {:src "icons/origin-ac.png" :width "32" :height "32"}]
     [:li "Guard/Reserve Origin"]
     [:img {:src "icons/origin-rc.png" :width "32" :height "32"}]]]])

(defn ->entry [label image]
  ^{:key label}
  [:div
   [:li #_{:style {:display "inline-block"}} label]
   [:img {#_#_:style {:display "inline-block"} :src image :width "32" :height "32"}]])

(defn flex-legend []
  [:div #_{:style {:margin-top "10px" }}
   [:div #_.legend-title "Legend"]
   [:div #_.legend-scale
    [:ul {:style {:display "flex"
                  :flex-flow "row wrap"
                  :justify-content "space-around"}}
     (->> ["PAX Movement" "icons/pax-move.png"
           "Equipment Movement" "icons/eq-move.png"
           "POE"  "icons/poe.png"
           "APOE"  "icons/apoe.png"
           "Active Origin"  "icons/origin-ac.png"
           "Guard/Reserve Origin"  "icons/origin-rc.png"]
          (partition 2)
          (map (fn [[l i]] (->entry l i))))]]])


(defn first-file
  [e]
  (let [target (.-currentTarget e)
        file (-> target .-files (aget 0))]
    (set! (.-value target) "")
    file))

(defn load-trends! [file]
  (let [reader (js/FileReader.)
        _ (println (str "loading trends:" (.-name file)))
        _ (clear-moves!)]
    (set! (.-onload reader)
          #(some-> % .-target .-result etl/read-visual-moves load-moves!))
    (.readAsText reader file)))

(defn file-input []
  [:input.cesium-button
   {:type "file" :id "file" :accept ".txt" :name "file" :on-change
    (fn [e] (swap! app-state assoc :file-to-load (load-trends! (first-file e))))}])

(defn rendering-options [s v]
  (merge s
         (case v
           :default    default-state
           :no-transit-icons {:transit-jitter     [2 2 0]
                              :destination-jitter [2.5 2.5 0]
                              :home-icons         true
                              :shared-icons       false}
           :no-icons       {:transit-jitter     [2 2 0]
                            :destination-jitter [2.5 2.5 0]
                            :home-icons         false
                            :shared-icons       false}
           :no-jitter    {:transit-jitter     [0 0 0]
                          :destination-jitter [0 0 0]
                          :home-icons         true
                          :shared-icons       true}
           :no-transit-jitter {:transit-jitter     [0 0 0]
                               :destination-jitter [2.5 2.5 0]
                               :home-icons         true
                               :shared-icons       true}
           (throw (ex-info "unknown rendering preset!" {:in v})))))

(defn ->drop-down [label id opts & {:keys [on-change]}]
  [:div
   [:p {:style {:font-size "70%"}} label]
   [:select.cesium-button {:id id :name id :on-change #(on-change (keyword (.. % -target -value)))}
    (for [[k v] opts]
      ^{:key k} [:option {:value (name k)} v])]])

(defn visual-options []
  (->drop-down "Rendering Options" "renderopts"
    {:default          :default
     :no-transit-icons :no-transit-icons
     :no-icons         :no-icons
     :no-jitter        :no-jitter}
    :on-change #(swap! app-state rendering-options %)))

(defn layout-options []
  (->drop-down "Page Layout" "pagelayout"
    {:stacked :stacked
     :overlay :overlay
     :tightly-stacked :tightly-stacked}
    :on-change #(swap! app-state assoc :layout % :layout-changed true)))

(defn overlay-page [ratom]
  [:div
   [cesium-root]
   [:div {:id "c-day" :class "header" :style {:position "absolute" :top "0px" :left "45%" :font-size "xx-large"}}
    [:p {:style {:margin "0 auto"}}
     "C-Day: " @c-day]]
   [:div.controlPanel
    [:div
     [:button.cesium-button {:style {:display "block"} :id "play" :type "button" :on-click #(play!)}
      "play"]
     [:button.cesium-button {:style {:display "block"} :id "stop" :type "button" :on-click #(stop!)}
      "stop"]
     [:button.cesium-button {:style {:display "block"} :id "clear-moves" :type "button" :on-click #(clear-moves!)}
      "clear-moves"]
     [:button.cesium-button {:style {:display "block"} :id "random-moves" :type "button" :on-click #(random-moves!)}
      "random-moves"]]
    [:button.cesium-button {:style {:display "block"} :id "demo" :type "button" :on-click
                            #(p/do! (println "loading moves")
                                    (random-moves!)
                                    (println "done")
                                    (println "loading-images")
                                    (when-not (@app-state :loaded)(p/delay 2000))
                                    (swap! app-state assoc :loaded true)
                                    (println "done")
                                    (play!))}
     "demo"]
    [file-input]
    [visual-options]
    [layout-options]
    [legend]]
   [:div.header {:id "inset-root" :style {:position "absolute" :top "55%"    :right "0%" :width "500px" :height "250px"}}
    [:p {:style {:margin "0 auto"}} "Destination Inset"]
    [cesium-inset]]
   [:div.header {:id "chart-root" :style {:position "absolute" :bottom "48%"  :right "0%" :width "300px" :height "200px"}}
    [v/vega-chart "flow-plot" v/line-equipment-spec]]])

(defn stacked-page [ratom]
  [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
   [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                   :justify-content "space-between"
                   :font-size "xxx-large"}}
     [:p {:style {:margin "0 auto" :text-align "center" }}
      "Origin"]
     [:p {:id "c-day" :style {:margin "0 auto" :text-align "center" }}
      "C-Day: " @c-day]
     [:p {:style {:margin "0 auto" :text-align "center" }}
     "Transit"]]
   [:div  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                   :justify-content "space-between"}}
    [:div {:style {:flex "0.50" :max-width "50%"}}
     [cesium-root]]
    [:div {:style  {:flex "0.50" :max-width "50%"}}
     [cesium-inset]]]
   [:div.header {:id "chart-root" :style {;:display "flex" ;"inline-block"
                                          :height  "auto" #_"100%" :width "100%"} #_{:position "absolute" :bottom "48%"  :right "0%"}}
    [:div {:width "100%" :height  "auto" #_"100%"}
     [v/vega-chart "flow-plot" v/line-equipment-spec #_v/area-spec]]]
   [flex-legend]
   [:div.flexControlPanel {:style {:display "flex" :width "100%" :height "auto" #_"50%"}}
    [:button.cesium-button {:style {:flex "1"} :id "play" :type "button" :on-click #(play!)}
     "play"]
    [:button.cesium-button {:style {:flex "1"} :id "stop" :type "button" :on-click #(stop!)}
     "stop"]
    [:button.cesium-button {:style {:flex "1"} :id "clear-moves" :type "button" :on-click #(clear-moves!)}
     "clear-moves"]
    [:button.cesium-button {:style {:flex "1"} :id "random-moves" :type "button" :on-click #(random-moves!)}
     "random-moves"]
    [:button.cesium-button {:style {:flex "1"} :id "demo" :type "button" :on-click
                            #(p/do! (println "loading moves")
                                    (random-moves!)
                                    (println "done")
                                    (println "loading-images")
                                    (when-not (@app-state :loaded)(p/delay 2000))
                                    (swap! app-state assoc :loaded true)
                                    (println "done")
                                    (play!))}
     "demo"]
    [file-input]
    [visual-options]
    [layout-options]]])

(defn tightly-stacked-page [ratom]
  [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
   [:div {:id "c-day" :class "header" :style {:font-size "xx-large"}}
    [:p {:style {:margin "0 auto" :text-align "center" }}
     "C-Day: " @c-day]]
   [:div {:style {:flex 1  :width "100%" :align-self "center"}}
     [cesium-root]]
   [:div {:style  {:flex 1 :width "100%" :align-self "center"}}
    [cesium-inset]]
   [:div {:id "chart-root" :style {:height "auto"   #_"auto" #_"100%" :min-width "100%"}}
    [v/vega-chart "flow-plot" v/line-equipment-spec]]
   [:div.flexControlPanel {:style {:display "flex" :width "100%" :height "auto" #_"50%"
                                   :flex-flow "row wrap"}}
    [:button.cesium-button {:style {:flex "0.5"} :id "play" :type "button" :on-click #(play!)}
     "play"]
    [:button.cesium-button {:style {:flex "0.5"} :id "stop" :type "button" :on-click #(stop!)}
     "stop"]
    [:button.cesium-button {:style {:flex "0.5"} :id "clear-moves" :type "button" :on-click #(clear-moves!)}
     "clear-moves"]
    [:button.cesium-button {:style {:flex "0.5"} :id "random-moves" :type "button" :on-click #(random-moves!)}
     "random-moves"]
    [:button.cesium-button {:style {:flex "0.5"} :id "demo" :type "button" :on-click
                            #(p/do! (println "loading moves")
                                    (random-moves!)
                                    (println "done")
                                    (println "loading-images")
                                    (when-not (@app-state :loaded)(p/delay 2000))
                                    (swap! app-state assoc :loaded true)
                                    (println "done")
                                    (play!))}
     "demo"]
    [file-input]
    [visual-options]
    [layout-options]]])

(defn ensure-layers! [obj]
  (let [res obj
        _   (when (@app-state :layout-changed)
              (swap! app-state dissoc :entities :layout-changed))
        _   (when (empty? (layer-names))
              (p/future (layers!)))]
    res))

(defn page [ratom]
  (let [layout (@ratom :layout)]
    (case layout
      :overlay (ensure-layers! (overlay-page ratom))
      :stacked (ensure-layers! (stacked-page ratom))
      :tightly-stacked (ensure-layers! (tightly-stacked-page ratom))
        [:p (str "unknown layout!" layout)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn reload []
  (reset! app-state default-state)
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))

;;Main
;;====

(defn ^:export main []
  (ces/set-extents! -125.147327626 24.6163352675 -66.612171376 49.6742238918)
  (dev-setup)
  (reload)
  (layers!)
  (listen-to-time!)
  (add-watch c-day :plotting
             (fn [k r oldt newt]
               (if (< newt oldt)
                 (v/rewind-samples! :flow-plot-view "c-day" newt)
                 (v/push-samples! :flow-plot-view (daily-stats newt))))))
