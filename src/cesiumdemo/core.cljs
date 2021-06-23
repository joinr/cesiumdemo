(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
   [cesiumdemo.entityatlas :as ent]
   [cesiumdemo.network :as net]
   [cesiumdemo.entityatlas :as ea]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [cesiumdemo.vega :as v]
   [cesiumdemo.time :as time :refer [->jd add-days iso-str interval]]
   [cesiumdemo.util :as util]
   [cesiumdemo.czml :as czml]
   [promesa.core :as p]))

#_(set! *warn-on-infer* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {:loaded false}))

;;Date/Time Junk
;;We have to convert between js/Date and cesium's julian
;;date quite a bit.  For the moment, we do stuff fairly manually.
;;We have the current time the app was launched and then
;;provide some convenience functions around it for adding days,
;;converting to julian, etc.

(def +now+ (new js/Date))

(def shared-clock (js/Cesium.ClockViewModel.))

(defn play! []
  (set! (.-shouldAnimate shared-clock) true))

(defn stop! []
  (set! (.-shouldAnimate shared-clock) false))

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
             "AF Active"    [0   0   0   255]})

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
                :material  {:solidColor {:color {:rgba [255 0 0 175]}}}
                :width 1
                :clampToGround false}
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
                 :material  {:solidColor {:color {:rgba [255, 165, 0, 175]}}}
                 :width 1
                 :clampToGround false}
      :properties {:move-type "pax"}}))

(def dummy-move [0  -78.59   35.08   30000
                 3  -79.0052 35.1015 1000
                 22 13.4605  51.0804 1000])

#_
(def mv (->move [-78.59   35.08   30000]
                [-79.0052 35.1015 1000]
                [13.4605  51.0804 1000]
                +now+
                3
                22
                :id "beavis"
                :move-types #{:pax :equipment}
                :from-name "bragg"
                :to-name   "nc"
                ))

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
        mult  (if (< (rand) 0.5) -1 1)
        mp    (->> (util/midpoint tr t)
                   ;;we can jitter the midpoint...
                   (util/jitter-txyz [(* mult 20) (* mult 20) 100000]))
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
                             :interpolationAlgorithm "LAGRANGE"}
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
      :billboard {:image (ea/patch-path Patch)
                  :scale 0.25
                  :pixelOffset {:cartesian2 [0 0]}
                  :eyeOffset   {:cartesian [0 0 -10000]}}
       :position {:reference target-pos}
      :availability dynavail
      :properties {:billboard-type "patch" :shared true :shared-avail sharedavail}}
     {:id   (str bbid "src")
      :name (str bbid "src")
      :billboard {:image (ea/icon-path Icon)
                  :scale 0.85
                  :pixelOffset {:cartesian2 [63 0]}
                  :eyeOffset   {:cartesian [0 0 -10000]}}
      :availability dynavail
      :position {:reference target-pos}
      :properties {:billboard-type "icon" :shared true :shared-avail sharedavail}}]
     )))

(defn shrink-icon  [r]
  (if (r :billboard)
    (case (-> r :properties :billboard-type)
      "icon"  (update-in r [:billboard :scale] * 0.5)
      "patch" (update r :billboard
                      (fn [{:keys [scale pixelOffset] :as r}]
                        (assoc r :scale (* 0.5 scale)
                              :pixelOffset {:cartesian2 [30 0]})))
      r)
    r))

(def ger [11.430040468408205	49.80008750153199 10000])

(defn random-move [& {:keys [tstart origin->poe poe->pod destination]
                      :or {destination ger}}]
  (let [edge      (rand-nth (keys connections))
        [from to] edge
        {:keys [start stop]} (connections edge)
        origin->poe (or origin->poe (rand-nth (range 1 11)))
        poe->pod    (or poe->pod (rand-nth (range 3 30)))
        total       (+ origin->poe poe->pod)
        tstart      (or tstart (time/add-days +now+ (rand-int 180)))
        mult        (if (> (rand) 0.5) -1 1)
        move-type   #{:pax} #_(rand-nth [#{:pax} #{:equipment} #{:pax :equipment}])]
    (->move [(start :long) (start :lat) 300000]
            [(stop :long) (stop :lat) 100000]
            (util/jitter-xyz [(* mult 2.5) (* mult 2.5) 0] destination)
            tstart
            origin->poe total :id  (str "beavis" (rand)) :move-types #{:pax :equipment}
            :from-name from :to-name to)))

(def last-first (atom nil))
(defn random-movements
  ([n]
   #_(->czml-packets "moves" (apply concat (repeatedly n random-move)))
   (apply concat  (repeatedly n random-move)))
  ([start n]
   (apply concat (repeatedly n #(random-move :tstart (add-days start (rand-int 180)))))))

(defn layers! []
  (do (states!)
      (forts!)
      (ports!)))

(defn moves! []
  (ces/load-czml! (random-movements 500)))

(defn timed-random-moves! []
  (let [rands  (random-movements +now+ 500)
        shared (->> rands
                    (filter (fn [r] (or (some-> r :properties :shared)
                                        (= (r :id) "document"))))
                    (map (fn [r]
                           (assoc r :availability (-> r :properties :shared-avail))))
                    (map shrink-icon))]
    ;;reverse order to ensure we don't skip time!
    (p/do! (ces/load-czml! (->czml-packets "moves" shared) :id :inset)
           (ces/load-czml! (->czml-packets "moves" rands) :id :current)
           :done)))

(defn tada!       [] (do (layers!) (moves!)))
(defn tada-timed! [] (do (layers!) (timed-random-moves!)))

(defn ^js/Cesium.DataSourceCollection
  get-layers! [& {:keys [id] :or {id :current}}]
  (-> id ces/get-view .-dataSources))

(defn imagery-layers [& {:keys [id] :or {id :current}}]
  (-> id ces/get-view .-imageryLayers))

(defn layer-names [& {:keys [id] :or {id :current}}]
  (for [^js/Cesium.DataSource v (-> id ces/get-view .-dataSources .-_dataSources)]
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
  (let [{:strs [pax equipment]}  (get @app-state :entities)]
    #js[#js{:c-day t :trend "equipment" :value (count (present-on-day t equipment))}
        #js{:c-day t :trend "pax"       :value (count (present-on-day t pax))}]))


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
  (v/rewind-samples! :flow-plot-view "c-day" 0))

(defn random-moves! []
  (p/do! (timed-random-moves!)
         (derive-movement-stats!)))


;;UI / Page
;;=========

(def viewer-options
  {:skyBox false
   :baseLayerPicker false
   :imageryProvider (-> local-layers :blue-marble)
   :geocoder false
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
          }))

;;todo : figure out way to allow online toggle.
(def online-options
  {:skyBox false})

(defn cesium-root []
  (let [_ (js/console.log "Starting the cesium-root")]
    (fn []
      [:div.cesiumContainer {:class "fullSize"}
       [ces/cesium-viewer {:name "cesium" :opts viewer-options}]])))

(defn cesium-inset []
  (let [_ (js/console.log "Starting the cesium-inset")]
    (fn []
      [:div.cesiumContainer {} #_{:class "fullSize"}
       [ces/cesium-viewer {:name "cesium" :opts inset-options :id :inset}]])))

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

#_
(defn ->button [id on-click label]
  [:button.cesium-button {:id id :on-click on-click}
   label])

(defn page [ratom]
  [:div
   [cesium-root]
   [:div {:id "c-day" :class "header" :style {:position "absolute" :top "0px" :left "45%" :font-size "xx-large"}}
    [:p {:style {:margin "0 auto"}}
     "C-Day: " @c-day]]
   [:div.controlPanel
    [:div
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
    [legend]]
   #_[:div.header {:id "inset-root" :style {:position "absolute" :bottom "53%" :right "0%" :width "600px" :height "300px"}}
    [:p {:style {:margin "0 auto"}} "Destination Inset"]
    [cesium-inset]]
   #_[:div.header {:id "chart-root" :style {:position "absolute" :top "50%" :right "0%"}}
    [v/vega-chart "flow-plot" v/equipment-spec #_v/area-spec]]
   [:div.header {:id "inset-root" :style {:position "absolute" :top "55%"    :right "0%" :width "500px" :height "250px"}}
    [:p {:style {:margin "0 auto"}} "Destination Inset"]
    [cesium-inset]]
   [:div.header {:id "chart-root" :style {:position "absolute" :bottom "48%"  :right "0%"}}
    [v/vega-chart "flow-plot" v/equipment-spec #_v/area-spec]]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn reload []
  (swap! app-state dissoc :entities)
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
