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
   [cesiumdemo.util :as util]))

#_(set! *warn-on-infer* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {}))

;;Date/Time Junk
;;We have to convert between js/Date and cesium's julian
;;date quite a bit.  For the moment, we do stuff fairly manually.
;;We have the current time the app was launched and then
;;provide some convenience functions around it for adding days,
;;converting to julian, etc.

(def +now+ (new js/Date))

#_(defn add-days [from n]
  (let [res (new js/Date from)
        _  (.setDate res (+ (.getDate res ) n))]
    res))

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
;;packets.
(defn ->czml-packets
  ([packet-name xs]
   (clj->js
    (concat [{:id "document"
              :name packet-name
              :version "1.0"}]
            xs)))
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

(defn pax-movement [from to start stop]
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


(defn movement->growing [mv start stop]
  (let [t1     (str (->jd start))
        t2     (str (->jd stop))
        t2+    (str (->jd stop))
        t3     (str (->jd (add-days stop 365)))
        from   (str (gensym "from"))
        to     (str (gensym "target"))
        dynavail  (str t1 "/" t2)
        staticavail (str t2+ "/" t3)
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
        id     (mv :id)
        bbid (str "randmove" (rand))
        {:keys [Patch Icon]} (rand-nth ea/known-imagery)]
    [source
     target
     (-> mv
      (assoc-in [:polyline :positions] {:references [(str from "#position") (str to "#position")]})
      (assoc :availability dynavail :id (str id  "_dynamic")))
     ;;entity icons that follow the to of the move....
     {:id   bbid
      :name bbid
      :billboard {:image (ea/patch-path Patch)
                  :scale 0.35
                  :pixelOffset {:cartesian2 [0 0]}
                  :eyeOffset   {:cartesian [0 0 -500000]}}
      :position {:reference (str to "#position")}
      :availability dynavail
      :properties {:billboard-type "patch"}}
     {:id   (str bbid "src")
      :name (str bbid "src")
      :billboard {:image (ea/icon-path Icon)
                  :scale 1.0
                  :pixelOffset {:cartesian2 [63 0]}
                  :eyeOffset   {:cartesian [0 0 -500000]}}
      :position {:reference (str to "#position")}
      :properties {:billboard-type "icon"}
      :availability dynavail}
     (assoc mv :availability staticavail :id (str id "_static") :name (str (mv :name) "_static"))]))


(def +day-seconds+  86400)
(def +hour-seconds+ 3600)
(def ger [49.27	73.3])

(defn ->path [id availability moves-t-long-lat-height
              & {:keys [material width leadTime trailTime resolution epoch lerp-level]
                 :or {material {:polylineOutline {:color {:rgba [255, 0, 255, 255]},
                                                  :outlineColor {:rgba [0, 255, 255, 255]}
                                                  :outlineWidth 5}}
                      width 8
                      resolution +day-seconds+
                      lerp-level 0}}]
  (let [pos (util/->sampled-degrees moves-t-long-lat-height
              :scale resolution :epoch epoch :lerp-level lerp-level)]
  {:id id
   :name (str "path" id)
   :description "a random path!"
   :availability availability,
   :path {:material material
          :width    width,
          :leadTime leadTime
          :trailTime trailTime
          :resolution resolution}
   :position pos}))

(defn demo-path []
  (let [scale #(* % +day-seconds+)
        day-samples [0  -78.59   35.08   10000
                     10 -79.0052 35.1015 5000
                     22 13.4605  51.0804 0]
        scaled-samples (->> (for [[t y x z] (->> (partition 4 day-samples)
                                                 (util/lerpn 3))]
                              [(scale t) y x z])
                            (apply concat)
                            vec)
        tstart (->jd +now+)
        tstop  (add-jdays (->jd +now+) 22)
        avail  (str tstart "/" tstop)]
    (->path "demo-path"
            avail
            scaled-samples
            :epoch (str (->jd +now+))
            :leadTime 0)))

(defn bouncy-path [floor ceiling xs]
  (let [[start middle stop] (util/ensure-partitions 4 xs)
        [ts xs ys zs] start
        [tf xf yf zf] stop
        [t x y z]     (util/midpoint middle stop)
        [tt xx yy zz] (util/midpoint start middle)]
    [[ts xs ys ceiling] [tt xx yy floor] middle [t x y zf] [tf xf yf floor]]))

(defn geo-jitter [xs]
  (util/jitter-xyz [0.25 0.25 0] xs))

;;it would be nice to concat 2 paths.


(defn demo-path2 []
  (let [samples (geo-jitter [0  -78.59   35.08   30000
                             3  -79.0052 35.1015 30000
                             22 13.4605  51.0804 1000])
        tstart +now+
        tstop  (add-days +now+ 22)
        avail  (interval tstart tstop)]
    (-> (->path "demo-path"
                avail
                samples
                :epoch    (iso-str +now+)
                :leadTime 0
                :resolution +day-seconds+
                :width    1
                :material {:solidColor {:color        {:rgba [255 0 0 130]}}})
        (update :position util/interpolate :interp-degree 2 ))))

(defn ->arcing-path [id start-stop  moves
                     & {:keys [scale material width lerp-level]
                        :or {material {:solidColor
                                       {:color
                                        {:rgba [255 255 255 255]}}}
                             width 1
                             lerp-level 1}}]
  (let [[tstart tstop] (if (coll? start-stop)
                         start-stop
                         [start-stop nil])
        tstop (or tstop
                  (->> moves
                       (util/ensure-partitions 4)
                       (map first)
                       (reduce max)
                       (add-days tstart)))
        avail  (interval tstart tstop)]
    (-> (->path id
                avail
                moves
                :epoch (iso-str +now+)
                :leadTime 0
                :resolution +day-seconds+
                :material material
                :width width
                :lerp-level lerp-level)
        (update :position util/interpolate :interp-degree 5))))

(defn demo-path3 []
  (let [move1  (util/ensure-partitions 4 [0  -78.59   35.08   30000
                                          3  -78.59   35.08 1000])
        move2 [3.00  -78.59   35.08 1000
               12.5 -32.77235 43.09095 1000000
               22 13.4605  51.0804 1000]
        move2 (util/spline-degrees 5 (geo-jitter  move2))]
  (-> (->arcing-path "blah" +now+
                     (concat move1 move2)
                     #_[0  -78.59   35.08   30000
                        3  -78.59   35.08 1000
                        12.5 -32.77235 43.09095 1500000
                        22 13.4605  51.0804 1000]
                     :material {:solidColor {:color {:rgba [255 0 0 130]}}}
                     :lerp-level 3 :width 5)
      (assoc :billboard {:image "/icons/div.png"
                         :scale 0.1
                         ;:pixelOffset {:cartesian2 [0 0]}
                         :eyeOffset   {:cartesian [0 0 -500000]}
                         }))))



#_
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

(defn random-movement []
  (let [edge (rand-nth (keys connections))
        [from to] edge
        {:keys [start stop]} (connections edge)]
   [(equipment-movement from to start stop)
    (pax-movement from to start stop)]))

(defn time-based-movement
  [start duration mv]
  (let [pos1 (-> mv :polyline)]
    (movement->growing mv start  (add-days start duration))))

(defn random-time [init span]
  (str (->jd (add-days init (rand-int span)))
             "/"
             (->jd (add-days init 1000))))

(defn random-movements
  ([n]
   (->czml-packets "moves" (apply concat (repeatedly n random-movement))))
  ([start n]
   (->> (repeatedly n #(let [tstart   (add-days start (rand-int 180))
                             duration (rand-int 15)]
                         (mapcat (fn [mv] (time-based-movement tstart duration mv)) (random-movement))))
        (apply concat)
        (->czml-packets "moves"))))

(defn layers! []
  (do (states!)
      (forts!)
      (ports!)))

(defn moves! []
  (ces/load-czml! (random-movements 500)))

(defn timed-random-moves! []
  (ces/load-czml! (random-movements +now+ 500)))

(defn tada!       [] (do (layers!) (moves!)))
(defn tada-timed! [] (do (layers!) (timed-random-moves!)))

(defn ^js/Cesium.DataSourceCollection
  get-layers! []
  (-> (ces/current-view) .-dataSources))

(defn imagery-layers []
  (-> (ces/current-view) .-imageryLayers))

(defn layer-names []
  (for [^js/Cesium.DataSource v (-> (ces/current-view) .-dataSources .-_dataSources)]
    (.-name v))) 

(defn ^js/Cesium.EntityCollection
  get-layer! [id]
  (-> (get-layers!) (.getByName id) first))

(defn drop-layer! [id]
  (let [l   (get-layers!)
        tgt (first (.getByName l id))]
    (.remove l tgt true)))

(defn entities-in [layer]
  (-> (get-layer! layer)
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
  (drop-layer! "moves")
  (swap! app-state dissoc :entities)
  (v/rewind-samples! :flow-plot-view "c-day" 0))

(defn random-moves! []
  (timed-random-moves!)
  (derive-movement-stats!))


;;UI / Page
;;=========

(def viewer-options
  {:skyBox false
   :baseLayerPicker false
   :imageryProvider (-> local-layers :blue-marble)
   :geocoder false})

;;todo : figure out way to allow online toggle.
(def online-options
  {:skyBox false})

(defn cesium-root []
  (let [_ (js/console.log "Starting the cesium-root")]
    (fn []
      [:div.cesiumContainer {:class "fullSize"}
       [ces/cesium-viewer {:name "cesium" :opts viewer-options}]])))

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
    [legend]]
   [:div.header {:id "chart-root" :style {:position "absolute" :top "50%" :right "0%"}}
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
