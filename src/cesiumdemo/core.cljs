(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
   [cesiumdemo.entityatlas :as ent]
   [cesiumdemo.network :as net]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [cesiumdemo.vega :as v]))

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

(defn add-days [from n]
  (let [res (new js/Date from)
        _  (.setDate res (+ (.getDate res ) n))]
    res))

(defn add-jdays [^js/Cesium.JulianDate  from n]
  (let [res (.clone from)
        _   (js/Cesium.JulianDate.addDays res n res)]
    res))

(defn ^js/Cesium.JulianDate ->jd [d]
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

#_ 
(def czml
  (clj->js ;;have to recursively do this...
   [{:id "document"
     :name "CZML Point"
     :version "1.0"}
    {:id "point 1"
     :name "point"
     :position {:cartographicDegrees [-76.21848087 39.38973326  0]} ; #_ [39.38973326 -76.21848087 0]
     :point {:color {:rgba [255 255 255 255]}
             :outlineColor {:rgba [255 0 0 255]}
             :outlineWidth 4
             :pixelSize 20}
     }]))

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
             "AF Active"  [0   0   0   255]})

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

(defn jitter+ [n]
  (+ n (* (rand) 0.25)))

(defn jitter- [n]
  (- n (* (rand) 0.25)))

(defn equipment-movement [from to start stop]
  (let [arc (str from "->" to "-eq" )]
    ;;just draw a straight line between from and to for now.
    {:id   arc
     :name arc
     :polyline {:positions {:cartographicDegrees (mapv jitter+ [(start :long) (start :lat) 200
                                                               (stop  :long) (stop  :lat) 200])}
                :material  {:solidColor {:color {:rgba [255 0 0 175]}}}
                :width 1
                :clampToGround false}
     :properties {:move-type "equipment"}}))

(defn pax-movement [from to start stop]
  ;;just draw a straight line between from and to for now.
  (let [arc (str from "->" to "-pax" )]
    ;;just draw a straight line between from and to for now.
    {:id   arc
     :name arc
     :polyline {:positions {:cartographicDegrees (mapv jitter- [(start :long) (start :lat) 100000
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
        id     (mv :id)]
    [source
     target
     (-> mv
      (assoc-in [:polyline :positions] {:references [(str from "#position") (str to "#position")]})
      (assoc :availability dynavail :id (str id  "_dynamic")))
     (assoc mv :availability staticavail :id (str id "_static") :name (str (mv :name) "_static"))]))


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
  (ces/load-czml! (random-movements 3000)))

(defn timed-random-moves! []
  (ces/load-czml! (random-movements +now+ 3000)))

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
