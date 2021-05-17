(ns cesiumdemo.core
  (:require
   [reagent.core :as reagent]
   [cesiumdemo.widget :as ces]
   [cesiumdemo.cesium :as c]
   [cesiumdemo.data :as d]
   [cesiumdemo.network :as net]
   [cljs-bean.core :refer [bean ->clj ->js]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {}))

(def +now+ (new js/Date))

(defn add-days [from n]
  (let [res (new js/Date from)
        _  (.setDate res (+ (.getDate res ) n))]
    res))

(defn add-jdays [from n]
  (let [res (.clone from)
        _   (js/Cesium.JulianDate.addDays res n res)]
    res))

(defn ->jd [d]
  (js/Cesium.JulianDate.fromDate d))

(def +jnow+ (->jd +now+))

(defn ->button [id on-click label]
  [:button.cesium-button {:id id :on-click on-click}
   label])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page

(def c-time (reagent/atom 0))
(def c-init (reagent/atom (.-dayNumber (->jd +now+))))
(def c-day  (reagent/atom 0))

(defn current-day [curr]
  (- (.-dayNumber curr) @c-init))

(defn listen-to-time! []
  (.addEventListener (.-onTick (ces/clock))
                     (fn [e] (let [t  (.-currentTime  (ces/clock))
                                   d-prev @c-day
                                   d  (current-day t)]
                               (when (not= d-prev d)
                                 (reset! c-day d))))))

(defn cesium-root []
  (let [_ (js/console.log "Starting the cesium-root")]
    (fn []
      [:div.cesiumContainer {:class "fullSize"}
       [ces/cesium-viewer {:name "cesium" :opts {:skyBox false}}]])))

(declare clear-moves!)
(declare random-moves!)


(defn legend []
  [:div.my-legend {:style {:margin-top "10px"}}
   [:div.legend-title "Legend"]
   [:div.legend-scale
    [:ul.legend-labels
     [:li "PAX Movement"]
     [:li "Equipment Movement"]
     [:li "POE"]
     [:li "Active Origin"]
     [:li "Guard/Reserve Origin"]]]])

(defn page [ratom]
  [:div
   [cesium-root]
   [:div.controlPanel
    [:button.cesium-button {:id "clear-moves" :type "button" :on-click #(clear-moves!)}
     "clear-moves"]
    [:button.cesium-button {:id "random-moves" :type "button" :on-click #(random-moves!)}
     "random-moves"]
    [:div "C-Day: " @c-day]
    [legend]]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn reload []
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))


(defn ->czml-packets
  ([packet-name xs]
   (clj->js
    (concat [{:id "document"
              :name packet-name
              :version "1.0"}]
            xs)))
  ([xs] (->czml-packets (gensym "packet") xs)))

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
                :clampToGround false}}))

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
                :clampToGround false}}))

(defn movement->growing [mv start stop]
  (let [t1     (str (->jd start))
        t2     (str (->jd stop))
        t3     (str (->jd (add-days stop 365)))
        from   (str (gensym "from"))
        to     (str (gensym "target"))
        avail  (str t1 "/" t3)
        {:keys [cartographicDegrees] :as m} (-> mv :polyline :positions)
        source {:id from
                :name from
                :availability avail
                :position {:cartographicDegrees (vec (take 3 cartographicDegrees))}}
        target {:id   to
                :name to
                :availability avail
                :position {:cartographicDegrees (vec (concat (into [t1] (take 3 cartographicDegrees))
                                                             (into [t2] (drop 3 cartographicDegrees))
                                                             (into [t3] (drop 3 cartographicDegrees))))
                           :interpolationAlgorithm "LAGRANGE"}}]
    [source
     target
     (-> mv
      (assoc-in [:polyline :positions] {:references [(str from "#position") (str to "#position")]})
      (assoc :availability avail))]))


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

(defn get-layers! []
  (-> @ces/view :current .-dataSources))

(defn imagery-layers []
  (-> @ces/view :current .-imageryLayers))

(defn layer-names []
  (for [v (-> @ces/view :current .-dataSources .-_dataSources)] (.-name v)))

(defn get-layer! [id]
  (-> (get-layers!) (.getByName id) first))

(defn drop-layer! [id]
  (let [l   (get-layers!)
        tgt (first (.getByName l id))]
    (.remove l tgt true)))

(defn url->xyz [url]
  (js/Cesium.UrlTemplateImageryProvider. #js{:url url}))

(defn base-layer []
  (.get (imagery-layers) 0))

(defn xyz-provider [url]
  (url->xyz url))

(def local-layers
  {:blue          (xyz-provider "layers/bm5/{z}/{x}/{y}.png")
   :virtual-earth (xyz-provider "layers/bingve/{z}/{x}/{y}.png")})

(defn set-imagery [provider]
  (let [layers   (imagery-layers)
        base     (base-layer)
        _        (.remove layers base)]
    (.addImageryProvider layers provider)))

(defn ^:export main []
  (ces/set-extents! -125.147327626 24.6163352675 -66.612171376 49.6742238918)
  (dev-setup)
  (reload)
  (layers!)
  (listen-to-time!))

(defn clear-moves! []
  (drop-layer! "moves"))
(defn random-moves! []
  (timed-random-moves!))


