(ns cesiumdemo.vega
  (:require  cljsjs.vega
             cljsjs.vega-lite
             cljsjs.vega-embed
             [vega-tools.validate :refer [check]]
             [promesa.core :as p]
             [reagent.core :as r]
             [cljs-bean.core :refer [bean ->clj ->js]]
             [clojure.core.async :as a]))


(defn- wrap-chart-constructor
  [chart]
  (fn [params] (chart (clj->js params))))

(defn parse
  "Parse a Vega specification.
  Returns a Promesa promise that resolves to a chart constructor, or rejects
  with a parsing error."
  ([spec] (parse spec nil))
  ([spec config]
   (p/promise (fn [resolve reject]
                (js/vega.parse (clj->js spec) (clj->js config)
                                  (fn [error chart]
                                    (if error
                                      (reject error)
                                      (resolve (wrap-chart-constructor chart)))))))))

(defn validate
  "Validate a Vega specification.
  Return a Promesa promise that resolves to the spec itself, or rejects with a
  validation error."
  [spec]
  (p/promise (fn [resolve reject]
               (if-let [error (check spec)]
                 (reject error)
                 (resolve spec)))))

(defn validate-and-parse
  "Validate and parse a Vega specification.
  Returns a Promise promise that resolves to a chart constructor, or rejects
  with a validation/parsing error."
  ([spec] (validate-and-parse spec nil))
  ([spec config]
   (->> (validate spec)
        (p/mapcat #(parse % config)))))

(def vlSpec
   (clj->js {:$schema "https://vega.github.io/schema/vega-lite/v5.json",
    :data {:values [
                    {:a "C", :b 2},
                    {:a "C", :b 7},
                    {:a "C", :b 4},
                    {:a "D", :b 1},
                    {:a "D", :b 2},
                    {:a "D", :b 6},
                    {:a "E", :b 8},
                    {:a "E", :b 4},
                    {:a "E", :b 7}
                    ]
           },
    :mark "bar",
    :encoding {
               :y {:field "a", :type "nominal"},
               :x {
                   :aggregate "average",
                   :field "b",
                   :type "quantitative",
                   :axis {:title "Average of b"}}}}))


(def dummy-data
  (clj->js
   (vec (for [i (range 10)
              t  ["pax" "b"]]
          {:c-day i
           :trend t
           :value  (case t
                     "a" (* i 2)
                     (* i 0.5))}))))
(def area-spec
  (clj->js
   {"$schema" "https://vega.github.io/schema/vega-lite/v5.json",
    "width" "container", "height" "container",
    "data" #_{"url" "unemployment-across-industries.json"} {"values" dummy-data},
    "mark" "area",
    "encoding" {"x" {"field" "x"} #_{
                     "timeUnit" "yearmonth", "field" "date",
                     "axis" {"format" "%Y"}
                     },
                "y" {"field" "value" "aggregate" "sum"}#_{
                     "aggregate" "sum", "field" "count"
                     },
                "color" {
                         "field" "trend" #_"series",
                         "scale" {"scheme" "category20b"}
                         }}}))

(defn ->stacked-area-spec [x y trend & {:keys [init-data x-label y-label trend-label]}]
  (clj->js
   {;"$schema" "https://vega.github.io/schema/vega-lite/v5.json","
    "autosize" {
                "type" "pad",
                "resize" true
                 "contains" "padding"
                 }
    "width" #_"container" 300, "height" #_"container" 200,
    "data"  {"name"   "table"
             ;"values" init-data
             },
    "mark" "area",
    "encoding" {"x"  {"field" x},
                "y"  {"field" y "aggregate" "sum"}
                "color"  {
                         "field" trend #_"series",
                         "scale" {"scheme" "category20b"}
                                          }}}))

(def lightColor  "#fff")
(def  medColor  "#888")

(def dark-theme
  {:config {:background "rgba(42, 42, 42, 0.6)" #_"#333",

            :title {
                    :color lightColor,
                    :subtitleColor lightColor
                    },
            
            :style {
                    :guide-label {
                                  :fill lightColor,
                                  },
                    :guide-title {
                                  :fill lightColor,
                                  },
                    },
            
            :axis {:domainColor lightColor,
                   :gridColor medColor,
                   :tickColor lightColor}}})

(def equipment-spec
  (-> {:width "container" :height 200;;:width 600, :height 200,
       "autosize" {
                   "type" "pad",
                   "resize" true ;;maybe revisit this.
                   "contains" "padding"
                   }
       :title {:text "Total Equipment and Pax Moves By C-Day"
               :fontSize 22}
;       :background "rgba(42, 42, 42, 0.8)"
       :data
        {:name "table"
            #_ #_:values  [ {:c-day 0, :quantity 0, :trend "pax"}
                          {:c-day 0, :quantity 0, :trend "equipment"}]},
       :mark "area",
       :encoding  {:x  {:field "c-day" :type "quantitative"},
                      :y  {:field "value", :aggregate "sum"},
                      :color  {:field "trend",
                                  :type "nominal"
                                  :scale  {;:scheme "category20b"
                                              :domain ["equipment" "pax"]
                                              :range  ["#ffa500"   "#ff0000"]}
                                  :legend  {:direction "horizontal"
                                               :orient "bottom"
                                            :layout {:bottom {:anchor "middle"}}}}}
       #_#_:signals [
                   { 
                    :name "width", 
                    :value 0,
                    :on [{ 
                             :event { :source "window", :type "resize" }, 
                             :update "containerSize()[0]" 
                             } ]
                    }
                   ]}
      (merge dark-theme)
      clj->js))

(defn random-changes [n]
  (for [i (range n)]
    [{:c-day i :trend "pax" :value (rand-int 10)}
     {:c-day i :trend "equipment" :value (rand-int 10)}]))

(defn stringify [m]
  (cond
    (map? m)
    (into {} (for [[k v] m]
               [(str (name k)) (stringify v)]))
    (coll? m)
     (into (empty m) (map stringify m))
     (keyword? m)  (str (name m))
     :else m))

(def charts (r/atom {}))

(defn render!
  [k spec opts]
   (let [view (js/vegaEmbed k opts)]
     (.runAsync view)))

(defn vrender! [spec]
  (vega.View. (vega.parse spec) #js{:renderer "canvas" :container "#chart-root" :hover true :log-level vega.Warn}))

(defn vega-chart [name spec]
  (let [vw (keyword (str name "-view"))]
    (r/create-class
     {:display-name (str name)
      :reagent-render (fn [] [:div])
      :component-did-mount
      (fn [this]
        (let [promise (.then (js/vegaEmbed (r/dom-node this) spec)
                             (fn [view]
                               (swap! charts assoc vw view)
                               #_(.update view)))]
          ))
      #_:component-did-update
      #_(fn [this]
          (when-let [view (get @app-state vw)]
            (.update view)))
      #_:component-will-update
      #_(fn [this]
          (let [view (chart {:el (r/dom-node this)})
                _    (swap! app-state assoc :view view)]
            (.update view)))})))

(defn chart-root
  ([k chart-spec]
   [:div
    [vega-chart  k chart-spec]])
  ([] (chart-root "chart" area-spec)))


;; var changeSet = vega
;; .changeset()
;; .insert(valueGenerator())
;; .remove(function (t) {
;;                       return t.x < minimumX;
;;                       });
;; view.change('table', changeSet).run();

(defn ->changes
  ([rs tmax]
   (-> (new js/vega.changeset)
       (.insert rs)
       (.remove (fn [t]
                  (> (.-x t) tmax)))))
  ([rs]
   (-> (new js/vega.changeset)
       (.insert rs))))

(defn ->rewind [fld tmax]
  (-> (new js/vega.changeset)
      (.remove (fn [t]
                 (> (aget t fld) tmax)))))

;;(def changes (random-changes 10))
;;(def sets (for [cs changes] (->changes "table" (clj->js cs) (-> cs :c-day))))
;;(def vw (-> charts deref vals first .-view))
(defn testplot []
  (let [changes  (random-changes 10)
        sets (for [cs changes] (->changes  (clj->js cs) (-> cs :c-day)))
        vw (-> charts deref vals first .-view)]
    (doseq [c sets]
      (.run (.change vw "table" c)))))

(defn testrewind [tmax]
  (let [vw (-> charts deref vals first .-view)]
    (.run (.change vw "table" (->rewind "c-day" tmax)))))

(defn push-samples! [plot-name xs]
  (let [vw (or (some-> @charts (get plot-name) .-view) (throw (ex-info "unknown plot!" {:name plot-name})))
        cs (->changes xs)]
    (.run (.change vw "table" cs))))

(defn rewind-samples! [plot-name field bound]
  (let [vw (or (some-> @charts (get plot-name) .-view) (throw (ex-info "unknown plot!" {:name plot-name})))
        cs (->rewind field bound)]
    (.run (.change vw "table" cs))))

