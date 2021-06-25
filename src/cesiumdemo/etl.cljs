;;ns for wrangling loading from csv files and
;;the like.
(ns cesiumdemo.etl
  (:require #_[testdouble.cljs.csv :as csv] ;;produced messed up csv without setting cr+lf
            [goog.labs.format.csv :as gcsv]
            [semantic-csv.core :as sc]
            [cesiumdemo.parser :as parse]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cesiumdemo.entityatlas :as ea])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def ^:dynamic *offset* 10)

;;todo - see if google's parser is better/autodetcts crlf vs lf.
;;cljs.csv forces you to choose - we can detect as well.
(defn tsv->records [txt & {:keys [separator schema] :or {separator \tab schema {}}}]
  (->>
   (gcsv/parse txt nil separator) ;;works out of the box...
   #_(csv/read-csv txt :separator separator :newline :cr+lf)
   (sc/remove-comments)
   (sc/mappify)
   (sc/cast-with schema)))

(def move-schema
  {:TOTAL_STONS parse/str->double,
   :CMP #(get {"A" 1
               "G" 2
               "R" 3} % 0)
   :PAX parse/str->int
   :ALD parse/parse-day
   :CRD parse/parse-day
   :LAD parse/parse-day
   :RDD parse/parse-day})


;;simple repl development helper.
(defn make-remote-call
  ([endpoint]
   (let [res (atom nil)]
     (go (let [response (<! (http/get endpoint) )]
           ;;enjoy your data
           (reset! res (:body response))))
     res))
  ([endpoint f]
   (go (let [response (<! (http/get endpoint) )]
         ;;enjoy your data
         (f (:body response))))))


(defn read-moves [txt]
  (tsv->records txt :schema move-schema))

;;we want to extract a simple information model:
;;entity {name {:keys [id bounds moves patch icon]}}
;;we assume some initial move based on a simple factor,
;;like t = 10 (or something that makes sense)
;;tstart = rld - 10
;;moves then are
;;[tstart, origin]
;;[rld,    poe]
;;[lad,    pod]
;;[rdd,    final destination]
;;final desition is probably an abstract place.

(defn  bounds [xs]
  (->> xs
       (reduce (fn [[l r] x]
                 [(min (or l x) x) (max (or r x) x)]) [nil nil])))

(defn entity-bounds [moves]
  (->> moves
       (mapcat (juxt :tstart :RDD :CRD))
       bounds))

(defn add-info [{:keys [UIC PATCH ICON] :as e}]
  (if (and PATCH ICON)
      e
      (if-let [u (ea/find-unit UIC)]
        (assoc e :PATCH  (or PATCH (u :Patch))
                 :ICON   (or ICON (u :Icon)))
        (assoc e :PATCH "USARMY.jpg"
               e :ICON  "unknown.gif"))))

(defn collect-entities [xs]
  (let [off *offset*
        offset (if (fn? off)
                 (fn [] (off))
                 (fn [] off))]
  (->> (for [[e xs] (->> xs
                         (map (fn [{:keys [ALD] :as r}]
                                (assoc r :tstart (- ALD (offset)))))
                         (group-by :UIC))]
         (let [x (first xs)]
           (try [e (->> {:UIC e :SRC (get x :SRC) :COMPO (get x :CMP) :bounds (entity-bounds xs) :moves (vec (sort-by :tstart xs))}
                        add-info)]
                (catch js/Error e (throw (ex-info "bad-data!" {:e e :xs xs}))))))
       (into {}))))

;;we now have a map of
;; {uic {:keys [UIC SRC COMPO bounds moves]
;;       :where [moves [:keys [tstart
;;                             RDD
;;                             SRC
;;                             ORIG_NAME
;;                             TOTAL_STONS
;;                             UIC
;;                             POE_GEO
;;                             POE_NAME
;;                             ALD
;;                             POD_GEO
;;                             LAD
;;                             CRD
;;                             PAX
;;                             POD_NAME
;;                             ORIG_GEO
;;                             CMP]]]}}

;;we want to construct a dataset from these.

(defn move->transit [m]
  (assoc m :delay (- (m :RDD) (m :CRD))))
;;an entity is delayed if any of its moves are delayed.

#_
(->move [(start :long) (start :lat) 300000]
        [(stop :long) (stop :lat) 100000]
        (util/jitter-xyz [jx jy jz] destination)
        tstart
        origin->poe total :id  (str "beavis" (rand)) :move-types move-type
        :from-name from :to-name to)

(defn geo->lat-long [geo]
  (some-> geo ea/geo->location (select-keys [:lat :long])))

(defn move->visual-move [uic {:keys [tstart ALD LAD CRD RDD  ORIG_GEO POE_GEO POD_GEO PAX TOTAL_STONS]}]
  (let [start  (geo->lat-long ORIG_GEO)
        middle (geo->lat-long POE_GEO)
        dest   (geo->lat-long POD_GEO)
        origin->poe (- ALD 10)]
    {:id       uic
     :start    [(start  :long) (start  :lat)  300000]
     :transit  [(middle :long) (middle :lat)  100000]
     :dest     [(dest   :long) (dest   :lat)  10000]
     :cstart   tstart
     :ctransit (- ALD tstart)
     :cstop    (- RDD tstart)
     :delay    (- RDD CRD)
     :pax         PAX
     :total-stons TOTAL_STONS}))

(defn entity->visual-moves [{:keys [UIC SRC COMPO PATCH ICON moves]}]
  (for [[i m] (map-indexed vector moves)]
    (-> (move->visual-move (str UIC "-" i) m )
        (assoc :src SRC :compo COMPO :patch PATCH :icon ICON))))

(defn read-visual-moves [txt]
  (->> txt
       read-moves
       collect-entities
       vals
       (mapcat entity->visual-moves)))


;;we now want to analyze the moves....
