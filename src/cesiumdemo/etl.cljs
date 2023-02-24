;;ns for wrangling loading from csv files and
;;the like.
(ns cesiumdemo.etl
  (:require #_[testdouble.cljs.csv :as csv] ;;produced messed up csv without setting cr+lf
            [goog.labs.format.csv :as gcsv]
            [semantic-csv.core :as sc]
            [cesiumdemo.parser :as parse]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cesiumdemo.entityatlas :as ea]
            [cesiumdemo.util :as u]
            [helins.interval.map :as imap])
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

(defn if-empty [v else]
  (if (empty? v) else v))

;;We can supply patches and icons via the data with extra fields. Origin, etc.
;;should already be taken care of.
(defn add-info [{:keys [UIC PATCH ICON] :as e}]
  (let [PATCH (if-empty PATCH nil)
        ICON  (if-empty ICON  nil)]
    (if (and PATCH ICON)
      e
      (if-let [u (ea/find-unit UIC)]
        (assoc e :PATCH  (or PATCH (u :Patch))
               :ICON   (or ICON (u :Icon)))
        (assoc e :PATCH "USARMY.jpg"
               e :ICON  "unknown.gif")))))

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
           (try [e (->> {:UIC e :SRC (get x :SRC) :COMPO (get x :CMP) :PATCH (get x :PATCH) :ICON (get x :ICON)
                         :bounds (entity-bounds xs) :moves (vec (sort-by :tstart xs))}
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
  (if-let [res (some-> geo ea/geo->location (select-keys [:lat :long]))]
    res
    (throw (ex-info (str [:unknown-location! geo]) {:in geo}))))

(defn move->visual-move [id uic
                         {:keys [tstart ALD LAD CRD RDD  ORIG_GEO POE_GEO POD_GEO PAX TOTAL_STONS]}]
  (let [start  (geo->lat-long ORIG_GEO)
        middle (geo->lat-long POE_GEO)
        dest   (geo->lat-long POD_GEO)
        origin->poe (- ALD 10)
        dstop    (- RDD tstart)
        dtransit (- ALD tstart)]
    {:id       id
     :uic      uic
     :start    [(start  :long) (start  :lat)  300000]
     :transit  [(middle :long) (middle :lat)  100000]
     :dest     [(dest   :long) (dest   :lat)  10000]
     :cstart   tstart
     :ctransit (+ tstart dtransit)
     :cstop    (+ tstart dstop)
     :dtransit dtransit
     :dstop    dstop
     :delay    (- RDD CRD)
     :pax         PAX
     :stons TOTAL_STONS}))

(defn entity->visual-moves [{:keys [UIC SRC COMPO PATCH ICON moves]}]
  (for [[i m] (map-indexed vector moves)]
    (-> (move->visual-move (str UIC "-" i) UIC m)
        (assoc :src SRC :compo COMPO :patch PATCH :icon ICON))))

(defn read-visual-moves [txt]
  (->> txt
       read-moves
       collect-entities
       vals
       (mapcat entity->visual-moves)))


;;we now want to analyze the moves....
;;primary analytic outputs:
;;a series of time, %cumulative equipment, %cumulative pax closures.
;;a series of time, %Late To Need
;;need total pax, total stons
(defn closures [moves]
  (let [tmin (atom 0)
        [ptot etot] (reduce (fn [[p e] {:keys [cstart pax stons]}]
                              (swap! tmin min cstart)
                              [(+ p pax) (+ e stons)]) [0 0] moves)]
    {:ptotal ptot
     :etotal etot
     :data  (vec (concat [[@tmin 0 0]]
                   (for [xs (->> moves
                                 (sort-by :cstop)
                                 (partition-by :cstop))]
                     (let [t       (:cstop (first xs))
                           [tp te] (reduce (fn [[p e] {:keys [pax stons]}]
                                             [(+ p pax) (+ e stons)])
                                           [0 0] xs)]
                       [t tp te]))))}))

(defn cumulative-closures [xs]
  (let [{:keys [ptotal etotal data]} (closures xs)
        p+     (fn [l r] (u/precision (+ l r) 4))
        accum (let [pacc (atom 0)
                    eacc (atom 0)]
                (fn [[t p e]]
                   [t (swap! pacc p+ p) (swap! eacc p+ e)]))]
    (->> data
         (map (fn [[t p e]]
                [t (/ p ptotal) (/ e etotal)]))
         (mapv accum)
         (hash-map :ptotal ptotal
                   :etotal etotal
                   :data))))

(defn ->samples [xs]
  (let [final (last xs)]
    (->> (partition 2 1 xs)
         (reduce (fn [acc [l r]]
                   (imap/mark acc (first l) (dec (first r)) l))
                 (imap/mark imap/empty (first final) (first final) final)))))

(defn intersect [sm t]
  (some-> sm
          (rsubseq <= t)
          first
          val))

(defn extents [sm]
  (let [ks (keys sm)]
    [(ffirst ks) (second (last ks))]))

(defn simple-agg
  [vs]
  (let [v (first vs)]
    (fn [t]
      (assoc v 0 t))))

(defn expand-samples
  ([agg-func s]
   (apply concat
          (for [[[from to] vs] (seq s)]
            (let [f (agg-func vs)]
              (map f (range from (inc to)))))))
  ([s] (expand-samples simple-agg s)))

;;provides a map we can lookup to get daily-stats.

;;(def res (make-remote-call "rawmoves.txt"))
;;(def mvs (-> @res read-visual-moves))
;;(def cs (-> mvs cumulative-closures))
;;(def ctrends (cumulative-closure-trends cs))
(defn cumulative-closure-trends [emoves]
  (let [{:keys [etotal ptotal data]} (cumulative-closures emoves)
        samples  (->samples data)
        trends   (->> samples expand-samples
                     (reduce (fn [acc [t e p]]
                               (assoc acc t #js[#js{:c-day t :trend "pax" :value p}
                                                #js{:c-day t :trend "equipment" :value e}])) {}))]
    {:extents (extents samples) :etotal etotal :ptotal ptotal :trends trends}))

;;this can be dumped into app-state.

;;we also want late-to-need over time.
;;from visual move records, we can infer the necessary information.
;;Group moves by uic (already done when we read entities).
;;If any piece of that entity is LTN, uic is ltn.
;;simplest is to just filter entity moves by positive delay,
;;group-by uic, since delays will be assumably a subset.

(defn cumulative-ltn-trends [emoves]
  (let [total-units (->> emoves (map :uic)  distinct count)
        late-units (->> emoves
                        (filter (comp pos? :delay))
                        (group-by :uic))
        total-late (count late-units)
        unit-finals (for [[uic moves] late-units]
                      (let [{:keys [cstop delay]} (->> moves (sort-by (comp - :cstop)) first)]
                        (- cstop delay)))
        p+          (fn [l r] (u/precision (+ l r) 4))
        samples     (->> unit-finals
                         sort
                         (partition-by identity)
                         (map (let [accum (atom 0)]
                                 (fn [xs]
                                   [(first xs) (swap! accum p+ (count xs))])))
                         (map (fn [[t n]] [t (u/precision (double (/ n total-units)) 4)]))
                         ->samples)

       [from to] (extents samples)
        trends   (->> samples
                      expand-samples
                      (reduce (fn [acc [t ltn]]
                                (assoc acc t #js[#js{:c-day t :trend "ltn" :value ltn}])) {}))]
    {:total-units total-units
     :total-ltn   total-late
     :percent-ltn (u/precision (/ total-late total-units) 4)
     :extents     (extents samples)
     :trends      trends}))

;;we can go entity-moves->
;;  cumulative closures
;;  cumulative %ltn
