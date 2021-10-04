(defproject cesiumdemo "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [reagent "0.7.0"]
                 [cljcolor "0.1.0-SNAPSHOT"]
                 [cljsjs/vega "5.17.0-0"]
                 [cljsjs/vega-lite "4.17.0-0"]
                 [cljsjs/vega-embed "6.14.2-0"]
                 [metosin/vega-tools "0.2.0"]
                 [funcool/promesa "6.0.1"]
                 [testdouble/clojurescript.csv "0.5.1"]
                 [semantic-csv "0.2.1-alpha1"]
                 #_[lambdaisland/fetch "1.0.33"]
                 [cljs-http "0.1.46"]
                 [org.clojure/core.async "1.3.618"]
                 [io.helins/interval "1.0.0-beta0"]]
  :profiles
  {:dev
   {:dependencies [[org.clojure/clojurescript "1.10.844"]
                   [com.bhauman/figwheel-main "0.2.13"]
                   [cljs-bean "1.7.0"]
                   ;; exclude on Windows
                   #_[com.bhauman/rebel-readline-cljs "0.1.4"]]
    :resource-paths ["target"]
    :clean-targets ^{:protect false} ["target"]}}
  ;; remove the string "trampoline" below if you are using Windows
  :aliases {"fig:build" [#_"trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig"       [#_"trampoline" "run" "-m" "figwheel.main"]})
