(defproject cesiumdemo "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [reagent "0.7.0"]
                 [cljcolor "0.1.0-SNAPSHOT"]
                 #_[cesiumcljs "1.5.3-SNAPSHOT"]]

  :min-lein-version "2.5.3"

  :source-paths ["src/clj"]

  :plugins [[lein-cljsbuild "1.1.4"]]

  :clean-targets ^{:protect false} ["resources/public/js"
                                    "target"]

  :figwheel {:css-dirs ["resources/public/css"]
             #_:server-port #_8080}

  :profiles
  {:dev
   {:dependencies []

    :plugins      [[lein-figwheel "0.5.18" #_"0.5.15"]]}}
                   
  
  
  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs"]
     :figwheel     {:on-jsload "cesiumdemo.core/reload"}
     :compiler     {:main                 cesiumdemo.core
                    :optimizations        :none
                    :output-to            "resources/public/js/app.js"
                    :output-dir           "resources/public/js/dev"
                    :asset-path           "js/dev"
                    :source-map-timestamp true}}
    
    {:id           "min"
     :source-paths ["src/cljs"]
     :compiler     {:main            cesiumdemo.core
                    :optimizations   :advanced
                    :output-to       "resources/public/js/app.js"
                    :output-dir      "resources/public/js/min"
                    :elide-asserts   true
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}]})

    
