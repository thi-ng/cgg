(defproject thi.ng/cgg "0.1.0-SNAPSHOT"
  :description  "Cosine gradient designer for thi.ng/color"
  :url          "http://thi.ng/gdt"
  :license      {:name "Apache Software License"
                 :url  "http://www.apache.org/licenses/LICENSE-2.0"
                 :distribution :repo}

  :min-lein-version "2.6.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374" :exclusions [org.clojure/tools.reader]]
                 [thi.ng/geom "0.0.1062"]
                 [thi.ng/color "1.1.1"]
                 [cljs-log "0.2.2"]
                 [reagent "0.5.1"]]

  :plugins      [[lein-figwheel "0.5.2"]
                 [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild    {:builds
                 [{:id "dev"
                   :source-paths ["src"]
                   :figwheel true
                   :compiler {:main       thi.ng.cgg.core
                              :asset-path "js/compiled/out"
                              :output-to  "resources/public/js/compiled/app.js"
                              :output-dir "resources/public/js/compiled/out"
                              :source-map-timestamp true}}
                  {:id "prod"
                   :source-paths ["src"]
                   :compiler {:output-to     "resources/public/js/compiled/app.js"
                              :main          thi.ng.cgg.core
                              :optimizations :advanced
                              :pretty-print  false}}]}

  :figwheel     {:css-dirs ["resources/public/css"]})
