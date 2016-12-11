(defproject finn-chamber "0.1.0-SNAPSHOT"
  :description "Finn and the Enchanted Chamber"
  :url "https://github.com/plexus/finn-chamber"

  :license {:name "Mozilla Public License 2.0"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [cljsjs/phaser "2.6.1-0"]
                 [binaryage/devtools "0.8.3"]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.1"]
                                  [figwheel-sidecar "0.5.8"]]}}

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]

              :figwheel { :on-jsload "finn-chamber.core/on-reload" }

              :compiler {:main finn-chamber.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/dev.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true
                         :preloads [devtools.preload]}}
             {:id "min"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/js/compiled/prod.js"
                         :main finn-chamber.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel {:css-dirs ["resources/public/css"]})
