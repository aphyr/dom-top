(defproject dom-top "1.0.8-SNAPSHOT"
  :description "Unorthodox control flow for Clojurists with masochistic sensibilities"
  :url "http://github.com/aphyr/dom-top"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[riddley "0.2.0"] ; For code-walking
                 [net.cgrand/macrovich "0.2.1"]]

  :aliases {"kaocha" ["with-profile" "+dev" "run" "-m" "kaocha.runner"]
            "test"   ["version"]}

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [criterium "0.4.6"]
                                  [lambdaisland/kaocha "1.66.1034"]
                                  [kaocha-noyoda "2019-06-03"]
                                  [philoskim/debux "0.8.2"]
                                  [hashp "0.2.1"]
                                  ]}}
  :test-selectors {:perf :perf
                   :default (fn [m] (not (or (:perf m))))})
