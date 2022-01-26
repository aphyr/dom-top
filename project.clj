(defproject dom-top "1.0.7"
  :description "Unorthodox control flow for Clojurists with masochistic sensibilities"
  :url "http://github.com/aphyr/dom-top"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[riddley "0.2.0"] ; For code-walking
                 ]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.3"]
                                  [criterium "0.4.6"]]}}
  :test-selectors {:perf :perf
                   :default (fn [m] (not (or (:perf m))))})
