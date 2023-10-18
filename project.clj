(defproject dom-top "1.0.9"
  :description "Unorthodox control flow for Clojurists with masochistic sensibilities"
  :url "http://github.com/aphyr/dom-top"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[riddley "0.2.0"]] ; For code-walking
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [org.clj-commons/primitive-math "1.0.1"]
                                  [criterium "0.4.6"]]}}
  :test-selectors {:perf :perf
                   :focus :focus
                   :default (fn [m] (not (or (:perf m))))})
