(defproject dom-top "1.0.8-SNAPSHOT"
  :description "Unorthodox control flow for Clojurists with masochistic sensibilities"
  :url "http://github.com/aphyr/dom-top"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[riddley "0.2.0"] ; For code-walking
                 ; For ASM generation. We're pulling in an old version so it
                 ; doesn't conflict with core.async. Since the ONLY thing we
                 ; need this for is generating simple classes with no methods,
                 ; might be worth dropping this and using clojure.asm instead.
                 [insn "0.2.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [org.clj-commons/primitive-math "1.0.0"]
                                  [criterium "0.4.6"]]}}
  :test-selectors {:perf :perf
                   :focus :focus
                   :default (fn [m] (not (or (:perf m))))})
