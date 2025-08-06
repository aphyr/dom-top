(ns dom-top.core
  (:require
   [cljs.pprint :refer [pprint]])
  (:require-macros [dom-top.core]))

(defrecord Retry [bindings])

(defn fcatch
  "Takes a function and returns a version of it which returns, rather than
  throws, exceptions.

      ; returns RuntimeException
      ((fcatch #(throw (js/Error. \"hi\"))))"
  [f]
  (fn wrapper [& args]
    (try (apply f args)
         (catch js/Error e e))))

(deftype Return [value]
  Object
  (toString [this]
    (str "(Return. " (pr-str value) ")")))

#_(defmethod print-method Return
  [^Return ret ^java.io.Writer w]
  (.write w (.toString ret)))
