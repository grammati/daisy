(ns duke.daisy
  "Helpers for com.stuartsierra/component.

  Provides tools that help to eliminate some of the boilerplate
  involved in developing component-based systems.

  `defcomponent` : macro for defining component-record types
  
   - Prints start/stop messages
   - Keeps track of component state, and will not start an
     already-started component.

   - Also provides a function to pretty-print a system map."
  (:require [com.stuartsierra.component :as component]
            [clojure.string :as string]))


(def ^:dynamic *print-lifecycle?* true)

(defprotocol Lifecycle
  (-start [this])
  (-stop  [this]))

(defprotocol Documented
  (-doc [this]))


;;; plc == "print-life-cycle"
(defn- plc [& args]
  (when *print-lifecycle?*
    (print (apply str args))))
(defn- plcn [& args]
  (when *print-lifecycle?*
    (println (apply str args))))

(defn started?
  "Returns true if the component is started."
  [c]
  (::started? c))

(defn start
  "Starts the component only if it is not already started.
  Prints what it's doing if *print-lifecycle?* is true."
  [c]
  (if (::started? c)
    (do
      (plcn "Skipping (already started): " c)
      c)
    (do
      (plc "Starting: " c " ... ")
      (let [c (assoc (component/start c) ::started? true)]
        (plcn "success")
        c))))

(defn stop
  "Stops the component only if it is started.
  Prints what it's doing if *print-lifecycle?* is true."
  [c]
  (if (::started? c)
    (do
      (plc "Stopping: " c " ... ")
      (let [c (assoc (component/stop c) ::started? nil)]
        (plcn (str "success"))
        c))
    (do
      (plcn "Skipping (already stopped): " c)
      c)))


(defn- camel->lisp [s]
  (-> s (string/replace #"([a-z])([A-Z])" "$1-$2") string/lower-case))

(defn- component-record
  [type-name doc fields body]
  `(defrecord ~type-name ~fields
     Lifecycle
     (~'-start [~'c] (start ~'c))
     (~'-stop  [~'c] (stop ~'c))
     Documented
     (~'-doc   [~'c] ~doc)
     ~@body))

(defn- component-extension
  [type-name start stop]
  `(extend-protocol component/Lifecycle
     ~type-name
     ~start
     ~stop))

(defn- component-factory
  [type-name fn-name doc args]
  (let [args-map (zipmap (map keyword args) args)
        fn-doc   (str "Factory function for " (name type-name) ".\n\n" doc)]
   `(defn ~(symbol fn-name) ~fn-doc ~(vec args)
      (~(symbol (str "map->" type-name)) ~args-map))))


(defmacro defcomponent
  "FIXME"
  [type-name doc {:keys [init-args injected creates factory-name]} start stop & body]
  (let [fields  (vec (concat init-args injected creates))
        fn-name (or factory-name (str "new-" (camel->lisp (name type-name))))]
    (list
     'do
     (component-record type-name doc fields body)
     (component-extension type-name start stop)
     (component-factory type-name fn-name doc init-args))))



(defn pprint-system [s]
  (let [shallow-map
        (fn [c]
          (into {} (for [[k v] c]
                     [k (if (satisfies? component/Lifecycle v) k v)])))]
    (clojure.pprint/pprint (into {} (for [[k v] s] [k (shallow-map v)])))))

