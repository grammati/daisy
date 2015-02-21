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
            [clojure.pprint]
            [clojure.string :as string]))


(defprotocol Lifecycle
  (-start [this])
  (-stop  [this]))

(defprotocol Documented
  (-doc [this]))


;;; Default implementations
(extend-protocol Lifecycle
  Object
  (-start [this] (component/start this))
  (-stop  [this] (component/stop this)))

(extend-protocol Documented
  Object
  (-doc [_]))


(def ^:dynamic *print-lifecycle?* true)

;;; plc == "print-life-cycle"
(defn- plc [& args]
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
  (if (started? c)
    (do
      (plc "Skipping (already started): " c)
      c)
    (do
      (plc "Starting: " c " ... ")
      (assoc (-start c) ::started? true))))

(defn stop
  "Stops the component only if it is started.
  Prints what it's doing if *print-lifecycle?* is true."
  [c]
  (if (started? c)
    (do
      (plc "Stopping: " c " ... ")
      (assoc (-stop c) ::started? nil))
    (do
      (plc "Skipping (already stopped): " c)
      c)))


(defn- camel->lisp [s]
  (-> s (string/replace #"([a-z])([A-Z])" "$1-$2") string/lower-case))

(defn- component-record
  [type-name doc fields start stop body]
  `(defrecord ~type-name ~fields
     Lifecycle
     ~start
     ~stop
     Documented
     (~'-doc  [~'c] ~doc)
     component/Lifecycle
     (~'start [~'c] (start ~'c))
     (~'stop  [~'c] (stop ~'c))
     ~@body))

(defn- component-factory
  [type-name fn-name doc init-args injected]
  (let [args-map (zipmap (map keyword init-args) init-args)
        fn-doc   (str "Factory function for " (name type-name) ".\n\n" doc)]
   `(defn ~(symbol fn-name) ~fn-doc ~(vec init-args)
      (let [c# (~(symbol (str "map->" type-name)) ~args-map)]
        (component/using c# ~(mapv keyword injected))))))


(defmacro defcomponent
  "Macro to define a component.

  Expands into a defrecord and a factory function.

  The record implements com.stuartsierra.component/Lifecycle such that
  the start and stop methods call daisy/start and daisy/stop.

  Parameters are:
  `type-name` - symbol, the name of the record
  `doc` - a docstring
  `opts` - map - keys can include:
      :init-args - vector of symbols. Becomes the argument list of the factory fn.
      :injected - vector of symbols. Defines the components that this component
                  depends on, and expects to have injected before it starts.
      :creates - vector of symbols. Defines fields that this component will set
                 values for in its start method.
      :factory-name - symbol. Specifies a name for the factory fn. If not set, the
                      name will be generated as \"new-\" + (camel->lisp type-name)
  `start` and `stop` - method bodies for the lifecycle start and stop methods.
  `body` - anything here will be added to the body of the defrecord. You can use
           this to implement other protocols.
  "
  [type-name doc {:keys [init-args injected creates factory-name]} start stop & body]
  {:pre [(symbol? type-name)
         (= 'start (first start))]}
  (let [fields  (vec (concat init-args injected creates))
        fn-name (or factory-name (str "new-" (camel->lisp (name type-name))))
        start   (cons '-start (rest start))
        stop    (cons '-stop  (rest stop))]
    (list
     'do
     (component-record type-name doc fields start stop body)
     (component-factory type-name fn-name doc init-args injected))))



(defn pprint-system [s]
  (let [shallow-map
        (fn [c]
          (into {} (for [[k v] c]
                     [k (if (satisfies? component/Lifecycle v) (str "component:" v) v)])))]
    (clojure.pprint/pprint (into {} (for [[k v] s] [k (shallow-map v)])))))

