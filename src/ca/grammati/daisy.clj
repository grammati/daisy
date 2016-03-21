(ns ca.grammati.daisy
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
            [clojure.string :as string])
  (:import [com.stuartsierra.component SystemMap]))


(defprotocol Lifecycle
  (-start [this])
  (-stop  [this]))

(defprotocol Documented
  (-doc [this]))


(def ^:dynamic *print-lifecycle?* true)

;;; plc == "print-life-cycle"
(defn- plc [& args]
  (when *print-lifecycle?*
    (println (apply str args))))

(defn started?
  "Returns true if the component is started."
  [c]
  (::started? c))

(defn system?
  "Returns true if the component is a SystemMap"
  [c]
  (instance? SystemMap c))

(defn start
  "Starts the component only if it is not already started.
  Prints what it's doing if *print-lifecycle?* is true."
  [c]
  (if (and (started? c) (not (system? c)))
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
  (if (or (started? c) (system? c))
    (do
      (plc "Stopping: " c " ... ")
      (assoc (-stop c) ::started? false))
    (do
      (plc "Skipping (already stopped): " c)
      c)))


;;; Default implementations
(extend-protocol Lifecycle
  SystemMap
  (-start [this] (-> this component/start (assoc ::started? true)))
  (-stop  [this] (-> this component/stop (assoc ::started? false))))

(extend-protocol Documented
  Object
  (-doc [_]))


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

(defprotocol ComponentDependsOn
  (-depends-on [this] "Returns a vector of keywords describing the other components that the given componenet depends on."))

(extend-protocol ComponentDependsOn
  Object
  (-depends-on [this] nil))

(defn- component-meta
  [type-name injected]
  `(extend-protocol ComponentDependsOn
     ~type-name
     (-depends-on [_#] ~(mapv keyword injected))))

(defmacro
  ^{:style/indent [1 :form :form [1]]}
  defcomponent
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

  Example:

  (defcomponent Foo
    \"This is a sweet component\"
    {:init-args [blah]
     :injected  [bar]
     :creates   [foo]}
    (start [this] (assoc this :foo (flarp blah bar)))
    (stop  [this] (unflarp! foo) (assoc this :foo nil))
    IFunky
    (funky [this] (funkify foo bar blah)))
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
     (component-factory type-name fn-name doc init-args injected)
     (component-meta type-name injected))))

(defn dependency-map [m]
  (into {} (for [[k c] m
                 :let [deps (-depends-on c)]
                 :when (seq deps)]
             [k deps])))

(defn system-map
  "Builds and returns a system-map containing dependency information
  extracted from the components in the given map.

 `defcomponent` expects you to declare, using the `:injected` option,
  a list of things that a component depends on. We can use this
  information to automatically generate the dependency map that is the
  second argument to component's `system-using` function.

  Note that this only works if you follow certain conventions: the
  sybol given in `:injected` must match the keyword used as the key in
  the system-map passed to this function.

  Example:
  ;; Given:
  (defcomponent Foo
    \"docstring\"
    {:injected [bar]}
    (start [this] ...)
    (stop [this] ...))
  (defcomponent Bar ...)

  ;; then:
  (daisy/system-map {:bar (new-bar) :foo (new-foo)})
  ;; is equvalent to:
  (component/system-using
    (component/map->System {:bar (new-bar) :foo (new-foo)})
    {:foo [:bar]})
  "
  [m]
  (component/system-using
    (component/map->SystemMap m)
    (dependency-map m)))


(defn- lifecycle-fn-wrapper [lifecycle-fn f]
  (fn [system-map]
    (let [[system error] (try
                           [(lifecycle-fn system-map) nil]
                           (catch clojure.lang.ExceptionInfo e
                             [(-> e ex-data :system) e]))]
      (f system)
      (when error
        (throw error)))))

(defn starter
  "Returns a function that will attempt to start a system, and will
  call the given callback, passing the resulting system, whether the
  start succeeds or not.

  If startup fails with an exception, it will be re-thrown after
  calling the callback."
  [f]
  (lifecycle-fn-wrapper start f))

(defn stopper
  "Returns a function that will attempt to stop a system, and will
  call the given callback, passing the resulting system, wether or not
  the stop succeeds.

  If stopping fails with an exception, it will be re-thrown after
  calling the callback."
  [f]
  (lifecycle-fn-wrapper stop f))

(defn start-to-var
  "Attempts to start a system, and puts the resulting system, whether
  fully started or not, into the given var via `alter-var-root`.

  The key value here is that if starting fails with an exception, the
  partially-started system map will still be stored in the var, before
  re-thowing."
  [v system-map]
  ((starter #(alter-var-root v (constantly %))) system-map))

(defn stop-var
  "Attempts to stop the system stored in the given var, and puts the
  resulting system, whether fully stopped or not, into the var via
  `alter-var-root`.

  The key value here is that if stopping fails with an exception, the
  partially-stopped system map will still be stored in the var, before
  re-thowing."
  [v]
  (when-let [system-map @v]
    ((stopper #(alter-var-root v (constantly %))) system-map)))


(defn pprint-system
  "Pretty-print a system-map in a way that won't make your face melt."
  [s]
  (let [shallow-map
        (fn [c]
          (into {} (for [[k v] c]
                     [k (if (and (record? v)
                                 (satisfies? component/Lifecycle v))
                          (str "component:" v)
                          v)])))]
    (clojure.pprint/pprint (into {} (for [[k v] s] [k (shallow-map v)])))))
