(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [com.stuartsierra.component :as component]))


(def system nil)

(def config {})

(defonce global-state (atom nil))

(defrecord ACmp [a-val]
  component/Lifecycle
  (start [this]
    this)
  (stop [this]
    this))
(defn new-a [] (map->ACmp {}))

(defrecord BCmp [b-val a]
  component/Lifecycle
  (start [this]
    (swap! global-state assoc :b this)
    this)
  (stop [this]
    (swap! global-state dissoc :b)))
(defn new-b [] (map->BCmp {}))

(defrecord CCmp [c-val b]
  component/Lifecycle
  (start [this]
    (throw (ex-info "Ooops!" {})))
  (stop [this]
    this))
(defn new-c [] (map->CCmp {}))

(defn new-dev-system []
  (component/system-using
   (component/map->SystemMap
    {:a (new-a)
     :b (new-b)
     :c (new-c)})
   {:b [:a]
    :c [:b]}))


(defn init []
  (alter-var-root #'system (constantly (new-dev-system))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start)
  :ok)

(defn reset []
  (stop)
  (refresh :after 'dev/go))



