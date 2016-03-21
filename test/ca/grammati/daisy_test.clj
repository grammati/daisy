(ns ca.grammati.daisy-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [ca.grammati.daisy :as daisy :refer [defcomponent]]))


(defcomponent DeadSimple
  "This is the simplest possible component - it has no state and no
  dependencies"
  {}
  (start [this] this)
  (stop [this] this))

(deftest test-basics
  (testing "defcomponent defines factory function"
    (is (new-dead-simple)))

  (let [c (new-dead-simple)]
    (testing "factory function returns a component"
      (is (satisfies? component/Lifecycle c))
      (is (satisfies? daisy/Lifecycle c)))
    (testing "component type has docstring attached (available via protocol)"
      (is (= "This is" (subs (daisy/-doc c) 0 7)))))

  (testing "factory fn has docstring"
    (is (= "Factory" (subs (-> #'new-dead-simple meta :doc) 0 7))))

  (testing "component can be started with daisy/start"
    (let [c1 (new-dead-simple)
          c2 (daisy/start c1)]
      (is (daisy/started? c2))
      (is (not (daisy/started? c1)))

      (testing "start does not re-start"
        (is (identical? c2 (daisy/start c2))))

      (testing "component can be stopped"
        (let [c3 (daisy/stop c2)]
          (is (not (daisy/started? c3))))))))


(defcomponent InitArgsOnly
  "A component that has some state that must be set in its
  constructor."
  {:init-args [a-value]}
  (start [this] this)
  (stop [this] this))

(deftest test-init-args
  (testing "factory sets init-args on component"
    (is (= 23 (:a-value (new-init-args-only 23))))))


(defprotocol Foo
  (foo [this]))

(defcomponent MyApp
  "Component that uses all the features of defcomponent."
  {:init-args    [a]
   :injected     [b x]
   :creates      [c]
   :factory-name new-cool-app}
  (start [this]
    (assoc this :c (str a b)))
  (stop [this]
    (assoc this :c nil))
  Foo
  (foo [this] (str a b c))

  Object
  (toString [this] "MyApp"))

(defcomponent Db
  "Database component"
  {:init-args [conn-str]
   :creates   [conn]}
  (start [this]
    (assoc this :conn {:fake-conn conn-str}))
  (stop [this]
    (assoc this :conn nil)))

(defcomponent Thing
  "Some thing"
  {:injected [b]}
  (start [this] this)
  (stop [this] this))

(deftest test-app
  (let [s-map (component/map->SystemMap
               {:app (new-cool-app "aaa")
                :b   (new-db "jdbc://blah")
                :x   (new-thing)})
        sys   (component/start s-map)]
    (is sys)
    (component/stop sys)))

(deftest test-system-map
  (let [m (daisy/system-map
            {:b   (new-dead-simple)
             :x   (new-thing)
             :app (new-cool-app 99)})
        s (component/start m)]
    (is (:b (:x s)))
    (is (:b (:app s)))))

(deftest test-required-injections
  ;; Make sure an error is thrown if you mess up the keys
  (let [m (daisy/system-map
            ;; Use a non-matching key, so the ":b" that :app requires
            ;; will not be found (we called it :not-b instead, oops).
            {:not-b (new-dead-simple)
             :x     (new-thing)
             :app   (new-cool-app 99)})]
    (is (thrown? Exception (component/start m)))))


(defrecord SomeOtherThing []
  component/Lifecycle
  (start [this] (assoc this :state :happy))
  (stop [this] (assoc this :state :sad)))

(deftest non-daisy-components
  (let [sys (-> {:thing (new-thing)
                                :b (->SomeOtherThing)
                 :x (new-thing)}
                daisy/system-map
                daisy/start)]
    (is (daisy/started? sys))
    (is (= :happy (:state (:b sys))))
    (let [sys (daisy/stop sys)]
      (is (not (daisy/started? sys)))
      (is (= :sad (:state (:b sys)))))))


(defcomponent FragileThing
  "Blows up on start"
  {:init-args [val]
   :injected  [zzz]
   :creates   [yyy]}
  (start [this]
    (assoc this :yyy (if (odd? val) (/ 1 0) 42)))
  (stop [this]
    (dissoc this :yyy)))

(def test-system nil)

(deftest test-lifecycle
  (testing "happy path"
    (let [sys (daisy/system-map {:app (new-cool-app 33)
                                 :b   (new-dead-simple)
                                 :x   (new-thing)})]
      (daisy/start-to-var #'test-system sys)
      (is test-system)
      (is (daisy/started? test-system))
      (daisy/stop-var #'test-system)
      (is (not (daisy/started? test-system)))))

  (testing "partial start"
    (let [sys (daisy/system-map {:app (new-thing)
                                 :b   (new-fragile-thing 7)
                                 :zzz (new-dead-simple)})]
      (is (thrown? Exception (daisy/start-to-var #'test-system sys)))
      (is test-system)
      (is (not (daisy/started? test-system)))
      (is (daisy/started? (:zzz test-system)))
      (is (not (daisy/started? (:b test-system))))
      (is (not (daisy/started? (:app test-system))))

      (daisy/stop-var #'test-system)
      (doseq [c [test-system (:app test-system) (:b test-system) (:zzz test-system)]]
        (prn c)
        (is (not (daisy/started? c))))))

  (testing "restart"
    (-> {:app (new-thing)
         :b   (new-dead-simple)}
        daisy/system-map
        daisy/start
        daisy/stop
        daisy/start)))
