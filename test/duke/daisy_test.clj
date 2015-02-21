(ns duke.daisy-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [duke.daisy :as daisy :refer (defcomponent)]))


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
