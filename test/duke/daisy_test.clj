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



(defcomponent TheFullMonty
  "Component that shows off all the features of defcomponent."
  {:init-args [a]
   :injected  [b]
   :creates   [c]}
  (start [this]
         (assoc this :c (+ a b)))
  (stop [this]
        (assoc this :c nil)))
