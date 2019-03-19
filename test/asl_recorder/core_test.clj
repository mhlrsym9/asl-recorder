(ns asl-recorder.core-test
  (:require [clojure.test :refer :all]
            [asl-recorder.core :refer :all]
            [clojure.zip :as zip]))

(deftest test-append-event
  (testing "Try to add an event."
    (let [new-loc (append-event @game-zip-loc "Add action" "Add result")
          fe (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/down zip/node :content first :content first)
          fa (-> fe first :content first)
          fr (-> fe second :content first)]
      (is (= fa "Add action"))
      (is (= fr "Add result")))))

(deftest test-append-phase
  (testing "Try to add a phase."
    (let [new-loc (append-phase @game-zip-loc "Prep Fire")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/node :content)]
      (is (= "Prep Fire" (-> fp first :attrs :name)))
      (is (= "Rally" (-> fp second :attrs :name))))))

(deftest test-append-attacker
  (testing "Try to add an attacker."
    (let [new-loc (append-attacker @game-zip-loc "Russian")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/node :content)]
      (is (= "Russian" (-> fp first :attrs :attacker)))
      (is (= "German" (-> fp second :attrs :attacker))))))

(deftest test-append-turn
  (testing "Try to add an turn."
    (let [new-loc (append-turn @game-zip-loc 2)
          fp (-> new-loc zip/root zip/xml-zip zip/node :content)]
      (is (= 2 (-> fp first :attrs :number)))
      (is (= 1 (-> fp second :attrs :number))))))