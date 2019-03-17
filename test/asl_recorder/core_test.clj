(ns asl-recorder.core-test
  (:require [clojure.test :refer :all]
            [asl-recorder.core :refer :all]
            [clojure.zip :as zip]))

(deftest a-test
  (testing "Try to add an event."
    (let [new-loc (append-event @game-zip-loc "Add event")
          fc (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/down zip/node :content first :content first)]
      (is (= fc "Add event")))))
