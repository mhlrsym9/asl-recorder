(ns asl-recorder.core-test
  (:require [clojure.test :refer :all]
            [asl-recorder.core :refer :all]
            [clojure.zip :as zip]))

(deftest test-append-event
  (testing "Try to add an event."
    (let [new-loc (append-event @game-zip-loc "Reinforcements" "Add action" "Add result")
          fe (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/down zip/node :content first :content first)
          fa (-> fe first :content first)
          fr (-> fe second :content first)]
      (is (= fa "Add action"))
      (is (= fr "Add result")))))

(deftest test-append-phase
  (testing "Try to add a phase."
    (let [new-loc (append-phase @game-zip-loc "Prep Fire")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/node :content)]
      (is (= "Prep Fire" (-> fp second :attrs :name)))
      (is (= "Rally" (-> fp first :attrs :name))))))

(deftest test-append-attacker
  (testing "Try to add an attacker."
    (let [new-loc (append-attacker @game-zip-loc "Russian")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/node :content)]
      (is (= "Russian" (-> fp second :attrs :attacker)))
      (is (= "German" (-> fp first :attrs :attacker))))))

(deftest test-append-turn
  (testing "Try to add an turn."
    (let [new-loc (append-turn @game-zip-loc 2)
          fp (-> new-loc zip/root zip/xml-zip zip/node :content)]
      (is (= 2 (-> fp second :attrs :number)))
      (is (= 1 (-> fp first :attrs :number))))))

(deftest test-get-current-game-turn
  (testing "Try to get the current game turn."
    (is (= 1 (get-game-turn @game-zip-loc)))))

(deftest test-get-current-game-attacker
  (testing "Try to get the current game attacker."
    (is (= "German" (get-game-attacker @game-zip-loc)))))

(deftest test-get-current-game-phase
  (testing "Try to get the current game phase."
    (is (= "Rally" (get-game-phase @game-zip-loc)))))

(deftest test-advance-game-rally-phase
  (testing "Try out advance-game-rally-phase"
    (let [r (advance-game-rally-phase @game-zip-loc "Reinforcements")]
      (is (= "ATTACKER Recovery" (:next-rally-phase r))))))

(deftest test-advance-game-through-rally-phase
  (testing "Try out advance-game-rally-phase until all sub-phases added."
    (let [advance-results (take 12 (iterate (fn [{:keys [new-loc next-rally-phase]}] (advance-game-rally-phase new-loc next-rally-phase))
                                            {:new-loc @game-zip-loc :next-rally-phase "Reinforcements"}))]
      (is (= "Reinforcements" (:next-rally-phase (nth advance-results 0))))
      (is (= "ATTACKER Recovery" (:next-rally-phase (nth advance-results 1))))
      (is (= "DEFENDER Recovery" (:next-rally-phase (nth advance-results 2))))
      (is (= "ATTACKER Repair" (:next-rally-phase (nth advance-results 3))))
      (is (= "DEFENDER Repair" (:next-rally-phase (nth advance-results 4))))
      (is (= "ATTACKER Transfer" (:next-rally-phase (nth advance-results 5))))
      (is (= "DEFENDER Transfer" (:next-rally-phase (nth advance-results 6))))
      (is (= "ATTACKER Self-Rally" (:next-rally-phase (nth advance-results 7))))
      (is (= "DEFENDER Self-Rally" (:next-rally-phase (nth advance-results 8))))
      (is (= "ATTACKER Unit Rally" (:next-rally-phase (nth advance-results 9))))
      (is (= "DEFENDER Unit Rally" (:next-rally-phase (nth advance-results 10))))
      (is (= "Reinforcements" (:next-rally-phase (nth advance-results 11))))
      (is (= "Prep Fire" (get-game-phase (:new-loc (last advance-results))))))))

(deftest test-advance-game-phase
  (testing "Try out advance-game-phase"
    (let [{new-loc :new-loc} (advance-game-phase @game-zip-loc)
          {new-loc-2 :new-loc} (advance-game-phase new-loc)]
      (is (= "Movement" (get-game-phase new-loc-2)))
      (is (= 3 (count (-> new-loc-2 zip/up zip/node :content)))))))
