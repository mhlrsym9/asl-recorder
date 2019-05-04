(ns asl-recorder.core-test
  (:require [clojure.test :refer :all]
            [asl-recorder.core :refer :all]
            [asl-recorder.game-attributes :as ga]
            [clojure.zip :as zip]
            [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd]]))

(deftest test-append-event
  (testing "Try to add an event."
    (let [loc (get-current-game-zip-loc)
          new-loc (append-event loc {:sub-phase "Reinforcements" :action-option "Place Reinforcements" :description "Add description" :firepower 16 :result "Add result"})
          fe (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/down zip/node :content first :content)
          fa (-> fe first :content first)
          ff (-> fe second :content first)
          fr (-> fe (nth 2) :content first)]
      (is (= fa "Add description"))
      (is (= ff 16))
      (is (= fr "Add result")))))

(deftest test-append-phase
  (testing "Try to add a phase."
    (let [loc (get-current-game-zip-loc)
          new-loc (append-phase loc "Prep Fire")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/down zip/node :content)]
      (is (= "Prep Fire" (-> fp second :attrs :name)))
      (is (= "Rally" (-> fp first :attrs :name))))))

(deftest test-append-attacker
  (testing "Try to add an attacker."
    (let [loc (get-current-game-zip-loc)
          new-loc (append-attacker loc "Russian")
          fp (-> new-loc zip/root zip/xml-zip zip/down zip/node :content)]
      (is (= "Russian" (-> fp second :attrs :attacker)))
      (is (= "German" (-> fp first :attrs :attacker))))))

(deftest test-append-turn
  (testing "Try to add an turn."
    (let [loc (get-current-game-zip-loc)
          new-loc (append-turn loc 2)
          fp (-> new-loc zip/root zip/xml-zip zip/node :content)]
      (is (= 2 (-> fp second :attrs :number)))
      (is (= 1 (-> fp first :attrs :number))))))

(deftest test-get-current-game-turn
  (testing "Try to get the current game turn."
    (let [loc (get-current-game-zip-loc)]
      (is (= 1 (get-current-game-turn loc))))))

(deftest test-get-current-game-attacker
  (testing "Try to get the current game attacker."
    (let [loc (get-current-game-zip-loc)]
      (is (= "German" (get-current-game-attacker loc))))))

(deftest test-get-current-game-phase
  (testing "Try to get the current game phase."
    (let [loc (get-current-game-zip-loc)]
      (is (= "Rally" (get-current-game-phase loc))))))

(deftest test-advance-game-rally-phase
  (testing "Try out advance-game-rally-phase"
    (let [loc (get-current-game-zip-loc)
          r (advance-game-sub-phase loc "Reinforcements" rally-phase-map)]
      (is (= "ATTACKER Recovery" (get-in r [:next-sub-phase-info :next-sub-phase]))))))

(deftest test-advance-game-through-rally-phase
  (testing "Try out advance-game-rally-phase until all sub-phases added."
    (let [advance-results (take 12 (iterate (fn [{:keys [new-loc] {:keys [next-sub-phase]} :next-sub-phase-info}]
                                              (advance-game-sub-phase new-loc next-sub-phase rally-phase-map))
                                            {:new-loc (get-current-game-zip-loc) :next-sub-phase-info {:next-sub-phase "Reinforcements"}}))
          extract-fn (fn [r] (get-in r [:next-sub-phase-info :next-sub-phase]))]
      (is (= "Reinforcements" (extract-fn (nth advance-results 0))))
      (is (= "ATTACKER Recovery" (extract-fn (nth advance-results 1))))
      (is (= "DEFENDER Recovery" (extract-fn (nth advance-results 2))))
      (is (= "ATTACKER Repair" (extract-fn (nth advance-results 3))))
      (is (= "DEFENDER Repair" (extract-fn (nth advance-results 4))))
      (is (= "ATTACKER Transfer" (extract-fn (nth advance-results 5))))
      (is (= "DEFENDER Transfer" (extract-fn (nth advance-results 6))))
      (is (= "ATTACKER Self-Rally" (extract-fn (nth advance-results 7))))
      (is (= "DEFENDER Self-Rally" (extract-fn (nth advance-results 8))))
      (is (= "ATTACKER Unit Rally" (extract-fn (nth advance-results 9))))
      (is (= "DEFENDER Unit Rally" (extract-fn (nth advance-results 10))))
      (is (= nil (extract-fn (nth advance-results 11))))
      (is (= "Prep Fire" (get-current-game-phase (:new-loc (last advance-results))))))))

(deftest test-advance-game-rally-phase-with-american-attacker
  (testing "Try manipulating the rally phase map and then seeing that that carries through advance."
    (let [the-xml (create-game-start-xml "War of the Rats 2" "American" "German" 6 true)
          loc (initial-game-zip-loc the-xml)
          updated-rally-phase-map (get-sub-phase-map loc rally-phase-map)
          r (advance-game-sub-phase loc "Reinforcements" updated-rally-phase-map)]
      (is (= "American Recovery" (get-in r [:next-sub-phase-info :next-sub-phase]))))))

(deftest test-advance-game-phase
  (testing "Try out advance-game-phase"
    (let [loc (get-current-game-zip-loc)
          {new-loc :new-loc} (advance-game-phase loc)
          {new-loc-2 :new-loc} (advance-game-phase new-loc)]
      (is (= "Movement" (get-current-game-phase new-loc-2)))
      (is (= 3 (count (-> new-loc-2 zip/up zip/node :content)))))))

(deftest test-extract-selected-die
  (testing "Make sure I can select the chosen die"
    (let [{w :radio-buttons bg-white :button-group} (create-die-radio-buttons white)
          p1 (sm/mig-panel :id :white-die-panel :constraints ["fill, insets 0"] :items w :user-data {:color white :button-group bg-white})
          {r :radio-buttons bg-colored :button-group} (create-die-radio-buttons colored)
          p2 (sm/mig-panel :id :colored-die-panel :constraints ["fill, insets 0"] :items r :user-data {:color colored :button-group bg-colored})]
      (sc/selection! (last (sc/select p1 [:JRadioButton])) true)
      (sc/selection! (second (sc/select p2 [:JRadioButton])) true)
      (let [die-rolls (gather-die-rolls (list p1 p2))]
        (is (= 8 (apply + (map :die-roll die-rolls))))))))

(deftest test-get-side1
  (testing "Make sure get-side1 returns German"
    (let [loc (get-current-game-zip-loc)
          side1 (ga/get-side1-from-loc loc)]
      (is (= "German" side1)))))

(deftest test-get-side2
  (testing "Make sure get-side2 returns Russian"
    (let [loc (get-current-game-zip-loc)
          side2 (ga/get-side2-from-loc loc)]
      (is (= "Russian" side2)))))

(deftest test-number-turns
  (testing "Make sure get-number-turns returns 6"
    (let [loc (get-current-game-zip-loc)
          number-turns (ga/get-number-turns-from-loc loc)]
      (is (= 6 number-turns)))
    (let [test-game-loc (initial-game-zip-loc (create-game-start-xml "War" "German" "Russian" 7 true))]
      (is (= 7 (ga/get-number-turns-from-loc test-game-loc))))))

(deftest test-get-sub-phase-map
  (testing "Make sure get-sub-phase-map works"
    (let [test-game-loc (initial-game-zip-loc (create-game-start-xml "War" "German" "American" 7 true))
          rout-sub-phase-map (get-sub-phase-map test-game-loc rout-phase-map)]
      (is (some #{"American Rout"} (keys rout-sub-phase-map)))
      (is (some #{"German Rout"} (keys rout-sub-phase-map)))
      (is (= "American Rout" (:next-sub-phase (get rout-sub-phase-map "German Rout"))))
      (is (nil? (:next-sub-phase (get rout-sub-phase-map "American Rout")))))))

(deftest test-get-previous-description
  (testing "Make sure get-previous-description works"
    (let [loc (get-current-game-zip-loc)
          new-loc (append-event loc {:sub-phase "Reinforcements" :action-option "Place Reinforcements" :description "Add description" :result "Add result"})
          new-new-loc (append-event new-loc {:sub-phase "Reinforcements" :action-option "Place Reinforcements" :description "New description" :result "Add another result"})]
      (is (= (get-previous-description-from-loc new-new-loc) "New description")))))