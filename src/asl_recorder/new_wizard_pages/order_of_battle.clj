(ns asl-recorder.new-wizard-pages.order-of-battle
  (:require [asl-recorder.new-wizard-pages.utilities :as u]
            [asl-recorder.new-wizard-pages.side-configuration :as side]
            [seesaw [core :as sc] [layout :as layout] [table :as table]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)))

(defn- build-oob-id [group-number]
  (u/build-id "oob-id" group-number))

(defn- build-oob-pound-id [group-number]
  (u/build-pound-id "oob-id" group-number))

(defn- build-side-group-id [group-number]
  (u/build-id "side-group" group-number))

(defn- build-side-group-pound-id [group-number]
  (u/build-pound-id "side-group" group-number))

(defn- build-nationality-id [group-number]
  (u/build-id "nationality-id" group-number))

(defn- build-nationality-pound-id [group-number]
  (u/build-pound-id "nationality-id" group-number))

(defn- build-counter-type-id [group-number]
  (u/build-id "counter-type-id" group-number))

(defn- build-counter-type-pound-id [group-number]
  (u/build-pound-id "counter-type-id" group-number))

(defn- build-counter-id [group-number]
  (u/build-id "counter-id" group-number))

(defn- build-counter-pound-id [group-number]
  (u/build-pound-id "counter-id" group-number))

(defn- build-number-counters-id [group-number]
  (u/build-id "number-counters-id" group-number))

(defn- build-number-counters-pound-id [group-number]
  (u/build-pound-id "number-counters-id" group-number))

(defn- build-add-to-oob-id [group-number]
  (u/build-id "add-to-oob" group-number))

(defn- build-remove-last-from-oob-id [group-number]
  (u/build-id "remove-last-from-oob" group-number))

(defn- build-remove-last-from-oob-pound-id [group-number]
  (u/build-pound-id "remove-last-from-oob" group-number))

(defn- is-side-name-nationality? [side-name sc]
  (:is-nationality? (some #(when (= side-name (:side-name %)) %) sc)))

(defn- update-nationality-selection [group-number sc t]
  (let [r (sc/to-root t)
        side-name (sc/selection (sc/select r [(build-side-group-pound-id group-number)]))
        is-nationality? (is-side-name-nationality? side-name sc)
        nationality-cb (sc/select r [(build-nationality-pound-id group-number)])
        nationality (if is-nationality? side-name (first (u/extract-nationalities)))]
    (sc/selection! nationality-cb nationality)))

(defn- update-counter-model [group-number t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id group-number)]))
        counter-type (sc/selection (sc/select r [(build-counter-type-pound-id group-number)]))
        counter (sc/select r [(build-counter-pound-id group-number)])]
    (sc/config! counter :model (u/extract-counters nationality counter-type))))

(defn- add-to-oob-action [group-number t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id group-number)]))
        counter (sc/selection (sc/select r [(build-counter-pound-id group-number)]))
        number-counters-spinner (sc/select r [(build-number-counters-pound-id group-number)])
        number-counters (sc/selection number-counters-spinner)
        entry {:nationality     nationality
               :counter         counter
               :number-counters number-counters}
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id group-number)])]
    (table/insert-at! t (table/row-count t) entry)
    (sc/selection! number-counters-spinner 1)
    (sc/config! remove-last-from-oob :enabled? true)))

(defn- remove-last-from-oob-action [group-number t]
  (let [r (sc/to-root t)
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id group-number)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- oob-layout [group-number sc]
  (let [t (sc/table :id (build-oob-id group-number) :model [:columns [:nationality :counter :number-counters]])
        sides (map :side-name sc)
        side (sc/combobox :id (build-side-group-id group-number)
                          :model sides
                          :listen [:action (fn [_] (update-nationality-selection group-number sc t))])
        nationalities (u/extract-nationalities)
        nationality (sc/combobox :id (build-nationality-id group-number)
                                 :model nationalities
                                 :selected-item (let [is-nationality? (:is-nationality? (first sc))]
                                                  (if is-nationality? (:side-name (first sc)) (first nationalities)))
                                 :listen [:action (fn [_] (update-counter-model group-number t))])
        counter-type (sc/combobox :id (build-counter-type-id group-number)
                                  :model ["Infantry" "Support Weapon" "Gun" "Vehicle" "Fortification" "Concealment"]
                                  :listen [:action (fn [_] (update-counter-model group-number t))])
        counter (sc/combobox :id (build-counter-id group-number)
                             :model (u/extract-counters (sc/selection nationality) (sc/selection counter-type)))
        number-counters (sc/spinner :id (build-number-counters-id group-number)
                                    :model (sc/spinner-model 1 :from 1 :to 26 :by 1))
        add-to-oob-button (sc/button :id (build-add-to-oob-id group-number)
                                     :text "Add to OoB"
                                     :listen [:action (fn [_] (add-to-oob-action group-number t))])
        remove-last-from-oob-button (sc/button :id (build-remove-last-from-oob-id group-number)
                                               :text "Remove last from OoB"
                                               :listen [:action (fn [_] (remove-last-from-oob-action group-number t))]
                                               :enabled? false)
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Side: " side])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Nationality: " nationality [:fill-h 10]
                                                      "Counter Type: " counter-type])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Counter: " counter [:fill-h 10]
                                                      "Number of Counters: " number-counters])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-oob-button :fill-h remove-last-from-oob-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    layout))

(defn- initial-setup-oob-page [displayed-group-number number-groups]
  (let [title (str (u/capital-nth-string displayed-group-number) " Setup Order of Battle (OOB) Panel")
        tip (str "Initial Setup OOB for group " displayed-group-number " of " number-groups)]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn initial-setup-oob-panel [group-number sc]
  (let [layout (oob-layout group-number sc)
        total-initial-setup-groups (apply + (map :number-initial-setup-groups sc))
        p (sc/abstract-panel
            (initial-setup-oob-page (inc group-number) total-initial-setup-groups)
            (layout/box-layout :vertical)
            layout)]
    p))

(defn extract-initial-setup-oob [p group-number]
  {:side (sc/selection (sc/select p [(build-side-group-pound-id group-number)]))
   :oob (let [t (sc/select p [(build-oob-pound-id group-number)])]
          (table/value-at t (range (table/row-count t))))})

(defn- reinforcement-oob-page [group-number number-groups]
  (let [title (str (u/capital-nth-string group-number) " Reinforcement Order of Battle (OOB) Panel")
        tip (str "Reinforcement OOB for group " group-number " of " number-groups)]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn reinforcement-oob-panel [group-number sc]
  (let [layout (oob-layout group-number sc)
        total-reinforcement-groups (apply + (map :number-reinforcement-groups sc))
        p (sc/abstract-panel
            (reinforcement-oob-page (inc group-number) total-reinforcement-groups)
            (layout/box-layout :vertical)
            layout)]
    p))

