(ns asl-recorder.new-wizard-pages.order-of-battle
  (:require [asl-recorder.new-wizard-pages.utilities :as u]
            [asl-recorder.new-wizard-pages.side-configuration :as side]
            [seesaw [core :as sc] [layout :as layout] [table :as table]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)))

(defn- build-oob-id [{:keys [group-number suffix]}]
  (u/build-id "oob-id" group-number suffix))

(defn- build-oob-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "oob-id" group-number suffix))

(defn- build-side-group-id [{:keys [group-number suffix]}]
  (u/build-id "side-group" group-number suffix))

(defn- build-side-group-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "side-group" group-number suffix))

(defn- build-nationality-id [{:keys [group-number suffix]}]
  (u/build-id "nationality-id" group-number suffix))

(defn- build-nationality-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "nationality-id" group-number suffix))

(defn- build-counter-type-id [{:keys [group-number suffix]}]
  (u/build-id "counter-type-id" group-number suffix))

(defn- build-counter-type-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "counter-type-id" group-number suffix))

(defn- build-counter-id [{:keys [group-number suffix]}]
  (u/build-id "counter-id" group-number suffix))

(defn- build-counter-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "counter-id" group-number suffix))

(defn- build-number-counters-id [{:keys [group-number suffix]}]
  (u/build-id "number-counters-id" group-number suffix))

(defn- build-number-counters-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "number-counters-id" group-number suffix))

(defn- build-add-to-oob-id [{:keys [group-number suffix]}]
  (u/build-id "add-to-oob" group-number suffix))

(defn- build-remove-last-from-oob-id [{:keys [group-number suffix]}]
  (u/build-id "remove-last-from-oob" group-number suffix))

(defn- build-remove-last-from-oob-pound-id [{:keys [group-number suffix]}]
  (u/build-pound-id "remove-last-from-oob" group-number suffix))

(defn- is-side-name-nationality? [side-name sc]
  (:is-nationality? (some #(when (= side-name (:side-name %)) %) sc)))

(defn- update-nationality-selection [id-modifiers sc t]
  (let [r (sc/to-root t)
        side-name (sc/selection (sc/select r [(build-side-group-pound-id id-modifiers)]))
        is-nationality? (is-side-name-nationality? side-name sc)
        nationality-cb (sc/select r [(build-nationality-pound-id id-modifiers)])
        nationality (if is-nationality? side-name (first (u/extract-nationalities)))]
    (sc/selection! nationality-cb nationality)))

(defn- update-counter-model [id-modifiers t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id id-modifiers)]))
        counter-type (sc/selection (sc/select r [(build-counter-type-pound-id id-modifiers)]))
        counter (sc/select r [(build-counter-pound-id id-modifiers)])]
    (sc/config! counter :model (u/extract-counters nationality counter-type))))

(defn- add-to-oob-action [id-modifiers t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id id-modifiers)]))
        counter (sc/selection (sc/select r [(build-counter-pound-id id-modifiers)]))
        number-counters-spinner (sc/select r [(build-number-counters-pound-id id-modifiers)])
        number-counters (sc/selection number-counters-spinner)
        entry {:nationality     nationality
               :counter         counter
               :number-counters number-counters}
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id id-modifiers)])]
    (table/insert-at! t (table/row-count t) entry)
    (sc/selection! number-counters-spinner 1)
    (sc/config! remove-last-from-oob :enabled? true)))

(defn- remove-last-from-oob-action [id-modifiers t]
  (let [r (sc/to-root t)
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id id-modifiers)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- oob-layout [id-modifiers sc side-names-remaining]
  (let [t (sc/table :id (build-oob-id id-modifiers) :model [:columns [:nationality :counter :number-counters]])
        side (sc/combobox :id (build-side-group-id id-modifiers)
                          :model side-names-remaining
                          :listen [:action (fn [_] (update-nationality-selection id-modifiers sc t))])
        nationalities (u/extract-nationalities)
        nationality (sc/combobox :id (build-nationality-id id-modifiers)
                                 :model nationalities
                                 :selected-item (let [first-side-name (first side-names-remaining)
                                                      side-matching-side-name (first (filter #(= (:side-name %) first-side-name) sc))
                                                      is-nationality? (:is-nationality? side-matching-side-name)]
                                                  (if is-nationality? first-side-name (first nationalities)))
                                 :listen [:action (fn [_] (update-counter-model id-modifiers t))])
        counter-type (sc/combobox :id (build-counter-type-id id-modifiers)
                                  :model ["Infantry" "Support Weapon" "Gun" "Vehicle" "Fortification" "Concealment"]
                                  :listen [:action (fn [_] (update-counter-model id-modifiers t))])
        counter (sc/combobox :id (build-counter-id id-modifiers)
                             :model (u/extract-counters (sc/selection nationality) (sc/selection counter-type)))
        number-counters (sc/spinner :id (build-number-counters-id id-modifiers)
                                    :model (sc/spinner-model 1 :from 1 :to 26 :by 1))
        add-to-oob-button (sc/button :id (build-add-to-oob-id id-modifiers)
                                     :text "Add to OoB"
                                     :listen [:action (fn [_] (add-to-oob-action id-modifiers t))])
        remove-last-from-oob-button (sc/button :id (build-remove-last-from-oob-id id-modifiers)
                                               :text "Remove last from OoB"
                                               :listen [:action (fn [_] (remove-last-from-oob-action id-modifiers t))]
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

(defn initial-setup-oob-panel [group-number sc side-names-remaining]
  (let [layout (oob-layout {:group-number group-number :suffix "s"} sc side-names-remaining)
        total-initial-setup-groups (apply + (map :number-initial-setup-groups sc))
        p (sc/abstract-panel
            (initial-setup-oob-page (inc group-number) total-initial-setup-groups)
            (layout/box-layout :vertical)
            layout)]
    p))

(defn extract-initial-setup-oob [p group-number]
  (let [id-modifiers {:group-number group-number :suffix "s"}]
    {:side-name (sc/selection (sc/select p [(build-side-group-pound-id id-modifiers)]))
     :oob       (let [t (sc/select p [(build-oob-pound-id id-modifiers)])]
                  (table/value-at t (range (table/row-count t))))}))

(defn- reinforcement-oob-page [group-number number-groups]
  (let [title (str (u/capital-nth-string group-number) " Reinforcement Order of Battle (OOB) Panel")
        tip (str "Reinforcement OOB for group " group-number " of " number-groups)]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn reinforcement-oob-panel [group-number sc side-names-remaining]
  (let [layout (oob-layout {:group-number group-number :suffix "r"} sc side-names-remaining)
        total-reinforcement-groups (apply + (map :number-reinforcement-groups sc))
        p (sc/abstract-panel
            (reinforcement-oob-page (inc group-number) total-reinforcement-groups)
            (layout/box-layout :vertical)
            layout)]
    p))

(defn extract-reinforcement-oob [p group-number]
  (let [id-modifiers {:group-number group-number :suffix "r"}]
    {:side-name (sc/selection (sc/select p [(build-side-group-pound-id id-modifiers)]))
     :oob       (let [t (sc/select p [(build-oob-pound-id id-modifiers)])]
                  (table/value-at t (range (table/row-count t))))}))

