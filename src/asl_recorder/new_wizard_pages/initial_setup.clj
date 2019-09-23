(ns asl-recorder.new-wizard-pages.initial-setup
  (:require [asl-recorder.new-wizard-pages.utilities :as u]
            [clojure.string :as str]
            [seesaw [core :as sc] [layout :as layout] [table :as table]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)))

(defn- build-setup-nationality-id [setup-panel-index]
  (u/build-id "setup-nationality-id" setup-panel-index))

(defn- build-setup-nationality-pound-id [setup-panel-index]
  (u/build-pound-id "setup-nationality-id" setup-panel-index))

(defn- build-setup-counter-id [setup-panel-index]
  (u/build-id "setup-counter-id" setup-panel-index))

(defn- build-setup-counter-pound-id [setup-panel-index]
  (u/build-pound-id "setup-counter-id" setup-panel-index))

(defn- build-unique-id [setup-panel-index]
  (u/build-id "unique-id" setup-panel-index))

(defn- build-unique-pound-id [setup-panel-index]
  (u/build-pound-id "unique-id" setup-panel-index))

(defn- build-position-id [setup-panel-index]
  (u/build-id "position-id" setup-panel-index))

(defn- build-position-pound-id [setup-panel-index]
  (u/build-pound-id "position-id" setup-panel-index))

(defn- build-covered-arc-id [setup-panel-index]
  (u/build-id "covered-arc-id" setup-panel-index))

(defn- build-covered-arc-pound-id [setup-panel-index]
  (u/build-pound-id "covered-arc-id" setup-panel-index))

(defn- build-turret-covered-arc-id [setup-panel-index]
  (u/build-id "turret-covered-arc-id" setup-panel-index))

(defn- build-turret-covered-arc-pound-id [setup-panel-index]
  (u/build-pound-id "turret-covered-arc-id" setup-panel-index))

(defn- build-setup-id [setup-panel-index]
  (u/build-id "setup-id" setup-panel-index))

(defn- build-setup-pound-id [setup-panel-index]
  (u/build-pound-id "setup-id" setup-panel-index))

(defn- build-add-to-setup-id [setup-panel-index]
  (u/build-id "add-to-setup" setup-panel-index))

(defn- build-add-to-setup-pound-id [setup-panel-index]
  (u/build-pound-id "add-to-setup" setup-panel-index))

(defn- build-remove-last-from-setup-id [setup-panel-index]
  (u/build-id "remove-last-from-setup" setup-panel-index))

(defn- build-remove-last-from-setup-pound-id [setup-panel-index]
  (u/build-pound-id "remove-last-from-setup" setup-panel-index))

(defn- extract-all-nationalities-from-oob [oob]
  (apply merge-with + (map (fn [{:keys [nationality number-counters]}]
                             (hash-map nationality number-counters))
                           oob)))

(defn- extract-all-counters-from-oob [oob current-nationality]
  (apply merge-with +
         (filter identity (map (fn [{:keys [nationality counter number-counters]}]
                                 (when (= nationality current-nationality)
                                   (hash-map counter number-counters)))
                               oob))))

(defn- update-nationality-model [setup-panel-index oob t]
  (let [r (sc/to-root t)
        nationality (-> r (sc/select [(build-setup-nationality-pound-id setup-panel-index)]))
        selected-nationality (sc/selection nationality)
        all-nationalities-from-oob (extract-all-nationalities-from-oob oob)
        all-nationalities-setup (frequencies (map :nationality (table/value-at t (range (table/row-count t)))))
        nationalities-still-available (filter identity (map (fn [[k v]] (let [number-setup (get all-nationalities-setup k 0)]
                                                                          (when (< number-setup v) k)))
                                                            all-nationalities-from-oob))]
    (sc/config! nationality :model nationalities-still-available)
    (if (some #{selected-nationality} nationalities-still-available)
      (sc/selection! nationality selected-nationality)
      (sc/selection! nationality 0))))

(defn- update-counter-model [setup-panel-index oob t]
  (let [r (sc/to-root t)
        nationality (sc/select r [(build-setup-nationality-pound-id setup-panel-index)])
        selected-nationality (sc/selection nationality)
        counter (sc/select r [(build-setup-counter-pound-id setup-panel-index)])
        selected-counter (sc/selection counter)
        all-counters-from-oob (extract-all-counters-from-oob oob selected-nationality)
        all-counters-setup (frequencies (map :counter (filter #(= selected-nationality (:nationality %))
                                                              (table/value-at t (range (table/row-count t))))))
        counters-still-available (filter identity (map (fn [[k v]] (let [number-setup (get all-counters-setup k 0)]
                                                                     (when (< number-setup v) k)))
                                                       all-counters-from-oob))]
    (sc/config! counter :model counters-still-available)
    (if (some #{selected-counter} counters-still-available)
      (sc/selection! counter selected-counter)
      (sc/selection! counter 0))))

(defn- add-to-setup-enabled? [unique-pound-id position-pound-id e]
  (let [r (sc/to-root e)
        s1 (-> r (sc/select [unique-pound-id]) sc/text)
        s2 (-> r (sc/select [position-pound-id]) sc/text)]
    (and (-> s1 str/blank? not)
         (-> s2 str/blank? not))))

(defn- configure-add-to-setup-enabled-state [setup-panel-index t]
  (let [r (sc/to-root t)
        add-to-oob (sc/select r [(build-add-to-setup-pound-id setup-panel-index)])]
    (sc/config! add-to-oob :enabled? (add-to-setup-enabled? (build-unique-pound-id setup-panel-index)
                                                            (build-position-pound-id setup-panel-index)
                                                            t))))

(defn- add-to-setup-action [setup-panel-index oob t]
  (let [r (sc/to-root t)
        nationality (sc/select r [(build-setup-nationality-pound-id setup-panel-index)])
        counter (sc/select r [(build-setup-counter-pound-id setup-panel-index)])
        unique-id (sc/select r [(build-unique-pound-id setup-panel-index)])
        position (sc/select r [(build-position-pound-id setup-panel-index)])
        covered-arc (sc/select r [(build-covered-arc-pound-id setup-panel-index)])
        turret-covered-arc (sc/select r [(build-turret-covered-arc-pound-id setup-panel-index)])
        remove-last-from-oob (sc/select r [(build-remove-last-from-setup-pound-id setup-panel-index)])]
    (table/insert-at! t (table/row-count t) {:nationality (sc/selection nationality)
                                             :counter (sc/selection counter)
                                             :unique-id (sc/text unique-id)
                                             :position (sc/text position)
                                             :covered-arc (sc/text covered-arc)
                                             :turret-covered-arc (sc/text turret-covered-arc)})
    (update-nationality-model setup-panel-index oob t)
    (update-counter-model setup-panel-index oob t)
    (sc/config! remove-last-from-oob :enabled? true)
    (sc/text! unique-id "")
    (sc/text! position "")
    (sc/text! covered-arc "")
    (sc/text! turret-covered-arc "")))

(defn- remove-last-from-setup-action [setup-panel-index oob t]
  (let [r (sc/to-root t)
        unique-id (sc/select r [(build-unique-pound-id setup-panel-index)])
        position (sc/select r [(build-position-pound-id setup-panel-index)])
        covered-arc (sc/select r [(build-covered-arc-pound-id setup-panel-index)])
        turret-covered-arc (sc/select r [(build-turret-covered-arc-pound-id setup-panel-index)])
        remove-last-from-oob (sc/select r [(build-remove-last-from-setup-pound-id setup-panel-index)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (update-nationality-model setup-panel-index oob t)
    (update-counter-model setup-panel-index oob t)
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))
    (sc/text! unique-id "")
    (sc/text! position "")
    (sc/text! covered-arc "")
    (sc/text! turret-covered-arc "")))

(defn- initial-setup-layout [setup-panel-index oob]
  (let [t (sc/table :id (build-setup-id setup-panel-index) :model [:columns [:nationality :counter :unique-id :position :covered-arc :turret-covered-arc]])
        nationalities (keys (extract-all-nationalities-from-oob oob))
        nationality (sc/combobox :id (build-setup-nationality-id setup-panel-index)
                                 :model nationalities)
        counter (sc/combobox :id (build-setup-counter-id setup-panel-index)
                             :model (keys (extract-all-counters-from-oob oob (first nationalities))))
        unique-id-text (sc/text :id (build-unique-id setup-panel-index)
                                :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        position-text (sc/text :id (build-position-id setup-panel-index)
                               :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        covered-arc-text (sc/text :id (build-covered-arc-id setup-panel-index)
                                  :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        turret-covered-arc-text (sc/text :id (build-turret-covered-arc-id setup-panel-index)
                                         :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        add-to-setup-button (sc/button :id (build-add-to-setup-id setup-panel-index) :text "Add to OoB"
                                       :listen [:action (fn [_] (add-to-setup-action setup-panel-index oob t))])
        remove-last-from-setup-button (sc/button :id (build-remove-last-from-setup-id setup-panel-index)
                                                 :text "Remove last from OoB" :enabled? false
                                                 :listen [:action (fn [_] (remove-last-from-setup-action setup-panel-index oob t))])
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Nationality: " nationality
                                                      [:fill-h 10]
                                                      "Counter: " counter])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Unique ID: " unique-id-text
                                                      [:fill-h 10]
                                                      "Position: " position-text])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Covered Arc (CA): " covered-arc-text
                                                      [:fill-h 10]
                                                      "Turret Covered Arc (TCA):" turret-covered-arc-text])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-setup-button :fill-h remove-last-from-setup-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    layout))

(defn- initial-setup-page [displayed-setup-panel-number number-setup-panels]
  (let [title (str "Setup Panel " displayed-setup-panel-number)
        tip (str "Initial positions for all units in group "
                 (u/nth-string displayed-setup-panel-number)
                 " of " number-setup-panels ".")]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn initial-setup-panel [setup-panel-index sc mc oob]
  (let [layout (initial-setup-layout setup-panel-index oob)
        total-initial-setup-groups (apply + (map :number-initial-setup-groups sc))
        p (sc/abstract-panel
            (initial-setup-page (inc setup-panel-index) total-initial-setup-groups)
            (layout/box-layout :vertical)
            layout)]
    p))

(defn extract-initial-setup [p group-number]
  (let [t (sc/select p [(build-setup-pound-id group-number)])]
    (table/value-at t (range (table/row-count t)))))



