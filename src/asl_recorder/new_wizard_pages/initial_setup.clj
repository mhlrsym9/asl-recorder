(ns asl-recorder.new-wizard-pages.initial-setup
  (:require [asl-recorder.new-wizard-pages.utilities :as u]
            [clojure.string :as str]
            [seesaw [core :as sc] [layout :as layout] [table :as table]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)))

(defn- build-unique-id [side]
  (u/build-id "unique-id" side))

(defn- build-unique-pound-id [side]
  (u/build-pound-id "unique-id" side))

(defn- build-position-id [side]
  (u/build-id "position-id" side))

(defn- build-position-pound-id [side]
  (u/build-pound-id "position-id" side))

(defn- build-covered-arc-id [side]
  (u/build-id "covered-arc-id" side))

(defn- build-covered-arc-pound-id [side]
  (u/build-pound-id "covered-arc-id" side))

(defn- build-setup-id [side]
  (u/build-id "setup-id" side))

(defn- build-setup-pound-id [side]
  (u/build-pound-id "setup-id" side))

(defn- build-add-to-setup-id [side]
  (u/build-id "add-to-setup" side))

(defn- build-add-to-setup-pound-id [side]
  (u/build-pound-id "add-to-setup" side))

(defn- build-remove-last-from-setup-id [side]
  (u/build-id "remove-last-from-setup" side))

(defn- build-remove-last-from-setup-pound-id [side]
  (u/build-pound-id "remove-last-from-setup" side))

(defn- add-to-setup-enabled? [unique-pound-id position-pound-id e]
  (let [r (sc/to-root e)
        s1 (-> r (sc/select [unique-pound-id]) sc/text)
        s2 (-> r (sc/select [position-pound-id]) sc/text)]
    (and (-> s1 str/blank? not)
         (-> s2 str/blank? not))))

(defn- configure-add-to-setup-enabled-state [side t]
  (let [r (sc/to-root t)
        add-to-oob (sc/select r [(build-add-to-setup-pound-id side)])]
    (sc/config! add-to-oob :enabled? (add-to-setup-enabled? (build-unique-pound-id side) (build-position-pound-id side) t))))

(defn- add-to-setup-action [side p _]
  (let [unique-id (sc/select p [(build-unique-pound-id side)])
        position (sc/select p [(build-position-pound-id side)])
        covered-arc (sc/select p [(build-covered-arc-pound-id side)])
        remove-last-from-oob (sc/select p [(build-remove-last-from-setup-pound-id side)])
        t (sc/select p [(build-setup-pound-id side)])]
    (table/insert-at! t (table/row-count t) {:unique-id (sc/text unique-id)
                                             :position (sc/text position)
                                             :covered-arc (sc/text covered-arc)})
    (sc/config! remove-last-from-oob :enabled? true)
    (sc/text! unique-id "")
    (sc/text! position "")
    (sc/text! covered-arc "")))

(defn- remove-last-from-setup-action [side p _]
  (let [remove-last-from-oob (sc/select p [(build-remove-last-from-setup-pound-id side)])
        t (sc/select p [(build-setup-pound-id side)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-setup-layout [setup-panel-index]
  (let [t (sc/table :id (build-setup-id setup-panel-index) :model [:columns [:unique-id :position :covered-arc]])
        unique-id-text (sc/text :id (build-unique-id setup-panel-index)
                                :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        position-text (sc/text :id (build-position-id setup-panel-index)
                               :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        covered-arc-text (sc/text :id (build-covered-arc-id setup-panel-index)
                                  :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        add-to-setup-button (sc/button :id (build-add-to-setup-id setup-panel-index) :text "Add to OoB")
        remove-last-from-setup-button (sc/button :id (build-remove-last-from-setup-id setup-panel-index)
                                                 :text "Remove last from OoB" :enabled? false)
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Unique ID: " unique-id-text [:fill-h 10]
                                                      "Position: " position-text [:fill-h 10]
                                                      "Covered Arc (CA): " covered-arc-text])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-setup-button :fill-h remove-last-from-setup-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    {:add-to-setup-button add-to-setup-button :remove-last-from-setup-button remove-last-from-setup-button :layout layout}))

(defn- initial-setup-page [setup-panel-index number-setup-panels]
  (let [title (str "Setup Panel " (inc setup-panel-index))
        tip (str "Initial positions for all units in group " (u/nth-string (inc setup-panel-index)) " of " number-setup-panels ".")]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn initial-setup-panel [setup-panel-index sc]
  (let [{:keys [add-to-setup-button remove-last-from-setup-button layout]} (initial-setup-layout setup-panel-index)
        total-initial-setup-groups (apply + (map :number-initial-setup-groups sc))
        p (sc/abstract-panel
            (initial-setup-page setup-panel-index total-initial-setup-groups)
            (layout/box-layout :vertical)
            layout)]
    (sc/listen add-to-setup-button :action (partial add-to-setup-action setup-panel-index p))
    (sc/listen remove-last-from-setup-button :action (partial remove-last-from-setup-action setup-panel-index p))
    p))

(defn extract-initial-setup [p group-number]
  (let [t (sc/select p [(build-setup-pound-id group-number)])]
    (table/value-at t (range (table/row-count t)))))



