(ns asl-recorder.new-wizard
  (:require [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd] [layout :as layout] selector [table :as t]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]))

(def ^{:private true} basic-parameters-page
  (proxy [WizardPage seesaw.selector.Tag] ["Game Parameters" "Basic parameters about the scenario to be recorded."]
    (tag_name [] (.getSimpleName WizardPage))))

(def ^{:private true} basic-parameters-panel
  (let [p (sc/abstract-panel
            basic-parameters-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Name: " (sc/text :id :name :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["First Move: " (sc/combobox :id :first-move :model ["German" "Russian" "American"])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Other Side: " (sc/combobox :id :second-move :model ["German" "Russian" "American"])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Number Turns: " (sc/text :id :number-turns :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items [(sc/checkbox :id :extra-move? :text "First Side has extra move?")])
                      :fill-v]})
        preferred-size-fn #(-> p (sc/select %) (sc/config :preferred-size))
        maximum-size-fn #(-> p (sc/select %) (sc/config! :maximum-size (preferred-size-fn %)))]
    (dorun (map #(maximum-size-fn [%]) [:#name :#first-move :#second-move :#number-turns]))
    p))

(def ^{:private true} map-configuration-page
  (proxy [WizardPage seesaw.selector.Tag] ["Map Configuration" "Layout of the game area."]
    (tag_name [] (.getSimpleName WizardPage))))

(defn- create-orientation-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [(sc/radio :id :vertical-orientation :class :orientation-class :text "Vertical" :group the-button-group)
     (sc/radio :id :horizontal-orientation :class :orientation-class :text "Horizontal" :group the-button-group)]))

(def ^{:private true} map-configuration-panel
  (let [t (sc/table :id :map-table :model [:columns [:row :column :present? :board-id :upper-left? :upper-right? :lower-left? :lower-right?]
                                           :rows [[0 0 true "y" false false false false]]])
        update-table-fn (fn [e]
                          (let [r (sc/to-root e)
                                number-rows (-> r (sc/select [:#number-rows]) sc/selection)
                                number-columns (-> r (sc/select [:#number-columns]) sc/selection)]
                            (t/clear! t)
                            (dorun (for [r (range number-rows)
                                         c (range number-columns)]
                                     (t/insert-at! t (+ c (* r number-columns)) [r c true "y" false false false false])))))
        p (sc/abstract-panel
            map-configuration-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Number of rows: " (sc/spinner :id :number-rows :model (sc/spinner-model 1 :from 1 :to 5 :by 1) :listen [:change update-table-fn])
                                                   "Number of columns: " (sc/spinner :id :number-columns :model (sc/spinner-model 1 :from 1 :to 5 :by 1) :listen [:change update-table-fn])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-orientation-radio-buttons))
                      [:fill-v 5]
                      (sc/scrollable t)
                      :fill-v]})]
    p))

(def ^{:private true} defender-initial-setup-page
  (proxy [WizardPage seesaw.selector.Tag] ["DEFENDER Initial Setup" "Initial positions for all on-board DEFENDER units."]
    (tag_name [] (.getSimpleName WizardPage))))

(def ^{:private true} defender-initial-setup-panel
  (let [p (sc/abstract-panel
            defender-initial-setup-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["DEFENDER"])
                      :fill-v]})]
    p))

(def ^{:private true} attacker-initial-setup-page
  (proxy [WizardPage seesaw.selector.Tag] ["ATTACKER Initial Setup" "Initial positions for all on-board ATTACKER units."]
    (tag_name [] (.getSimpleName WizardPage))
    (rendering [path settings]
      (proxy-super rendering path settings)
      (doto this
        (.setFinishEnabled true)
        (.setNextEnabled false)))))

(def ^{:private true} attacker-initial-setup-panel
  (let [p (sc/abstract-panel
            attacker-initial-setup-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["ATTACKER"])
                      :fill-v]})]
    p))

(def ^{:private true} new-wizard-panels [basic-parameters-panel map-configuration-panel defender-initial-setup-panel attacker-initial-setup-panel])

(def new-wizard-page-factory
  (reify PageFactory
    (isTransient [_ _ _] false)
    (createPage [_ wizard-pages _]
      (get new-wizard-panels (.size wizard-pages)))))
