(ns asl-recorder.new-wizard-pages.basic-configuration
  (:require [seesaw [core :as sc] [layout :as layout]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (seesaw.selector Tag)))

; CJWizard has a bug where it reaches into the JSpinner, extracts the low-level
; formattedTextField for the number model, and stores its value. The problem is
; there is no way to change the name of this component, therefore if you use
; a number-based JSpinner in a subsequent page, those JSpinners will be set to
; the value of this one. CJWizard's rendering system sets the value of any component
; it can find on each page if the component name matches. To avoid that, I need
; to override the updateSettings implementation so I can remove that entry. Seesaw's id
; system will allow me to extract the entry without using WizardSettings.

(def ^{:private true} basic-configuration-page
  (proxy [WizardPage Tag] ["Game Parameters" "Basic parameters about the scenario to be recorded."]
    (tag_name [] (.getSimpleName WizardPage))
    (updateSettings [settings]
      (proxy-super updateSettings settings)
      (.remove settings "Spinner.formattedTextField"))))

(defn- create-rule-set-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [:fill-h
     "Rule Set:"
     (sc/radio :id :asl :class :orientation-class :text "ASL" :group the-button-group :selected? true)
     (sc/radio :id :asl-sk :class :orientation-class :text "ASL Starter Kit" :group the-button-group)
     :fill-h]))

(defn basic-configuration-panel []
  (let [p (sc/abstract-panel
            basic-configuration-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Name: " (sc/text :id :name :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-rule-set-radio-buttons))
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Number Turns: " (sc/spinner :id :number-turns
                                                                                :model (sc/spinner-model 1 :from 1 :to 15 :by 1))])
                      :fill-v]})
        preferred-size-fn #(-> p (sc/select %) (sc/config :preferred-size))
        maximum-size-fn #(-> p (sc/select %) (sc/config! :maximum-size (preferred-size-fn %)))]
    (dorun (map #(maximum-size-fn [%]) [:#name :#number-turns]))
    p))

(defn- is-asl-selected? [pm]
  (let [p (:basic-parameters @pm)]
    (sc/selection (sc/select p [:#asl]))))

(defn- extract-rule-set [pm]
  (if (is-asl-selected? pm) "asl" "asl-sk"))

(defn extract-basic-configuration [pm]
  (let [p (:basic-parameters @pm)
        name (sc/text (sc/select p [:#name]))
        rule-set (extract-rule-set pm)
        nt (sc/selection (sc/select p [:#number-turns]))]
    {:name name :rule-set rule-set :nt nt}))

