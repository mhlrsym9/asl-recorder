(ns asl-recorder.new-wizard-pages.optional-rules
  (:require [seesaw [core :as sc] [layout :as layout]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (seesaw.selector Tag)))

(def ^{:private true} optional-rules-page
  (proxy [WizardPage Tag] ["Optional Rules" "Choose the ASL optional rules in effect for this game."]
    (tag_name [] (.getSimpleName WizardPage))))

(defn optional-rules-panel []
  (let [p (sc/abstract-panel
            optional-rules-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items [(sc/checkbox :id :use-iift? :text "Use IIFT?")])
                      [:fill-v 5]
                      (sc/horizontal-panel :items [(sc/checkbox :id :use-battlefield-integrity? :text "Use Battlefield Integrity?")])
                      :fill-v]})]
    p))

(defn extract-optional-rules [pm]
  (let [p (:optional-rules @pm)
        iift? (sc/selection (sc/select p [:#use-iift?]))
        battlefield-integrity? (sc/selection (sc/select p [:#use-battlefield-integrity?]))]
    {:iift? iift? :battlefield-integrity? battlefield-integrity?}))

