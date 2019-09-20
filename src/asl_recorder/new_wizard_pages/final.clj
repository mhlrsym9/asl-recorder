(ns asl-recorder.new-wizard-pages.final
  (:require [seesaw [core :as sc] [layout :as layout]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)))

(defn- final-layout []
  (let [layout {:border 5
                :items ["Congratulations! You're done!"]}]
    layout))

(defn- final-page []
  (let [title "Final Panel "
        tip "This is the final panel!"]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage))
      (rendering [path settings]
        (proxy-super rendering path settings)
        (doto this
          (.setFinishEnabled true)
          (.setNextEnabled false))))))

(defn final-panel []
  (let [layout (final-layout)
        p (sc/abstract-panel
            (final-page)
            (layout/box-layout :vertical)
            layout)]
    p))

