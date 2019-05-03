(ns asl-recorder.info
  (:require [asl-recorder.game-attributes :as ga]
            [asl-recorder.swing-worker]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.zip :as zip]
            [seesaw [core :as sc]])
  (:import [java.awt Cursor]
           [java.beans PropertyChangeListener]
           [java.util.concurrent ExecutionException]
           [javax.swing SwingWorker$StateValue]))

(defn- extract-the-dice-locations [the-xml-zip attacker phase action color]
  (zip-xml/xml-> the-xml-zip
                 :turn
                 :side
                 (zip-xml/attr= :attacker attacker)
                 :phase
                 (zip-xml/attr= :name phase)
                 :event
                 (zip-xml/attr= :action action)
                 :die-rolls :die-roll
                 (zip-xml/attr= :color color)))

(def attacking-fire-actions ["Prep Fire" "Advancing Fire"])

(def defending-fire-actions ["Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP" "Final Fire"])

(def attacking-phases ["Prep Fire" "Advancing Fire"])

(def defending-phases ["Movement" "Defensive Fire"])

(def morale-actions ["Morale Check" "Pin Task Check" "Leader Loss Morale Check" "Leader Loss Task Check"])

(defn- build-actions [side phases actions]
  (for [phase phases
        action actions]
    (vector side phase action)))

(defn- extract-the-dice-rolls [the-xml-zip attacker phase action]
  (let [die-fn (comp first zip/down)]
    (apply map #(+ (die-fn %1) (die-fn %2)) (map (partial extract-the-dice-locations the-xml-zip attacker phase action) ["white" "colored"]))))

(defn- get-fire-dice-rolls [the-xml-zip attacker defender]
  (let [extract-fn (fn [[side phase action]] (extract-the-dice-rolls the-xml-zip side phase action))
        attacking-fire-rolls (mapcat extract-fn (build-actions attacker attacking-phases attacking-fire-actions))
        defending-fire-rolls (mapcat extract-fn (build-actions defender defending-phases defending-fire-actions))]
    (concat attacking-fire-rolls defending-fire-rolls)))

(defn- get-morale-dice-rolls [the-xml-zip attacker defender]
  (let [extract-fn (fn [[side phase action]] (extract-the-dice-rolls the-xml-zip side phase action))
        morale-rolls-during-defensive-phases (mapcat extract-fn (build-actions attacker defending-phases morale-actions))
        morale-rolls-during-attacking-phases (mapcat extract-fn (build-actions defender attacking-phases morale-actions))]
    (concat morale-rolls-during-defensive-phases morale-rolls-during-attacking-phases)))

(defn- calculate-average-dice-roll [the-dice-rolls]
  (format "%.2f" (double (/ (apply + the-dice-rolls) (count the-dice-rolls)))))

(defn- generate-average-fire-dice-roll-report [side the-dice-rolls]
  (str "Average roll for " side " fire attacks = " (calculate-average-dice-roll the-dice-rolls)))

(defn- generate-average-morale-roll-report [side the-dice-rolls]
  (str "Average roll for " side " morale checks = " (calculate-average-dice-roll the-dice-rolls)))

(defn dice [e loc]
  (let [r (sc/to-root e)
        sw (proxy [asl_recorder.swing_worker] []
             (doInBackground []
               (let [xml-zip (-> loc zip/root zip/xml-zip)
                     side1 (ga/get-side1-from-loc loc)
                     side2 (ga/get-side2-from-loc loc)
                     side1-fire-dice-rolls (get-fire-dice-rolls xml-zip side1 side2)
                     side1-morale-dice-rolls (get-morale-dice-rolls xml-zip side1 side2)
                     side2-fire-dice-rolls (get-fire-dice-rolls xml-zip side2 side1)
                     side2-morale-dice-rolls (get-morale-dice-rolls xml-zip side2 side1)]
                 (proxy-super publishFromClojure (into-array String [(generate-average-fire-dice-roll-report side1 side1-fire-dice-rolls)
                                                                     (generate-average-morale-roll-report side1 side1-morale-dice-rolls)
                                                                     (generate-average-fire-dice-roll-report side2 side2-fire-dice-rolls)
                                                                     (generate-average-morale-roll-report side2 side2-morale-dice-rolls)]))))
             (process [chunks]
               (doseq [chunk chunks] (println chunk)))
             (done []
               (try
                 (do
                   (proxy-super get))
                 (catch ExecutionException e
                   (throw e))
                 (finally
                   (.setCursor r Cursor/DEFAULT_CURSOR)))))
        pcl (proxy [PropertyChangeListener] []
              (propertyChange [e]
                (when (= (.getPropertyName e) "state")
                  (let [v (.getNewValue e)]
                    (cond
                      (= v SwingWorker$StateValue/STARTED) (.setCursor r Cursor/WAIT_CURSOR)
                      (= v SwingWorker$StateValue/DONE) (.setCursor r Cursor/DEFAULT_CURSOR))))))]
    (doto sw
      (.addPropertyChangeListener pcl)
      (.execute))))

(defn version [_])