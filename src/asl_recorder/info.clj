(ns asl-recorder.info
  (:require [asl-recorder.game-attributes :as ga]
            [asl-recorder.swing-worker]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [seesaw [core :as sc]])
  (:import [java.awt Cursor]
           [java.beans PropertyChangeListener]
           [java.util.concurrent ExecutionException]
           [javax.swing SwingWorker$StateValue]))

(defn- extract-the-dice-locations [the-xml-zip attacker phase action color]
  (zip-xml/xml-> the-xml-zip
                 :turns
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
  (if (seq the-dice-rolls)
    (format "%.2f" (double (/ (apply + the-dice-rolls) (count the-dice-rolls))))
    "-"))

(defn- generate-average-fire-dice-roll-prefix [side]
  (str "Average roll for " side " fire attacks = "))

(defn- generate-average-fire-dice-roll-report [side the-dice-rolls]
  (str (generate-average-fire-dice-roll-prefix side) (calculate-average-dice-roll the-dice-rolls)))

(defn- generate-average-morale-dice-roll-prefix [side]
  (str "Average roll for " side " morale checks = "))

(defn- generate-average-morale-dice-roll-report [side the-dice-rolls]
  (str (generate-average-morale-dice-roll-prefix side) (calculate-average-dice-roll the-dice-rolls)))

(defn- dice [d e loc]
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
                                                                     (generate-average-morale-dice-roll-report side1 side1-morale-dice-rolls)
                                                                     (generate-average-fire-dice-roll-report side2 side2-fire-dice-rolls)
                                                                     (generate-average-morale-dice-roll-report side2 side2-morale-dice-rolls)]))))
             (process [chunks]
               (let [side1 (ga/get-side1-from-loc loc)
                     side2 (ga/get-side2-from-loc loc)
                     dr (sc/to-root d)
                     m {(generate-average-fire-dice-roll-prefix side1) :#side1-fire
                        (generate-average-morale-dice-roll-prefix side1) :#side1-morale
                        (generate-average-fire-dice-roll-prefix side2) :#side2-fire
                        (generate-average-morale-dice-roll-prefix side2) :#side2-morale}]
                 (doseq [chunk chunks]
                   (let [fm (filter (fn [[k _]] (str/starts-with? chunk k)) m)]
                     (when (seq? fm)
                       (sc/text! (sc/select dr [(second (first fm))]) chunk))))
                 (-> d
                     sc/pack!
                     sc/show!)))
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

(defn dice-dialog [e loc]
  (-> (sc/dialog :content (sc/vertical-panel :items [(sc/horizontal-panel :items [(sc/label :id :side1-fire)])
                                                     (sc/horizontal-panel :items [(sc/label :id :side1-morale)])
                                                     (sc/horizontal-panel :items [(sc/label :id :side2-fire)])
                                                     (sc/horizontal-panel :items [(sc/label :id :side2-morale)])])
                 :option-type :default)
      (dice e loc)))

(defn version [_])