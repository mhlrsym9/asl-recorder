(ns asl-recorder.info
  (:require [asl-recorder.swing-worker]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.zip :as zip]
            [seesaw [core :as sc]])
  (:import [java.awt Cursor]
           [java.beans PropertyChangeListener]
           [java.util.concurrent ExecutionException]
           [javax.swing SwingWorker$StateValue]))

(comment (zip-xml/attr= :action "Advancing Fire")
         (zip-xml/tag= :die-roll)
         (zip-xml/attr= :color "white")
         zip-xml/text)

(defn- extract-the-dice-locations [the-xml-zip action color]
  (zip-xml/xml-> the-xml-zip
                 :turn :side :phase :event
                 (zip-xml/attr= :action action)
                 :die-rolls :die-roll
                 (zip-xml/attr= :color color)))

(defn- extract-the-dice-rolls [the-xml-zip action]
  (let [die-fn (comp first zip/down)]
    (apply map #(+ (die-fn %1) (die-fn %2)) (map (partial extract-the-dice-locations the-xml-zip action) ["white" "colored"]))))

(defn dice [e loc]
  (let [r (sc/to-root e)
        sw (proxy [asl_recorder.swing_worker] []
             (doInBackground []
               (let [xml-zip (-> loc zip/root zip/xml-zip)
                     the-dice-rolls (mapcat (partial extract-the-dice-rolls xml-zip) ["Prep Fire" "Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP" "Final Fire" "Advancing Fire"])]
                 (proxy-super publishFromClojure (into-array String [(str "Average white die for fire attacks = "
                                                                          (format "%.2f" (double (/ (apply + the-dice-rolls) (count the-dice-rolls)))))]))))
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