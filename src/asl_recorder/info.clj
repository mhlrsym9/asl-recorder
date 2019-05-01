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

(defn dice [e loc]
  (let [r (sc/to-root e)
        sw (proxy [asl_recorder.swing_worker] []
             (doInBackground []
               (let [the-dice-locs (-> loc
                                       zip/root
                                       zip/xml-zip
                                       (zip-xml/xml-> :turn :side :phase :event
                                                      (zip-xml/attr= :action "Advancing Fire")
                                                      :die-rolls :die-roll
                                                      (zip-xml/attr= :color "white")))
                     the-sum (reduce (fn [r v] (+ r ((comp first zip/down) v))) 0 the-dice-locs)]
                 (proxy-super publishFromClojure (into-array String [(str "Average white die for fire attacks = "
                                                                          (format "%.2f" (double (/ the-sum (count the-dice-locs)))))]))))
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