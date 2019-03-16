(ns asl-recorder.core
  (:require [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [asl-recorder.swing-worker]
            [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd]])
  (:import [java.awt Cursor]
           [java.beans PropertyChangeEvent PropertyChangeListener]
           [java.io File ByteArrayOutputStream]
           [java.util.concurrent ExecutionException]
           [javax.swing JFrame SwingWorker SwingWorker$StateValue JOptionPane])
  (:gen-class))

(def phase-map {"Rally" "Prep Fire"
                "Prep Fire" "Movement"
                "Movement" "Defensive Fire"
                "Defensive Fire" "Advancing Fire"
                "Advancing Fire" "Rout"
                "Rout" "Advance"
                "Advance" "Close Combat"
                "Close Combat" "Rally"})

(def attacker-map {"German" "Russian"
                   "Russian" "German"})

(defn- process-the-game [e])

(defn- inc-turn [turn]
  (inc (Integer/parseInt (sc/text turn))))

(defn- update-time [turn next-turn attacker next-attacker phase next-phase]
  (sc/text! turn next-turn)
  (sc/text! attacker next-attacker)
  (sc/text! phase next-phase))

(defn- advance-turn [turn attacker phase]
  (fn [_] (let [next-turn (inc-turn turn)]
            (update-time turn next-turn attacker "German" phase "Rally"))))

(defn- advance-attacker [turn attacker phase]
  (fn [_] (let [next-attacker (get attacker-map (sc/text attacker))
                next-turn (if (= next-attacker "German")
                            (inc-turn turn)
                            (sc/text turn))]
            (update-time turn next-turn attacker next-attacker phase "Rally"))))

(defn- advance-phase [turn attacker phase]
  (fn [_] (let [next-phase (get phase-map (sc/text phase))
                current-attacker (sc/text attacker)
                rally-phase? (= next-phase "Rally")
                next-attacker (if rally-phase?
                                (get attacker-map current-attacker)
                                current-attacker)
                next-turn (if (and rally-phase? (= next-attacker "German"))
                            (inc-turn turn)
                            (sc/text turn))]
            (update-time turn next-turn attacker next-attacker phase next-phase))))

(defn -main
  [& args]
  (sc/invoke-later
    (sc/with-widgets [(sc/label :id :turn :text "1")
                      (sc/button :id :advance-turn-button :text "Next Turn")
                      (sc/label :id :attacker :text "German")
                      (sc/button :id :advance-attacker-button :text "Next attacker")
                      (sc/label :id :phase :text "Rally")
                      (sc/button :id :advance-phase-button :text "Next phase")
                      (sc/button :id :ok :text "OK" :enabled? false)]
                     (let [ok-fn (fn [e] (process-the-game e))]
                       (sc/listen advance-turn-button :action (advance-turn turn attacker phase))
                       (sc/listen advance-attacker-button :action (advance-attacker turn attacker phase))
                       (sc/listen advance-phase-button :action (advance-phase turn attacker phase))
                       (sc/listen ok :action ok-fn)
                       (-> (sc/frame :title "ASL Recorder",
                                     :content (sm/mig-panel :constraints [] :items [["Turn:"] [turn "w 200!"] [advance-turn-button "wrap"]
                                                                                    ["Attacker:"] [attacker "w 200!"] [advance-attacker-button "wrap"]
                                                                                    ["Phase:"] [phase "w 200!"] [advance-phase-button "wrap"]
                                                                                    [ok "span, align center"]]),
                                     :on-close :exit)
                           sc/pack!
                           sc/show!)))))
