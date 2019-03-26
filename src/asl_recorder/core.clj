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
           [javax.swing JFrame SwingWorker SwingWorker$StateValue JOptionPane ButtonGroup])
  (:gen-class))

(def rally-phase-map {"Reinforcements" "ATTACKER Recovery"
                      "ATTACKER Recovery" "DEFENDER Recovery"
                      "DEFENDER Recovery" "ATTACKER Repair"
                      "ATTACKER Repair" "DEFENDER Repair"
                      "DEFENDER Repair" "ATTACKER Transfer"
                      "ATTACKER Transfer" "DEFENDER Transfer"
                      "DEFENDER Transfer" "ATTACKER Self-Rally"
                      "ATTACKER Self-Rally" "DEFENDER Self-Rally"
                      "DEFENDER Self-Rally" "ATTACKER Unit Rally"
                      "ATTACKER Unit Rally" "DEFENDER Unit Rally"
                      "DEFENDER Unit Rally" "Reinforcements"})

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

(def game (xml/element :game {:name "War of the Rats" :number-turns "6" :side1 "German" :side2 "Russian"}
                       (xml/element :turn {:number 1}
                                    (xml/element :side {:attacker "German"}
                                                 (xml/element :phase {:name "Rally"})))))

(def game-zip-loc (atom (-> game
                            zip/xml-zip
                            zip/down
                            zip/rightmost
                            zip/down
                            zip/rightmost
                            zip/down
                            zip/rightmost)))

(defn- process-the-game [e])

(defn- update-rally-time [rally next-rally]
  (sc/text! rally next-rally))

(defn- update-time [turn next-turn attacker next-attacker phase next-phase]
  (sc/text! turn next-turn)
  (sc/text! attacker next-attacker)
  (sc/text! phase next-phase))

(defn append-event [loc the-type the-description the-result]
  (let [n (zip/node loc)
        c (conj (vec (:content n)) (xml/element :event {:type the-type} (list (xml/element :action {} the-description)
                                                                (xml/element :result {} the-result))))]
    (zip/replace loc (assoc n :content c))))

(defn append-phase [loc the-phase]
  (let [l (zip/up loc)
        n (zip/node l)
        c (conj (vec (:content n)) (xml/element :phase {:name the-phase}))]
    (-> l (zip/replace (assoc n :content c)) zip/down zip/rightmost)))

(defn append-attacker [loc the-attacker]
  (let [l (-> loc zip/up zip/up)
        n (zip/node l)
        c (conj (vec (:content n)) (xml/element :side {:attacker the-attacker}
                                          (xml/element :phase {:name "Rally"})))]
    (-> l (zip/replace (assoc n :content c)) zip/down zip/rightmost zip/down zip/rightmost)))

(defn append-turn [loc the-turn]
  (let [l (-> loc zip/up zip/up zip/up)
        n (zip/node l)
        c (conj (vec (:content n)) (xml/element :turn {:number the-turn}
                                          (xml/element :side {:attacker "German"}
                                                       (xml/element :phase {:name "Rally"}))))]
    (-> l (zip/replace (assoc n :content c)) zip/down zip/rightmost zip/down zip/rightmost zip/down zip/rightmost)))

(defn get-game-phase [loc]
  (-> loc zip/node :attrs :name))

(defn get-game-attacker [loc]
  (-> loc zip/up zip/node :attrs :attacker))

(defn get-game-turn [loc]
  (-> loc zip/up zip/up zip/node :attrs :number))

(defn- add-rally-event [rally description result]
  (fn [_]
    (swap! game-zip-loc append-event (sc/text rally) (sc/text description) (sc/text result))
    (sc/text! description "")
    (sc/text! result "")))

(defn advance-game-rally-phase [loc current-rally-phase]
  (let [next-rally-phase (get rally-phase-map current-rally-phase)
        current-phase (get-game-phase loc)
        next-phase? (= next-rally-phase "Reinforcements")
        next-phase (if next-phase?
                     (get phase-map current-phase)
                     current-phase)
        new-loc (cond next-phase? (append-phase loc next-phase)
                      :else loc)]
    {:next-phase next-phase :next-rally-phase next-rally-phase :new-loc new-loc}))

(defn advance-game-phase [loc]
  (let [current-phase (get-game-phase loc)
        next-phase (get phase-map current-phase)
        current-attacker (get-game-attacker loc)
        next-attacker? (= next-phase "Rally")
        next-attacker (if next-attacker?
                        (get attacker-map current-attacker)
                        current-attacker)
        current-turn (get-game-turn loc)
        next-turn? (and next-attacker? (= next-attacker "German"))
        next-turn (if next-turn?
                    (inc current-turn)
                    current-turn)
        new-loc (cond next-turn? (append-turn loc next-turn)
                      next-attacker? (append-attacker loc next-attacker)
                      :else (append-phase loc next-phase))]
    {:next-turn next-turn :next-attacker next-attacker :next-phase next-phase :new-loc new-loc}))

(defn advance-game-attacker [loc]
  (let [current-attacker (get-game-attacker loc)]
    (loop [{:keys [next-attacker next-phase new-loc] :as r}
           {:next-turn (get-game-turn loc) :next-attacker current-attacker :next-phase (get-game-phase loc) :new-loc loc}]
      (if (and (= "Rally" next-phase) (not= current-attacker next-attacker))
        r
        (recur (advance-game-phase new-loc))))))

(defn advance-game-turn [loc]
  (let [current-turn (get-game-turn loc)]
    (loop [{:keys [next-turn next-attacker new-loc] :as r}
           {:next-turn current-turn :next-attacker (get-game-attacker loc) :next-phase (get-game-phase loc) :new-loc loc}]
      (if (and (= "German" next-attacker) (not= current-turn next-turn))
        r
        (recur (advance-game-phase new-loc))))))

(defn- advance-rally-phase [turn attacker phase rally-phase]
  (fn [_] (let [{:keys [next-phase next-rally-phase new-loc]} (advance-game-rally-phase @game-zip-loc (sc/text rally-phase))]
            (reset! game-zip-loc new-loc)
            (update-time turn (sc/text turn) attacker (sc/text attacker) phase next-phase)
            (update-rally-time rally-phase next-rally-phase))))

(defn- advance-phase [turn attacker phase]
  (fn [_] (let [{:keys [next-turn next-attacker next-phase new-loc]} (advance-game-phase @game-zip-loc)]
            (reset! game-zip-loc new-loc)
            (update-time turn next-turn attacker next-attacker phase next-phase))))

(defn- advance-attacker [turn attacker phase]
  (fn [_] (let [{:keys [next-turn next-attacker next-phase new-loc]} (advance-game-attacker @game-zip-loc)]
            (reset! game-zip-loc new-loc)
            (update-time turn next-turn attacker next-attacker phase next-phase))))

(defn- advance-turn [turn attacker phase]
  (fn [_] (let [{:keys [next-turn next-attacker next-phase new-loc]} (advance-game-turn @game-zip-loc)]
            (reset! game-zip-loc new-loc)
            (update-time turn next-turn attacker next-attacker phase next-phase))))

(defn- create-die-group [prefix]
  (let [the-info (map #(hash-map :id (keyword (str prefix "-" %1)) :text %2)
                      ["one" "two" "three" "four" "five" "six"]
                      ["1" "2" "3" "4" "5" "6"])
        the-class (keyword (str prefix "-" "die-class"))
        the-button-group (sc/button-group)]
    (for [{:keys [id text]} the-info]
      (vector (sc/radio :id id :class the-class :text text :group the-button-group)))))

(defn -main
  [& args]
  (sc/invoke-later
    (sc/with-widgets [(sc/label :id :turn :text "1")
                      (sm/mig-panel :id :turn-line :constraints ["" "nogrid" ""] :items [["Turn:"] [turn "grow"]])
                      (sc/button :id :advance-turn-button :text "Next Turn")
                      (sc/button :id :rewind-turn-button :text "Previous Turn")
                      (sm/mig-panel :id :turn-panel :constraints [] :items [[turn-line "span 2, align center, wrap"]
                                                                            [advance-turn-button] [rewind-turn-button]])

                      (sc/label :id :attacker :text "German")
                      (sm/mig-panel :id :attacker-line :constraints ["" "nogrid" ""] :items [["Attacker:"] [attacker "grow"]])
                      (sc/button :id :advance-attacker-button :text "Next Attacker")
                      (sc/button :id :rewind-attacker-button :text "Previous Attacker")
                      (sm/mig-panel :id :attacker-panel :constraints [] :items [[attacker-line "span 2, align center, wrap"]
                                                                                [advance-attacker-button] [rewind-attacker-button]])

                      (sc/label :id :phase :text "Rally")
                      (sm/mig-panel :id :phase-line :constraints ["" "nogrid" ""] :items [["Phase:"] [phase "grow"]])
                      (sc/button :id :advance-phase-button :text "Next Phase")
                      (sc/button :id :rewind-phase-button :text "Previous Phase")
                      (sm/mig-panel :id :phase-panel :constraints [] :items [[phase-line "span 2, align center, wrap"]
                                                                             [advance-phase-button] [rewind-phase-button]])

                      (sm/mig-panel :id :game-position-panel :constraints [] :items [[turn-panel] [attacker-panel] [phase-panel]])

                      (sc/label :id :rally-phase :text "Reinforcements")
                      (sm/mig-panel :id :rally-phase-line :constraints ["" "nogrid" ""] :items [["Rally Phase:"] [rally-phase "grow"]])
                      (sc/button :id :advance-rally-phase-button :text "Next Rally Phase")
                      (sc/button :id :rewind-rally-phase-button :text "Previous Rally Phrase")
                      (sm/mig-panel :id :rally-panel :constraints [] :items [[rally-phase-line "span 2, align center, wrap"]
                                                                             [advance-rally-phase-button] [rewind-rally-phase-button]])

                      (sc/text :id :description)
                      (sm/mig-panel :id :white-die :constraints [] :items (concat (vector (vector "White Die:")) (create-die-group "white")))
                      (sm/mig-panel :id :colored-die :constraints [] :items (concat (vector (vector "Colored Die:")) (create-die-group "colored")))
                      (sc/text :id :final-modifiers)
                      (sc/text :id :result)
                      (sc/button :id :add-rally-event-button :text "Add event")
                      (sm/mig-panel :id :event-panel :constraints ["" "[|fill,grow]" ""] :items [["Description:"] [description "wrap"]
                                                                                                 [white-die "span, wrap"]
                                                                                                 [colored-die "span, wrap"]
                                                                                                 ["Final Modifiers:"] [final-modifiers "wrap"]
                                                                                                 ["Result:"] [result "wrap"]
                                                                                                 [add-rally-event-button "span, align center"]])

                      (sm/mig-panel :id :game-event-panel :constraints ["" "[|fill,grow]"] :items [[rally-panel] [event-panel]])

                      (sc/button :id :ok :text "OK" :enabled? false)]

                     (let [ok-fn (fn [e] (process-the-game e))]
                       (sc/listen advance-turn-button :action (advance-turn turn attacker phase))
                       (sc/listen advance-attacker-button :action (advance-attacker turn attacker phase))
                       (sc/listen advance-phase-button :action (advance-phase turn attacker phase))
                       (sc/listen advance-rally-phase-button :action (advance-rally-phase turn attacker phase rally-phase))
                       (sc/listen add-rally-event-button :action (add-rally-event rally-phase description result))
                       (sc/listen ok :action ok-fn)
                       (-> (sc/frame :title "ASL Recorder",
                                     :content (sm/mig-panel :constraints [] :items [[game-position-panel "wrap"]
                                                                                    [game-event-panel "growx, wrap"]
                                                                                    [ok "align center"]]),
                                     :on-close :exit)
                           sc/pack!
                           sc/show!)))))
