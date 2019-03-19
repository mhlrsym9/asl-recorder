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

(def game-zip-loc (atom (-> (xml/element :game {:name "War of the Rats" :number-turns "6" :side1 "German" :side2 "Russian"}
                                         (xml/element :turn {:number 1}
                                                      (xml/element :side {:attacker "German"}
                                                                   (xml/element :phase {:name "Rally"}))))
                            zip/xml-zip
                            zip/down
                            zip/down
                            zip/down)))

(defn- process-the-game [e])

(defn- inc-turn [the-turn]
  (inc (Integer/parseInt the-turn)))

(defn- update-time [turn next-turn attacker next-attacker phase next-phase]
  (sc/text! turn next-turn)
  (sc/text! attacker next-attacker)
  (sc/text! phase next-phase))

(defn append-event [loc the-action the-result]
  (let [n (zip/node loc)
        c (conj (:content n) (xml/element :event {} (list (xml/element :action {} the-action)
                                                          (xml/element :result {} the-result))))]
    (zip/replace loc (assoc n :content c))))

(defn append-phase [loc the-phase]
  (let [l (zip/up loc)
        n (zip/node l)
        c (conj (:content n) (xml/element :phase {:name the-phase}))]
    (-> l (zip/replace (assoc n :content c)) zip/down)))

(defn append-attacker [loc the-attacker]
  (let [l (-> loc zip/up zip/up)
        n (zip/node l)
        c (conj (:content n) (xml/element :side {:attacker the-attacker}
                                          (xml/element :phase {:name "Rally"})))]
    (-> l (zip/replace (assoc n :content c)) zip/down zip/down)))

(defn append-turn [loc the-turn]
  (let [l (-> loc zip/up zip/up zip/up)
        n (zip/node l)
        c (conj (:content n) (xml/element :turn {:number the-turn}
                                          (xml/element :side {:attacker "German"}
                                                       (xml/element :phase {:name "Rally"}))))]
    (-> l (zip/replace (assoc n :content c)) zip/down zip/down zip/down)))

(defn get-game-phase [loc]
  (-> loc zip/node :attrs :name))

(defn get-game-attacker [loc]
  (-> loc zip/up zip/node :attrs :attacker))

(defn get-game-turn [loc]
  (-> loc zip/up zip/up zip/node :attrs :number))

(defn- add-event [action result]
  (fn [_]
    (swap! game-zip-loc append-event (sc/text action) (sc/text result))
    (sc/text! action "")
    (sc/text! result "")))

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
                    (inc-turn current-turn)
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

(defn -main
  [& args]
  (sc/invoke-later
    (sc/with-widgets [(sc/label :id :turn :text "1")
                      (sc/button :id :advance-turn-button :text "Next Turn")
                      (sc/label :id :attacker :text "German")
                      (sc/button :id :advance-attacker-button :text "Next attacker")
                      (sc/label :id :phase :text "Rally")
                      (sc/button :id :advance-phase-button :text "Next phase")
                      (sc/text :id :action)
                      (sc/text :id :result)
                      (sc/button :id :add-event-button :text "Add event")
                      (sc/button :id :ok :text "OK" :enabled? false)]
                     (let [ok-fn (fn [e] (process-the-game e))]
                       (sc/listen advance-turn-button :action (advance-turn turn attacker phase))
                       (sc/listen advance-attacker-button :action (advance-attacker turn attacker phase))
                       (sc/listen advance-phase-button :action (advance-phase turn attacker phase))
                       (sc/listen add-event-button :action (add-event action result))
                       (sc/listen ok :action ok-fn)
                       (-> (sc/frame :title "ASL Recorder",
                                     :content (sm/mig-panel :constraints [] :items [["Turn:"] [turn "w 100!"] ["Attacker:"] [attacker "w 100!"] ["Phase:"] [phase "w 100!, wrap"]
                                                                                    [advance-turn-button "span 2, align center"] [advance-attacker-button "span 2, align center"] [advance-phase-button "span 2, align center, wrap"]
                                                                                    ["Action:"] [action "span 5, growx, wrap"]
                                                                                    ["Result:"] [result "span 5, growx, wrap"]
                                                                                    [add-event-button "span, align center, wrap"]
                                                                                    [ok "span, align center"]]),
                                     :on-close :exit)
                           sc/pack!
                           sc/show!)))))
