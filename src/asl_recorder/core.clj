(ns asl-recorder.core
  (:require [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [asl-recorder.swing-worker]
            [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd]]
            [clojure.string :as str])
  (:import [java.awt Cursor]
           [java.beans PropertyChangeEvent PropertyChangeListener]
           [java.io File ByteArrayOutputStream]
           [java.util.concurrent ExecutionException]
           [javax.swing JFrame SwingWorker SwingWorker$StateValue JOptionPane ButtonGroup])
  (:gen-class))

(declare transition-to-rally-phase-reinforcements
         transition-to-rally-phase-recovery
         transition-to-rally-phase-repair
         transition-to-rally-phase-transfer
         transition-to-rally-phase-self-rally
         transition-to-rally-phase-unit-rally
         transition-to-prep-fire
         transition-to-movement
         transition-to-defensive-fire
         transition-to-advancing-fire
         transition-to-attacker-rout
         transition-to-defender-rout
         transition-to-advance
         transition-to-close-combat
         perform-rally-phase-activations
         perform-fire-phase-activations
         perform-rout-phase-activations
         perform-advance-phase-activations
         perform-close-combat-phase-activations)

(def rally-phase-map {"Reinforcements"      {:next-sub-phase "ATTACKER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "ATTACKER Recovery"   {:next-sub-phase "DEFENDER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "DEFENDER Recovery"   {:next-sub-phase "ATTACKER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "ATTACKER Repair"     {:next-sub-phase "DEFENDER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "DEFENDER Repair"     {:next-sub-phase "ATTACKER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "ATTACKER Transfer"   {:next-sub-phase "DEFENDER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "DEFENDER Transfer"   {:next-sub-phase "ATTACKER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "ATTACKER Self-Rally" {:next-sub-phase "DEFENDER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "DEFENDER Self-Rally" {:next-sub-phase "ATTACKER Unit Rally" :transition-fn #'transition-to-rally-phase-unit-rally}
                      "ATTACKER Unit Rally" {:next-sub-phase "DEFENDER Unit Rally" :transition-fn #'transition-to-rally-phase-unit-rally}
                      "DEFENDER Unit Rally" {:next-sub-phase nil :transition-fn #'transition-to-prep-fire}})

(def phase-map {"Rally"          {:next-phase "Prep Fire" :transition-fn #'transition-to-prep-fire :activation-fn #'perform-rally-phase-activations}
                "Prep Fire"      {:next-phase "Movement" :transition-fn #'transition-to-movement :activation-fn #'perform-fire-phase-activations}
                "Movement"       {:next-phase "Defensive Fire" :transition-fn #'transition-to-defensive-fire :activation-fn #'perform-fire-phase-activations}
                "Defensive Fire" {:next-phase "Advancing Fire" :transition-fn #'transition-to-advancing-fire :activation-fn #'perform-fire-phase-activations}
                "Advancing Fire" {:next-phase "Rout" :transition-fn #'transition-to-attacker-rout :activation-fn #'perform-fire-phase-activations}
                "Rout"           {:next-phase "Advance" :transition-fn #'transition-to-advance :activation-fn #'perform-rout-phase-activations}
                "Advance"        {:next-phase "Close Combat" :transition-fn #'transition-to-close-combat :activation-fn #'perform-advance-phase-activations}
                "Close Combat"   {:next-phase "Rally" :transition-fn #'transition-to-rally-phase-reinforcements :activation-fn #'perform-close-combat-phase-activations}})

(def rout-phase-map {"ATTACKER" {:next-sub-phase "DEFENDER" :transition-fn #'transition-to-defender-rout}
                     "DEFENDER" {:next-sub-phase nil :transition-fn #'transition-to-advance}})

(def attacker-map {"German" "Russian"
                   "Russian" "German"})

(def game-start (xml/element :game {:name "War of the Rats" :number-full-turns "6" :additional-half-turn false :side1 "German" :side2 "Russian"}
                             (xml/element :turn {:number 1}
                                          (xml/element :side {:attacker "German"}
                                                       (xml/element :phase {:name "Rally"})))))

(def the-game (atom {:game-zip-loc (-> game-start
                                       zip/xml-zip
                                       zip/down
                                       zip/rightmost
                                       zip/down
                                       zip/rightmost
                                       zip/down
                                       zip/rightmost)
                     :is-modified? false
                     :file-name    nil}))

(def white "white")
(def colored "colored")
(def random-label "-random-label")
(def random-die-panel "-random-die-panel")

(defn- create-random-dice-label-id [color]
  (keyword (str color random-label)))

(defn- create-random-dice-label-id-select [color]
  (keyword (str "#" color random-label)))

(defn- create-random-dice-panel-id [color]
  (keyword (str color random-die-panel)))

(defn- create-random-dice-panel-id-select [color]
  (keyword (str "#" color random-die-panel)))

(def random-dice-info (apply conj [{:color           white
                                    :label-id        (create-random-dice-label-id white)
                                    :label-id-select (create-random-dice-label-id-select white)
                                    :label-text      "White Die:"
                                    :panel-id        (create-random-dice-panel-id white)
                                    :panel-id-select (create-random-dice-panel-id-select white)
                                    :visible?        true}]
                             (map (fn [n] (let [color (str "colored-" n)]
                                            {:color           color
                                             :label-id        (create-random-dice-label-id color)
                                             :label-id-select (create-random-dice-label-id-select color)
                                             :label-text      (str "Colored " n " Die:")
                                             :panel-id        (create-random-dice-panel-id color)
                                             :panel-id-select (create-random-dice-panel-id-select color)
                                             :visible?        (if (= 1 n) true false)}))
                                  (range 1 10))))

(defn- process-the-game [e])

(defn- update-time [turn next-turn attacker next-attacker phase next-phase]
  (sc/text! turn next-turn)
  (sc/text! attacker next-attacker)
  (sc/text! phase next-phase))

(defn append-event [loc sub-phase the-action-option the-description the-die-rolls the-final-modifier the-attacker-final-modifier the-defender-final-modifier the-result]
  (let [n (zip/node loc)
        sub-phase-text? (-> sub-phase str/blank? not)
        c (conj (vec (:content n)) (xml/element :event (merge {:action the-action-option} (when sub-phase-text? {:sub-phase sub-phase}))
                                                (filter identity (list (xml/element :description {} the-description)
                                                                       (when the-die-rolls
                                                                         (xml/element :die-rolls {} (map #(xml/element :die-roll
                                                                                                                       {:color (:color %)}
                                                                                                                       (:die-roll %))
                                                                                                         the-die-rolls)))
                                                                       (when the-final-modifier
                                                                         (xml/element :final-modifier {} the-final-modifier))
                                                                       (when the-attacker-final-modifier
                                                                         (xml/element :attacker-final-modifier {} the-attacker-final-modifier))
                                                                       (when the-defender-final-modifier
                                                                         (xml/element :defender-final-modifier {} the-defender-final-modifier))
                                                                       (when the-result
                                                                         (xml/element :result {} the-result))))))]
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

(defn advance-game-sub-phase [loc current-sub-phase sub-phase-map]
  (let [{:keys [next-sub-phase] :as next-sub-phase-info} (get sub-phase-map current-sub-phase)
        current-phase (get-game-phase loc)
        next-phase? (nil? next-sub-phase)
        next-phase (if next-phase?
                     (:next-phase (get phase-map current-phase))
                     current-phase)
        new-loc (cond next-phase? (append-phase loc next-phase)
                      :else loc)]
    {:next-phase next-phase :next-sub-phase-info next-sub-phase-info :new-loc new-loc}))

(defn advance-game-phase [loc]
  (let [current-phase (get-game-phase loc)
        {:keys [next-phase] :as next-phase-info} (get phase-map current-phase)
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
    {:next-turn next-turn :next-attacker next-attacker :next-phase-info next-phase-info :new-loc new-loc}))

(defn advance-game-attacker [loc]
  (let [current-attacker (get-game-attacker loc)
        current-phase (get-game-phase loc)
        transition-fn (->> current-phase (get phase-map) :transition-fn)]
    (loop [{next-attacker :next-attacker, new-loc :new-loc, {next-phase :next-phase} :next-phase-info, :as r}
           {:next-turn (get-game-turn loc) :next-attacker current-attacker :next-phase-info {:next-phase current-phase :transition-fn transition-fn} :new-loc loc}]
      (if (and (= "Rally" next-phase) (not= current-attacker next-attacker))
        r
        (recur (advance-game-phase new-loc))))))

(defn advance-game-turn [loc]
  (let [current-turn (get-game-turn loc)
        current-phase (get-game-phase loc)
        transition-fn (->> current-phase (get phase-map) :transition-fn)]
    (loop [{:keys [next-turn next-attacker new-loc] :as r}
           {:next-turn current-turn :next-attacker (get-game-attacker loc) :next-phase-info {:next-phase current-phase :transition-fn transition-fn} :new-loc loc}]
      (if (and (= "German" next-attacker) (not= current-turn next-turn))
        r
        (recur (advance-game-phase new-loc))))))

(defn- update-the-game [new-loc]
  (swap! the-game assoc :game-zip-loc new-loc :is-modified? true))

(defn- perform-advance-sub-phase [e sub-phase-map]
  (let [r (sc/to-root e)
        sub-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        turn (sc/select r [:#turn])
        attacker (sc/select r [:#attacker])
        phase (sc/select r [:#phase])
        {:keys [next-phase new-loc] {:keys [transition-fn next-sub-phase]} :next-sub-phase-info}
        (-> the-game deref :game-zip-loc (advance-game-sub-phase sub-phase-text sub-phase-map))]
    (update-the-game new-loc)
    (update-time turn (sc/text turn) attacker (sc/text attacker) phase next-phase)
    (transition-fn e next-sub-phase)))

(defn- advance-sub-phase [e]
  (let [r (sc/to-root e)
        phase-text (-> r (sc/select [:#phase]) sc/text)]
    (cond (= "Rally" phase-text) (perform-advance-sub-phase e rally-phase-map)
          (= "Rout" phase-text) (perform-advance-sub-phase e rout-phase-map))))

(defn- advance-phase [e]
  (let [r (sc/to-root e)
        turn (sc/select r [:#turn])
        attacker (sc/select r [:#attacker])
        phase (sc/select r [:#phase])
        {:keys [next-turn next-attacker new-loc], {:keys [next-phase transition-fn]} :next-phase-info}
        (-> the-game deref :game-zip-loc advance-game-phase)]
    (update-the-game new-loc)
    (update-time turn next-turn attacker next-attacker phase next-phase)
    (transition-fn e)
    e))

(defn- advance-attacker [e]
  (let [{:keys [turn attacker phase]} (sc/group-by-id (sc/to-root e))
        {:keys [next-turn next-attacker new-loc] {:keys [next-phase transition-fn]} :next-phase-info}
        (-> the-game deref :game-zip-loc advance-game-attacker)]
    (update-the-game new-loc)
    (update-time turn next-turn attacker next-attacker phase next-phase)
    (transition-fn e)
    e))

(defn- advance-turn [e]
  (let [{:keys [turn attacker phase]} (sc/group-by-id (sc/to-root e))
        {:keys [next-turn next-attacker new-loc] {:keys [next-phase transition-fn]} :next-phase-info}
        (-> the-game deref :game-zip-loc advance-game-turn)]
    (update-the-game new-loc)
    (update-time turn next-turn attacker next-attacker phase next-phase)
    (transition-fn e)
    e))

(defn- create-die-info [prefix]
  (map #(hash-map :id (keyword (str prefix "-" %1)) :text (str %2) :user-data %2)
       ["one" "two" "three" "four" "five" "six"]
       (range 1 7)))

(defn create-die-radio-buttons [color]
  (let [the-info (create-die-info color)
        the-class (keyword (str color "-" "die-class"))
        the-button-group (sc/button-group)
        the-radio-buttons (for [{:keys [id text user-data]} the-info]
                            (vector (sc/radio :id id :class the-class :text text :group the-button-group :user-data user-data)))]
    {:color color :button-group the-button-group :radio-buttons the-radio-buttons}))

(defn- update-die-radio-buttons-enabled-state [die-panel enabled?]
  (let [the-info (create-die-info (-> die-panel sc/user-data :color))
        the-radio-buttons (map #(get (sc/group-by-id die-panel) %) (map :id the-info))]
    (dorun (map (fn [i] (sc/config! i :enabled? enabled?)) the-radio-buttons))))

(defn- disable-die-radio-buttons [die-panel]
  (update-die-radio-buttons-enabled-state die-panel false))

(defn- enable-die-radio-buttons [die-panel]
  (update-die-radio-buttons-enabled-state die-panel true))

(defn- find-selected-die-radio-button [die-panel]
  (first (filter identity (map #(when (sc/selection %) %) (sc/select die-panel [:JRadioButton])))))

(defn- gather-die-roll [die-panel]
  (when-let [selected (-> die-panel sc/user-data :button-group sc/selection)]
    (hash-map :die-roll (sc/user-data selected) :color (-> die-panel sc/user-data :color))))

(defn gather-die-rolls [die-panels]
  (doall (filter identity (map gather-die-roll die-panels))))

(defn- clear-die-roll [die-panel]
  (-> die-panel sc/user-data :button-group (sc/selection! false)))

(defn clear-die-rolls [die-panels]
  (dorun (map clear-die-roll die-panels)))

(defn- selected-die-radio-button? [die-panel]
  (seq (filter identity (map sc/selection (sc/select die-panel [:JRadioButton])))))

(defn- add-change-listener-to-die-radio-buttons [die-panel f]
  (dorun (map #(sc/listen % :selection f) (sc/select die-panel [:JRadioButton]))))

(defn- activate-die-panel [e action-options die-panel]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/text)]
    (update-die-radio-buttons-enabled-state (sc/select r [die-panel])
                                            ((complement not-any?) #{action-option-text} action-options))))

(defn- activate-white-die-during-rally-phase [e]
  (activate-die-panel e ["Recover SW" "Repair SW" "Self Rally" "Wound Resolution" "Leader Creation" "Unit Rally" "Other"] :#white-die-panel))

(defn- activate-colored-die-during-rally-phase [e]
  (activate-die-panel e ["Self Rally" "Unit Rally" "Other"] :#colored-die-panel))

(defn- activate-final-modifier-during-rally-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/text)
        final-modifier (sc/select r [:#final-modifier])
        enabled? ((complement not-any?) #{action-option-text} ["Recover SW" "Repair SW" "Self Rally" "Unit Rally" "Other"])]
    (sc/config! final-modifier :enabled? enabled?)))

(defn- activate-result-during-rally-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/text)
        result (sc/select r [:#result])
        enabled? ((complement not-any?) #{action-option-text} ["Recover SW" "Repair SW" "Self Rally" "Unit Rally" "Other"])]
    (sc/config! result :enabled? enabled?)))

(defn- activate-event-button-during-rally-phase [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        final-modifier-text (-> r (sc/select [:#final-modifier]) sc/selection)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{action-option-text} ["Place Reinforcements" "Transfer SW"])
                      description-text?
                      (some #{action-option-text} ["Recover SW" "Repair SW"])
                      (and description-text? white-die-selected? final-modifier-text result-text?)
                      (some #{action-option-text} ["Self Rally" "Unit Rally"])
                      (and description-text? white-die-selected? colored-die-selected? result-text?)
                      (some #{action-option-text} ["Wound Resolution" "Leader Creation"])
                      (and description-text? white-die-selected? result-text?)
                      (= "Other" action-option-text)
                      (or description-text? result-text?)
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-rally-phase-activations [e]
  (let [r (sc/to-root e)
        fields (vec (map #(sc/select r (vector %)) [:#movement-factors :#movement-points :#firepower]))]
    (sc/config! (-> r (sc/select [:#description])) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (activate-white-die-during-rally-phase e)
    (activate-colored-die-during-rally-phase e)
    (sc/config! fields :enabled? false)
    (sc/hide! (sc/select r [:#split-final-modifier-panel]))
    (sc/show! (sc/select r [:#final-modifier-panel]))
    (activate-final-modifier-during-rally-phase e)
    (sc/config! (sc/select r [:#attacker-final-modifier]) :enabled? false)
    (sc/config! (sc/select r [:#defender-final-modifier]) :enabled? false)
    (activate-result-during-rally-phase e)
    (activate-event-button-during-rally-phase e)))

(defn- update-random-dice [e id visible? rdi]
  (let [r (sc/to-root e)
        select-die-panel (fn [v] (sc/select r v))
        panels (map (comp select-die-panel vector id) rdi)]
    (dorun (map #(sc/config! % :visible? visible?) panels))))

(defn- activate-event-button-for-random-selection [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        number-dice-selection (-> r (sc/select [:#number-dice]) sc/selection)
        select-die-panel (fn [v] (sc/select r v))
        all-dice-selected? (every? selected-die-radio-button? (->> random-dice-info
                                                                   (take number-dice-selection)
                                                                   (map :panel-id-select)
                                                                   (map vector)
                                                                   (map select-die-panel)))
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (and description-text? all-dice-selected? result-text?)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-activations-for-random-selection [e]
  (let [r (sc/to-root e)
        number-dice-selection (-> r (sc/select [:#number-dice]) sc/selection)]
    (sc/config! (sc/select r [:#description]) :enabled? true)
    (sc/hide! (sc/select r [:#standard-event-panel]))
    (sc/show! (sc/select r [:#random-event-panel]))
    (sc/config! (sc/select r [:#number-dice]) :enabled? true)
    (->> random-dice-info
         (take number-dice-selection)
         (update-random-dice e :label-id-select true))
    (->> random-dice-info
         (drop number-dice-selection)
         (update-random-dice e :label-id-select false))
    (->> random-dice-info
         (take number-dice-selection)
         (update-random-dice e :panel-id-select true))
    (->> random-dice-info
         (drop number-dice-selection)
         (update-random-dice e :panel-id-select false))
    (sc/config! (sc/select r [:#result]) :enabled? true)
    (activate-event-button-for-random-selection e)
    (sc/pack! e)))

(defn- activate-white-die-during-fire-phase [e]
  (activate-die-panel e ["Place Smoke" "Recover SW"
                         "Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                         "Prep Fire" "Final Fire" "Advancing Fire"
                         "Morale Check" "Pin Task Check" "Wound Resolution" "Other"]
                      :#white-die-panel))

(defn- activate-colored-die-during-fire-phase [e]
  (activate-die-panel e ["Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                         "Prep Fire" "Final Fire" "Advancing Fire"
                         "Morale Check" "Pin Task Check" "Other"]
                      :#colored-die-panel))

(defn- activate-movement-factors-during-fire-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#movement-factors])
                :enabled? ((complement not-any?) #{action-option-text} ["Movement" "Assault Movement" "Place Smoke" "Recover SW" "Other"]))))

(defn- activate-firepower-during-fire-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#firepower])
                :enabled? ((complement not-any?) #{action-option-text} ["Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                                                                        "Prep Fire" "Final Fire" "Advancing Fire"
                                                                        "Other"]))))

(defn- activate-final-modifier-during-fire-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#final-modifier])
                :enabled? ((complement not-any?) #{action-option-text} ["Place Smoke" "Recover SW"
                                                                        "Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                                                                        "Prep Fire" "Final Fire" "Advancing Fire"
                                                                        "Morale Check" "Pin Task Check" "Wound Resolution" "Other"]))))

(defn- activate-result-during-fire-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#result])
                :enabled? ((complement not-any?) #{action-option-text} ["Place Smoke" "Recover SW"
                                                                        "Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                                                                        "Prep Fire" "Final Fire" "Advancing Fire"
                                                                        "Morale Check" "Pin Task Check" "Wound Resolution" "Other"]))))

(defn- activate-event-button-for-remaining-actions-during-fire-phase [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        movement-factors-text? (-> r (sc/select [:#movement-factors]) sc/text string/blank? not)
        firepower-text? (-> r (sc/select [:#firepower]) sc/text string/blank? not)
        final-modifier-text? (-> r (sc/select [:#final-modifier]) sc/selection)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{action-option-text} ["Movement" "Assault Movement"]) (and description-text? movement-factors-text?)
                      (= action-option-text "CX") description-text?
                      (some #{action-option-text} ["Place Smoke" "Recover SW"])
                      (and description-text? white-die-selected? movement-factors-text? final-modifier-text? result-text?)
                      (some #{action-option-text} ["Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                                                   "Prep Fire" "Final Fire" "Advancing Fire"])
                      (and description-text? white-die-selected? colored-die-selected? firepower-text? final-modifier-text? result-text?)
                      (some #{action-option-text} ["Morale Check" "Pin Task Check"])
                      (and description-text? white-die-selected? colored-die-selected? final-modifier-text? result-text?)
                      (= action-option-text "Wound Resolution") (and description-text? white-die-selected? final-modifier-text? result-text?)
                      (= action-option-text "Other") (or description-text? result-text?)
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-fire-phase-activations-for-remaining-actions [e]
  (let [r (sc/to-root e)]
    (sc/config! (sc/select r [:#description]) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (activate-white-die-during-fire-phase e)
    (activate-colored-die-during-fire-phase e)
    (activate-movement-factors-during-fire-phase e)
    (sc/config! (sc/select r [:#movement-points]) :enabled? false)
    (activate-firepower-during-fire-phase e)
    (sc/hide! (sc/select r [:#split-final-modifier-panel]))
    (sc/show! (sc/select r [:#final-modifier-panel]))
    (activate-final-modifier-during-fire-phase e)
    (sc/config! (sc/select r [:#attacker-final-modifier]) :enabled? false)
    (sc/config! (sc/select r [:#defender-final-modifier]) :enabled? false)
    (activate-result-during-fire-phase e)
    (activate-event-button-for-remaining-actions-during-fire-phase e)))

(defn- perform-fire-phase-activations [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (if (= "Random Selection" action-option-text)
      (perform-activations-for-random-selection e)
      (perform-fire-phase-activations-for-remaining-actions e))))

(defn- activate-white-die-during-rout-phase [e]
  (activate-die-panel e ["Interdiction" "Other"] :#white-die-panel))

(defn- activate-colored-die-during-rout-phase [e]
  (activate-die-panel e ["Interdiction" "Other"] :#colored-die-panel))

(defn- activate-movement-factors-during-rout-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#movement-factors])
                :enabled? ((complement not-any?) #{action-option-text} ["Rout" "Low Crawl" "Other"]))))

(defn- activate-final-modifier-during-rout-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#final-modifier])
                :enabled? ((complement not-any?) #{action-option-text} ["Interdiction" "Other"]))))

(defn- activate-result-during-rout-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#result])
                :enabled? ((complement not-any?) #{action-option-text} ["Interdiction" "Other"]))))

(defn- activate-event-button-during-rout-phase [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        movement-factors-text? (-> r (sc/select [:#movement-factors]) sc/text string/blank? not)
        final-modifier-text? (-> r (sc/select [:#final-modifier]) sc/selection)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{action-option-text} ["Rout" "Low Crawl"]) (and description-text? movement-factors-text?)
                      (= "Interdiction" action-option-text) (and description-text? white-die-selected? colored-die-selected? final-modifier-text? result-text?)
                      (= "Elimination" action-option-text) description-text?
                      (= "Other" action-option-text) (or description-text? result-text?)
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-rout-phase-activations [e]
  (let [r (sc/to-root e)]
    (sc/config! (-> r (sc/select [:#description])) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (activate-white-die-during-rout-phase e)
    (activate-colored-die-during-rout-phase e)
    (activate-movement-factors-during-rout-phase e)
    (sc/config! (sc/select r [:#movement-points]) :enabled? false)
    (sc/config! (sc/select r [:#firepower]) :enabled? false)
    (sc/hide! (sc/select r [:#split-final-modifier-panel]))
    (sc/show! (sc/select r [:#final-modifier-panel]))
    (activate-final-modifier-during-rout-phase e)
    (sc/config! (sc/select r [:#attacker-final-modifier]) :enabled? false)
    (sc/config! (sc/select r [:#defender-final-modifier]) :enabled? false)
    (activate-result-during-rout-phase e)
    (activate-event-button-during-rout-phase e)))

(defn- activate-result-during-advance-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#result])
                :enabled? ((complement not-any?) #{action-option-text} ["Advance" "Other"]))))

(defn- activate-event-button-during-advance-phase [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{action-option-text} ["Advance" "Transfer SW"]) description-text?
                      (= "Other" action-option-text) (or description-text? result-text?)
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-advance-phase-activations [e]
  (let [r (sc/to-root e)]
    (sc/config! (-> r (sc/select [:#description])) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (disable-die-radio-buttons (sc/select r [:#white-die-panel]))
    (disable-die-radio-buttons (sc/select r [:#colored-die-panel]))
    (sc/config! (sc/select r [:#movement-factors]) :enabled? false)
    (sc/config! (sc/select r [:#movement-points]) :enabled? false)
    (sc/config! (sc/select r [:#firepower]) :enabled? false)
    (sc/hide! (sc/select r [:#split-final-modifier-panel]))
    (sc/show! (sc/select r [:#final-modifier-panel]))
    (sc/config! (sc/select r [:#final-modifier]) :enabled? false)
    (sc/config! (sc/select r [:#attacker-final-modifier]) :enabled? false)
    (sc/config! (sc/select r [:#defender-final-modifier]) :enabled? false)
    (activate-result-during-advance-phase e)
    (activate-event-button-during-advance-phase e)))

(defn- activate-white-die-during-close-combat-phase [e]
  (activate-die-panel e ["Ambush" "ATTACKER CC" "DEFENDER CC" "Leader Creation" "Other"] :#white-die-panel))

(defn- activate-colored-die-during-close-combat-phase [e]
  (activate-die-panel e ["Ambush" "ATTACKER CC" "DEFENDER CC" "Other"] :#colored-die-panel))

(defn- activate-final-modifiers-during-close-combat-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#final-modifier])
                :enabled? ((complement not-any?) #{action-option-text} ["ATTACKER CC" "DEFENDER CC" "Leader Creation" "Other"]))
    (sc/config! (sc/select r [:#attacker-final-modifier]) :enabled? (= "Ambush" action-option-text))
    (sc/config! (sc/select r [:#defender-final-modifier]) :enabled? (= "Ambush" action-option-text))))

(defn- activate-result-during-close-combat-phase [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#result])
                :enabled? ((complement not-any?) #{action-option-text} ["Ambush" "ATTACKER CC" "DEFENDER CC" "Leader Creation" "Other"]))))

(defn- activate-event-button-for-remaining-actions-during-close-combat-phase [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        final-modifier-text? (-> r (sc/select [:#final-modifier]) sc/selection)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{action-option-text} ["Ambush" "ATTACKER CC" "DEFENDER CC"])
                      (and description-text? white-die-selected? colored-die-selected? final-modifier-text? result-text?)
                      (= "Leader Creation" action-option-text)
                      (and description-text? white-die-selected? final-modifier-text? result-text?)
                      (= "Other" action-option-text) (or description-text? result-text?)
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-close-combat-phase-activations-for-remaining-actions [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#description]) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (activate-white-die-during-close-combat-phase e)
    (activate-colored-die-during-close-combat-phase e)
    (sc/config! (sc/select r [:#movement-factors]) :enabled? false)
    (sc/config! (sc/select r [:#movement-points]) :enabled? false)
    (sc/config! (sc/select r [:#firepower]) :enabled? false)
    (sc/config! (sc/select r [:#split-final-modifier-panel]) :visible? (= "Ambush" action-option-text))
    (sc/config! (sc/select r [:#final-modifier-panel]) :visible? (not= "Ambush" action-option-text))
    (activate-final-modifiers-during-close-combat-phase e)
    (activate-result-during-close-combat-phase e)
    (activate-event-button-for-remaining-actions-during-close-combat-phase e)))

(defn- perform-close-combat-phase-activations [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (cond (= "Random Selection" action-option-text) (perform-activations-for-random-selection e)
          :else (perform-close-combat-phase-activations-for-remaining-actions e))))

(defn- perform-activations [e]
  (let [r (sc/to-root e)
        phase-text (-> r (sc/select [:#phase]) sc/text)
        {:keys [activation-fn]} (get phase-map phase-text)]
    (activation-fn e)))

(defn- reset-event-panel [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description number-dice white-die-panel colored-die-panel
                movement-factors movement-points firepower
                final-modifier attacker-final-modifier defender-final-modifier result]} (sc/group-by-id event-panel)
        select-die-panel (fn [v] (sc/select r v))]
    (sc/selection! action-options 0)
    (sc/text! description "")
    (sc/selection! number-dice 2)
    (clear-die-rolls (map (comp select-die-panel vector :panel-id-select) random-dice-info))
    (clear-die-rolls [white-die-panel colored-die-panel])
    (sc/text! movement-factors "")
    (sc/text! movement-points "")
    (sc/text! firepower "")
    (sc/selection! final-modifier 0)
    (sc/selection! attacker-final-modifier 0)
    (sc/selection! defender-final-modifier 0)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- switch-sub-phase-panel-visibility [e]
  (let [r (sc/to-root e)
        sub-phase-panel (sc/select r [:#sub-phase-panel])
        phase-text (-> r (sc/select [:#phase]) sc/text)]
    (if (some #{phase-text} ["Rally" "Rout"])
      (sc/show! sub-phase-panel)
      (sc/hide! sub-phase-panel))
    e))

(defn- update-sub-phase-panel [e next-sub-phase advance-sub-phase-button-enabled? rewind-sub-phase-button-enabled?]
  (let [r (sc/to-root e)
        sub-phase-panel (sc/select r [:#sub-phase-panel])
        {:keys [sub-phase advance-sub-phase-button rewind-sub-phase-button]} (sc/group-by-id sub-phase-panel)]
    (sc/text! sub-phase next-sub-phase)
    (sc/config! advance-sub-phase-button :enabled? advance-sub-phase-button-enabled?)
    (sc/config! rewind-sub-phase-button :enabled? rewind-sub-phase-button-enabled?)
    e))

(defn- establish-action-options [e options]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options]} (sc/group-by-id event-panel)]
    (sc/config! action-options :model options)
    (sc/config! action-options :enabled? true)
    e))

(defn- transition-to-rally-phase-reinforcements [e & rest]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel "Reinforcements" true false)
      (establish-action-options ["Place Reinforcements" "Other"])
      reset-event-panel))

(defn- transition-to-rally-phase-recovery [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Recover SW" "Other"])
      reset-event-panel))

(defn- transition-to-rally-phase-repair [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Repair SW" "Other"])
      reset-event-panel))

(defn- transition-to-rally-phase-transfer [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Transfer SW" "Other"])
      reset-event-panel))

(defn- transition-to-rally-phase-self-rally [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Self Rally" "Wound Resolution" "Leader Creation" "Other"])
      reset-event-panel))

(defn- transition-to-rally-phase-unit-rally [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Unit Rally" "Wound Resolution" "Other"])
      reset-event-panel))

(defn- transition-to-prep-fire [e & rest]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel "" false false)
      (establish-action-options ["Prep Fire" "Morale Check" "Pin Task Check" "Wound Resolution" "Random Selection" "Other"])
      reset-event-panel))

(defn- transition-to-movement [e]
  (-> e
      (establish-action-options ["Movement" "Assault Movement" "CX" "Place Smoke" "Recover SW"
                               "Defensive First Fire" "Subsequent First Fire" "Final Protective Fire" "Residual FP"
                               "Morale Check" "Pin Task Check" "Wound Resolution" "Random Selection" "Other"])
      reset-event-panel))

(defn- transition-to-defensive-fire [e]
  (-> e
      (establish-action-options ["Final Fire" "Morale Check" "Pin Task Check" "Wound Resolution" "Random Selection" "Other"])
      reset-event-panel))

(defn- transition-to-advancing-fire [e]
  (-> e
      (establish-action-options ["Advancing Fire" "Morale Check" "Pin Task Check" "Wound Resolution" "Random Selection" "Other"])
      reset-event-panel))

(defn- transition-to-attacker-rout [e & rest]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel "ATTACKER" true false)
      (establish-action-options ["Rout" "Low Crawl" "Interdiction" "Elimination" "Other"])
      reset-event-panel))

(defn- transition-to-defender-rout [e next-rout-phase]
  (-> e
      (update-sub-phase-panel next-rout-phase true true)
      (establish-action-options ["Rout" "Low Crawl" "Interdiction" "Elimination" "Other"])
      reset-event-panel))

(defn- transition-to-advance [e & rest]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel "" false false)
      (establish-action-options ["Advance" "Transfer SW" "Other"])
      reset-event-panel))

(defn- transition-to-close-combat [e]
  (-> e
      (establish-action-options ["Ambush" "ATTACKER CC" "DEFENDER CC" "Leader Creation" "Random Selection" "Other"])
      reset-event-panel))

(defn- add-event [e]
  (let [r (sc/to-root e)
        sub-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text (-> r (sc/select [:#description]) sc/text)
        white-die-panel (-> r (sc/select [:#white-die-panel]))
        colored-die-panel (-> r (sc/select [:#colored-die-panel]))
        random-die-panels (map (fn [pid] (sc/select r [pid])) (map :panel-id-select random-dice-info))
        die-rolls (gather-die-rolls (concat (list white-die-panel colored-die-panel) random-die-panels))
        final-modifier-selection (-> r (sc/select [:#final-modifier]) sc/selection)
        attacker-final-modifier-selection (when (= "Ambush" action-option-text)
                                            (-> r (sc/select [:#attacker-final-modifier]) sc/selection))
        defender-final-modifier-selection (when (= "Ambush" action-option-text)
                                            (-> r (sc/select [:#defender-final-modifier]) sc/selection))
        result-text (-> r (sc/select [:#result]) sc/text)]
    (swap! the-game update-in [:game-zip-loc] append-event sub-phase-text action-option-text description-text die-rolls
           final-modifier-selection attacker-final-modifier-selection defender-final-modifier-selection result-text)
    (reset-event-panel e)))

(defn- perform-file-new [e]
  (let [r (sc/to-root e)])
  e)

(defn- perform-file-open [e])

(defn- perform-file-save [e f]
  (let [r (sc/to-root e)
        sw (proxy [asl_recorder.swing_worker] []
             (doInBackground []
               (do
                 (with-open [w (clojure.java.io/writer f)]
                   (-> the-game
                       deref
                       :game-zip-loc
                       zip/root
                       (xml/indent w)))))
             (process [_])
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

(defn- choose-file [e]
  (sch/choose-file (sc/to-root e) :filters [["ASL files" ["asl"]]] :all-files? false :success-fn (fn [_ f] (perform-file-save e f))))

(defn- perform-file-exit [e])

(defn -main
  [& args]
  (sc/native!)
  (sc/invoke-later
    (let [white-die-info (create-die-radio-buttons white)
          colored-die-info (create-die-radio-buttons colored)
          random-dice-colors (map (fn [{:keys [color] :as rdi}] (assoc rdi :die-radio-buttons (create-die-radio-buttons color))) random-dice-info)
          random-dice-labels (map (fn [{:keys [label-id label-text visible?]}] (sc/label :id label-id :text label-text :visible? visible?)) random-dice-colors)
          random-dice-panels (map (fn [{:keys [color visible? panel-id], {:keys [radio-buttons button-group]} :die-radio-buttons}]
                                    (sm/mig-panel :id panel-id
                                                  :visible? visible?
                                                  :constraints ["fill, insets 0"]
                                                  :items radio-buttons
                                                  :user-data {:color color :button-group button-group}))
                                  random-dice-colors)]
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

                        (sc/label :id :sub-phase-label :text "Rally Sub-Phase:")
                        (sc/label :id :sub-phase)
                        (sm/mig-panel :id :sub-phase-line :constraints ["" "nogrid" ""] :items [[sub-phase-label] [sub-phase "grow"]])
                        (sc/button :id :advance-sub-phase-button :text "Next Rally Sub-Phase")
                        (sc/button :id :rewind-sub-phase-button :text "Previous Rally Sub-Phase")
                        (sm/mig-panel :id :sub-phase-panel :constraints [] :items [[sub-phase-line "span 2, align center, wrap"]
                                                                                   [advance-sub-phase-button] [rewind-sub-phase-button]])

                        (sm/mig-panel :id :game-position-panel :constraints [] :items [[turn-panel] [attacker-panel, "wrap"]
                                                                                       [phase-panel] [sub-phase-panel]])

                        (sc/spinner :id :final-modifier :model (sc/spinner-model 0 :from -10 :to 10 :by 1))
                        (sm/mig-panel :id :final-modifier-panel :constraints ["insets 0" "[|fill, grow]" ""] :items [["Final Modifier:" "align right"] [final-modifier "span, wrap"]])

                        (sc/spinner :id :attacker-final-modifier :model (sc/spinner-model 0 :from -10 :to 10 :by 1))
                        (sc/spinner :id :defender-final-modifier :model (sc/spinner-model 0 :from -10 :to 10 :by 1))
                        (sm/mig-panel :id :split-final-modifier-panel :visible? false :constraints ["insets 0" "[|fill, grow||fill, grow]" ""] :items [["ATTACKER Final Modifier:" "align right"] [attacker-final-modifier] ["DEFENDER Final Modifier:" "align right"] [defender-final-modifier "wrap"]])

                        (sc/label :id :movement-factors-label :text "MF:" :halign :right)
                        (sc/text :id :movement-factors :text "")
                        (sc/label :id :movement-points-label :text "MP:" :halign :right)
                        (sc/text :id :movement-points :text "")
                        (sc/label :id :firepower-label :text "FP:" :halign :right)
                        (sc/text :id :firepower :text "")
                        (sm/mig-panel :id :white-die-panel
                                      :constraints ["fill, insets 0"]
                                      :items (:radio-buttons white-die-info)
                                      :user-data {:color white :button-group (:button-group white-die-info)})
                        (sm/mig-panel :id :colored-die-panel
                                      :constraints ["fill, insets 0"]
                                      :items (:radio-buttons colored-die-info)
                                      :user-data {:color colored :button-group (:button-group colored-die-info)})
                        (sm/mig-panel :id :standard-event-panel :constraints ["" "[|fill, grow]" ""] :items [[movement-factors-label "grow"] [movement-factors] [movement-points-label] [movement-points] [firepower-label] [firepower "wrap"]
                                                                                                             ["White Die:" "align right"] [white-die-panel "span, wrap"]
                                                                                                             ["Colored Die:" "align right"] [colored-die-panel "span, wrap"]
                                                                                                             [final-modifier-panel "hidemode 3, span, wrap, grow"]
                                                                                                             [split-final-modifier-panel "hidemode 3, span, wrap, grow"]])

                        (sc/spinner :id :number-dice :model (sc/spinner-model 2 :from 2 :to 10 :by 1))
                        (sm/mig-panel :id :random-event-panel :visible? false :constraints ["" "[|fill, grow]" ""] :items (into [["Number dice:" "align right"] [number-dice "span, wrap"]]
                                                                                                                                (mapcat (fn [rdl rdp] (vector (vector rdl "align right, hidemode 3")
                                                                                                                                                              (vector rdp "span, wrap, hidemode 3")))
                                                                                                                                        random-dice-labels random-dice-panels)))

                        (sc/combobox :id :action-options)
                        (sc/text :id :description :text "")
                        (sc/text :id :result)
                        (sc/button :id :add-event-button :text "Add event")
                        (sm/mig-panel :id :event-panel :constraints ["" "[|fill, grow]" ""] :items [["Action:" "align right"] [action-options "span, wrap"]
                                                                                                    ["Description:" "align right"] [description "span, wrap"]
                                                                                                    [standard-event-panel "hidemode 3, span, wrap, grow"]
                                                                                                    [random-event-panel "hidemode 3, span, wrap, grow"]
                                                                                                    ["Result:" "align right"] [result "span, wrap"]
                                                                                                    [add-event-button "span, align center"]])

                        (sc/button :id :ok :text "OK" :enabled? false)
                        (sc/menu-item :id :file-new :listen [:action perform-file-new] :text "New..." :mnemonic \N)
                        (sc/menu-item :id :file-open :listen [:action perform-file-open] :text "Open..." :mnemonic \O)
                        (sc/menu-item :id :file-save :listen [:action choose-file] :text "Save..." :mnemonic \S)
                        (sc/menu-item :id :file-exit :listen [:action perform-file-exit] :text "Exit" :mnemonic \E)]
                       (let [ok-fn (fn [e] (process-the-game e))]
                         (sc/listen advance-turn-button :action advance-turn)
                         (sc/listen advance-attacker-button :action advance-attacker)
                         (sc/listen advance-phase-button :action advance-phase)
                         (sc/listen advance-sub-phase-button :action advance-sub-phase)
                         (sc/listen action-options :selection perform-activations)
                         (sc/listen description :document (fn [_] (perform-activations description)))
                         (sc/listen number-dice :change perform-activations)
                         (add-change-listener-to-die-radio-buttons white-die-panel perform-activations)
                         (add-change-listener-to-die-radio-buttons colored-die-panel perform-activations)
                         (dorun (map #(add-change-listener-to-die-radio-buttons % perform-activations)
                                     (map (fn [v] (sc/select random-event-panel v))
                                          (map vector (map :panel-id-select random-dice-info)))))
                         ;                         (sc/listen final-modifier :change perform-activations)
                         (sc/listen movement-factors :document (fn [_] (perform-activations movement-factors)))
                         (sc/listen movement-points :document (fn [_] (perform-activations movement-points)))
                         (sc/listen firepower :document (fn [_] (perform-activations firepower)))
                         (sc/listen result :document (fn [_] (perform-activations result)))
                         (sc/listen add-event-button :action add-event)
                         (sc/listen ok :action ok-fn)

                         (-> (sc/frame :title "ASL Recorder",
                                       :content (sm/mig-panel :constraints [] :items [[game-position-panel "wrap"]
                                                                                      [event-panel "growx, wrap"]
                                                                                      [ok "align center"]]),
                                       :menubar (sc/menubar :items [(sc/menu :text "File" :items [file-new file-open file-save file-exit])])
                                       :on-close :exit)
                             (transition-to-rally-phase-reinforcements "Reinforcements")
                             sc/pack!
                             sc/show!))))))
