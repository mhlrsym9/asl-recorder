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
         transition-to-rout
         transition-to-advance
         transition-to-close-combat)

(def rally-phase-map {"Reinforcements"      {:next-rally-phase "ATTACKER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "ATTACKER Recovery"   {:next-rally-phase "DEFENDER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "DEFENDER Recovery"   {:next-rally-phase "ATTACKER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "ATTACKER Repair"     {:next-rally-phase "DEFENDER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "DEFENDER Repair"     {:next-rally-phase "ATTACKER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "ATTACKER Transfer"   {:next-rally-phase "DEFENDER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "DEFENDER Transfer"   {:next-rally-phase "ATTACKER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "ATTACKER Self-Rally" {:next-rally-phase "DEFENDER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "DEFENDER Self-Rally" {:next-rally-phase "ATTACKER Unit Rally" :transition-fn #'transition-to-rally-phase-unit-rally}
                      "ATTACKER Unit Rally" {:next-rally-phase "DEFENDER Unit Rally" :transition-fn #'transition-to-rally-phase-unit-rally}
                      "DEFENDER Unit Rally" {:next-rally-phase nil :transition-fn #'transition-to-prep-fire}})

(def phase-map {"Rally"          {:next-phase "Prep Fire" :transition-fn #'transition-to-prep-fire}
                "Prep Fire"      {:next-phase "Movement" :transition-fn #'transition-to-movement}
                "Movement"       {:next-phase "Defensive Fire" :transition-fn #'transition-to-defensive-fire}
                "Defensive Fire" {:next-phase "Advancing Fire" :transition-fn #'transition-to-advancing-fire}
                "Advancing Fire" {:next-phase "Rout" :transition-fn #'transition-to-rout}
                "Rout"           {:next-phase "Advance" :transition-fn #'transition-to-advance}
                "Advance"        {:next-phase "Close Combat" :transition-fn #'transition-to-close-combat}
                "Close Combat"   {:next-phase "Rally" :transition-fn #'transition-to-rally-phase-reinforcements}})

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

(defn append-event [loc the-type the-action-option the-description the-die-rolls the-final-modifier the-result]
  (let [n (zip/node loc)
        c (conj (vec (:content n)) (xml/element :event {:type the-type}
                                                (xml/element :action {:option the-action-option}
                                                             (filter identity (list (xml/element :description {} the-description)
                                                                                    (when the-die-rolls
                                                                                      (xml/element :die-rolls {} (map #(xml/element :die-roll
                                                                                                                                    {:color (:color %)}
                                                                                                                                    (:die-roll %))
                                                                                                                      the-die-rolls)))
                                                                                    (when the-final-modifier
                                                                                      (xml/element :final-modifier {} the-final-modifier))
                                                                                    (when the-result
                                                                                      (xml/element :result {} the-result)))))))]
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

(defn advance-game-rally-phase [loc current-rally-phase]
  (let [{:keys [next-rally-phase] :as next-rally-phase-info} (get rally-phase-map current-rally-phase)
        current-phase (get-game-phase loc)
        next-phase? (nil? next-rally-phase)
        next-phase (if next-phase?
                     (get phase-map current-phase)
                     current-phase)
        new-loc (cond next-phase? (append-phase loc next-phase)
                      :else loc)]
    {:next-phase next-phase :next-rally-phase-info next-rally-phase-info :new-loc new-loc}))

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

(defn- advance-sub-phase [e]
  (let [r (sc/to-root e)
        sub-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        turn (sc/select r [:#turn])
        attacker (sc/select r [:#attacker])
        phase (sc/select r [:#phase])
        {:keys [next-phase new-loc] {:keys [transition-fn next-rally-phase]} :next-rally-phase-info} (advance-game-rally-phase @game-zip-loc sub-phase-text)]
    (reset! game-zip-loc new-loc)
    (update-time turn (sc/text turn) attacker (sc/text attacker) phase next-phase)
    (transition-fn e next-rally-phase)))

(defn- advance-phase [e]
  (let [r (sc/to-root e)
        turn (sc/select r [:#turn])
        attacker (sc/select r [:#attacker])
        phase (sc/select r [:#phase])
        {:keys [next-turn next-attacker new-loc], {:keys [next-phase transition-fn]} :next-phase-info} (advance-game-phase @game-zip-loc)]
    (reset! game-zip-loc new-loc)
    (update-time turn next-turn attacker next-attacker phase next-phase)
    (transition-fn e)
    e))

(defn- advance-attacker [turn attacker phase]
  (fn [_] (let [{:keys [next-turn next-attacker next-phase new-loc]} (advance-game-attacker @game-zip-loc)]
            (reset! game-zip-loc new-loc)
            (update-time turn next-turn attacker next-attacker phase next-phase))))

(defn- advance-turn [turn attacker phase]
  (fn [_] (let [{:keys [next-turn next-attacker next-phase new-loc]} (advance-game-turn @game-zip-loc)]
            (reset! game-zip-loc new-loc)
            (update-time turn next-turn attacker next-attacker phase next-phase))))

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
  (filter identity (map gather-die-roll die-panels)))

(defn- clear-die-roll [die-panel]
  (-> die-panel sc/user-data :button-group (sc/selection! false)))

(defn clear-die-rolls [die-panels]
  (dorun (map clear-die-roll die-panels)))

(defn- selected-die-radio-button? [die-panel]
  (seq (filter identity (map sc/selection (sc/select die-panel [:JRadioButton])))))

(defn- add-change-listener-to-die-radio-buttons [die-panel f]
  (dorun (map #(sc/listen % :selection f) (sc/select die-panel [:JRadioButton]))))

(defn- activate-white-die [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        enable? (cond (some #{rally-phase-text} (list "Reinforcements" "ATTACKER Transfer" "DEFENDER Transfer")) false
                      :else true)]
    (update-die-radio-buttons-enabled-state (sc/select r [:#white-die-panel]) enable?)))

(defn- activate-colored-die [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        enable? (cond (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                      (some #{"Self Rally" "Unit Rally"} (list action-option-text))
                      :else false)]
    (update-die-radio-buttons-enabled-state (sc/select r [:#colored-die-panel]) enable?)))

(defn- enable-on-description-text-and-white-die-selected [e]
  (let [r (sc/to-root e)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)]
    (and description-text? white-die-selected?)))

(defn- activate-final-modifier [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        final-modifier (sc/select r [:#final-modifier])
        enabled? (cond (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery" "ATTACKER Repair" "DEFENDER Repair"))
                       (enable-on-description-text-and-white-die-selected e)
                       (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                       true
                      :else false)]
    (sc/config! final-modifier :enabled? enabled?)))

(defn- activate-result [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        result (sc/select r [:#result])
        enabled? (cond (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery" "ATTACKER Repair" "DEFENDER Repair"))
                       (enable-on-description-text-and-white-die-selected e)
                       (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                       true
                       :else false)]
    (sc/config! result :enabled? enabled?)))

(defn- activate-add-rally-event-button [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        rally-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (some #{rally-phase-text} (list "Reinforcements" "ATTACKER Transfer" "DEFENDER Transfer"))
                      description-text?
                      (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery" "ATTACKER Repair" "DEFENDER Repair"))
                      (and description-text? (or (not white-die-selected?)
                                                 (and white-die-selected? result-text?)))
                      (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                      (and description-text? white-die-selected? result-text? (or (not= action-option-text "Self Rally")
                                                                                  colored-die-selected?))
                      :else false)]
    (sc/config! add-event-button :enabled? enable?)))

(defn- activate-add-prep-fire-event-button-for-random-selection [e]
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

(defn- activate-add-prep-fire-event-button-for-remaining-actions [e]
  (let [r (sc/to-root e)
        add-event-button (sc/select r [:#add-event-button])
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)
        colored-die-selected? (-> r (sc/select [:#colored-die-panel]) selected-die-radio-button?)
        result-text? (-> r (sc/select [:#result]) sc/text string/blank? not)
        enable? (cond (= "Wound Resolution" action-option-text) (and description-text? white-die-selected? result-text?)
                      :else (and description-text? white-die-selected? colored-die-selected? result-text?))]
    (sc/config! add-event-button :enabled? enable?)))

(defn- perform-rally-phase-activations [e]
  (let [r (sc/to-root e)
        fields (vec (map #(sc/select r (vector %)) [:#movement-factors :#movement-points :#firepower]))]
    (sc/config! (-> r (sc/select [:#description])) :enabled? true)
    (activate-white-die e)
    (activate-colored-die e)
    (activate-final-modifier e)
    (sc/config! fields :enabled? false)
    (activate-result e)
    (activate-add-rally-event-button e)))

(defn- update-random-dice [e id enabled? rdi]
  (let [r (sc/to-root e)]
    (->> rdi
         (map #(comp (partial sc/select r) vector id))
         (map #(sc/config! % :enabled? enabled?)))))

(defn- perform-prep-fire-phase-activations-for-random-selection [e]
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
    (activate-add-prep-fire-event-button-for-random-selection e)))

(defn- perform-prep-fire-phase-activations-for-remaining-actions [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (sc/config! (sc/select r [:#description]) :enabled? true)
    (sc/hide! (sc/select r [:#random-event-panel]))
    (sc/show! (sc/select r [:#standard-event-panel]))
    (enable-die-radio-buttons (sc/select r [:#white-die-panel]))
    (update-die-radio-buttons-enabled-state (sc/select r [:#colored-die-panel]) (not= "Wound Resolution" action-option-text))
    (sc/config! (sc/select r [:#movement-factors]) :enabled? false)
    (sc/config! (sc/select r [:#movement-points]) :enabled? false)
    (sc/config! (sc/select r [:#firepower]) :enabled? (= "Prep Fire" action-option-text))
    (sc/config! (sc/select r [:#final-modifier]) :enabled? true)
    (sc/config! (sc/select r [:#result]) :enabled? true)
    (activate-add-prep-fire-event-button-for-remaining-actions e)))

(defn- perform-prep-fire-phase-activations [e]
  (let [r (sc/to-root e)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)]
    (if (= "Random Selection" action-option-text)
      (perform-prep-fire-phase-activations-for-random-selection e)
      (perform-prep-fire-phase-activations-for-remaining-actions e))))

(defn- perform-activations [e]
  (let [r (sc/to-root e)
        phase-text (-> r (sc/select [:#phase]) sc/text)]
    (cond (= "Rally" phase-text) (perform-rally-phase-activations e)
          (= "Prep Fire" phase-text) (perform-prep-fire-phase-activations e))))

(defn- reset-event-panel [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description number-dice white-die-panel colored-die-panel movement-factors movement-points firepower final-modifier result]} (sc/group-by-id event-panel)
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
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- switch-sub-phase-panel-visibility [e]
  (let [r (sc/to-root e)
        sub-phase-panel (sc/select r [:#sub-phase-panel])
        phase-text (-> r (sc/select [:#phase]) sc/text)]
    (if (= "Rally" phase-text)
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

(defn- establish-action-options [e options enabled?]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options]} (sc/group-by-id event-panel)]
    (sc/config! action-options :model options)
    (sc/config! action-options :enabled? enabled?)
    e))

(defn- transition-to-rally-phase-reinforcements [e next-rally-phase]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel next-rally-phase true false)
      (establish-action-options ["Place Reinforcements"] false)
      reset-event-panel))

(defn- transition-to-rally-phase-recovery [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Recover SW"] false)
      reset-event-panel))

(defn- transition-to-rally-phase-repair [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Repair SW"] false)
      reset-event-panel))

(defn- transition-to-rally-phase-transfer [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Transfer SW"] false)
      reset-event-panel))

(defn- transition-to-rally-phase-self-rally [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Self Rally" "Wound Resolution" "Leader Creation"] true)
      reset-event-panel))

(defn- transition-to-rally-phase-unit-rally [e next-rally-phase]
  (-> e
      (update-sub-phase-panel next-rally-phase true true)
      (establish-action-options ["Unit Rally" "Wound Resolution"] true)
      reset-event-panel))

(defn- transition-to-prep-fire [e & rest]
  (-> e
      switch-sub-phase-panel-visibility
      (update-sub-phase-panel "" false false)
      (establish-action-options ["Prep Fire" "Morale Check" "Pin Task Check" "Wound Resolution" "Random Selection" "Other"] true)
      reset-event-panel))

(defn- add-event [e]
  (let [r (sc/to-root e)
        sub-phase-text (-> r (sc/select [:#sub-phase]) sc/text)
        action-option-text (-> r (sc/select [:#action-options]) sc/selection)
        description (sc/select r [:#description])
        description-text (sc/text description)
        white-die-panel (-> r (sc/select [:#white-die-panel]))
        colored-die-panel (-> r (sc/select [:#colored-die-panel]))
        die-rolls (gather-die-rolls (list white-die-panel colored-die-panel))
        final-modifier (sc/select r [:#final-modifier])
        final-modifier-selection (sc/selection final-modifier)
        result (sc/select r [:#result])
        result-text (sc/text result)]
    (swap! game-zip-loc append-event sub-phase-text action-option-text description-text die-rolls final-modifier-selection result-text)
    (reset-event-panel e)))

(defn -main
  [& args]
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

                        (sm/mig-panel :id :white-die-panel
                                      :constraints ["fill, insets 0"]
                                      :items (:radio-buttons white-die-info)
                                      :user-data {:color white :button-group (:button-group white-die-info)})
                        (sm/mig-panel :id :colored-die-panel
                                      :constraints ["fill, insets 0"]
                                      :items (:radio-buttons colored-die-info)
                                      :user-data {:color colored :button-group (:button-group colored-die-info)})
                        (sc/label :id :movement-factors-label :text "MF:" :halign :right)
                        (sc/text :id :movement-factors :text "")
                        (sc/label :id :movement-points-label :text "MP:" :halign :right)
                        (sc/text :id :movement-points :text "")
                        (sc/label :id :firepower-label :text "FP:" :halign :right)
                        (sc/text :id :firepower :text "")
                        (sc/spinner :id :final-modifier :model (sc/spinner-model 0 :from -10 :to 10 :by 1))
                        (sm/mig-panel :id :standard-event-panel :constraints ["" "[|fill, grow]" ""] :items [["White Die:" "align right"] [white-die-panel "span, wrap"]
                                                                                                             ["Colored Die:" "align right"] [colored-die-panel "span, wrap"]
                                                                                                             [movement-factors-label "grow"] [movement-factors] [movement-points-label] [movement-points] [firepower-label] [firepower "wrap"]
                                                                                                             ["Final Modifier:" "align right"] [final-modifier "span, wrap"]])

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

                        (sc/button :id :ok :text "OK" :enabled? false)]
                       (let [ok-fn (fn [e] (process-the-game e))]
                         (sc/listen advance-turn-button :action (advance-turn turn attacker phase))
                         (sc/listen advance-attacker-button :action (advance-attacker turn attacker phase))
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
                         (sc/listen result :document (fn [_] (perform-activations result)))
                         (sc/listen add-event-button :action add-event)
                         (sc/listen ok :action ok-fn)

                         (-> (sc/frame :title "ASL Recorder",
                                       :content (sm/mig-panel :constraints [] :items [[game-position-panel "wrap"]
                                                                                      [event-panel "growx, wrap"]
                                                                                      [ok "align center"]]),
                                       :on-close :exit)
                             (transition-to-rally-phase-reinforcements "Reinforcements")
                             sc/pack!
                             sc/show!))))))
