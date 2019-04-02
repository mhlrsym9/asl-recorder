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

(declare transition-to-rally-phase-recovery
         transition-to-rally-phase-repair
         transition-to-rally-phase-transfer
         transition-to-rally-phase-self-rally
         transition-to-rally-phase-attacker-unit-rally
         transition-to-rally-phase-defender-unit-rally)

(def rally-phase-map {"Reinforcements"      {:next-rally-phase "ATTACKER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "ATTACKER Recovery"   {:next-rally-phase "DEFENDER Recovery" :transition-fn #'transition-to-rally-phase-recovery}
                      "DEFENDER Recovery"   {:next-rally-phase "ATTACKER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "ATTACKER Repair"     {:next-rally-phase "DEFENDER Repair" :transition-fn #'transition-to-rally-phase-repair}
                      "DEFENDER Repair"     {:next-rally-phase "ATTACKER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "ATTACKER Transfer"   {:next-rally-phase "DEFENDER Transfer" :transition-fn #'transition-to-rally-phase-transfer}
                      "DEFENDER Transfer"   {:next-rally-phase "ATTACKER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "ATTACKER Self-Rally" {:next-rally-phase "DEFENDER Self-Rally" :transition-fn #'transition-to-rally-phase-self-rally}
                      "DEFENDER Self-Rally" {:next-rally-phase "ATTACKER Unit Rally" :transition-fn #'transition-to-rally-phase-attacker-unit-rally}
                      "ATTACKER Unit Rally" {:next-rally-phase "DEFENDER Unit Rally" :transition-fn #'transition-to-rally-phase-defender-unit-rally}
                      "DEFENDER Unit Rally" {:next-rally-phase "Reinforcements" :transition-fn nil}})

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

(def white "white")
(def colored "colored")

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
        next-phase? (= next-rally-phase "Reinforcements")
        next-phase (if next-phase?
                     (get phase-map current-phase)
                     current-phase)
        new-loc (cond next-phase? (append-phase loc next-phase)
                      :else loc)]
    {:next-phase next-phase :next-rally-phase-info next-rally-phase-info :new-loc new-loc}))

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

(defn- advance-rally-phase [e]
  (let [r (sc/to-root e)
        rally-phase (sc/select r [:#rally-phase])
        turn (sc/select r [:#turn])
        attacker (sc/select r [:#attacker])
        phase (sc/select r [:#phase])
        {:keys [next-phase new-loc] {:keys [transition-fn next-rally-phase]} :next-rally-phase-info} (advance-game-rally-phase @game-zip-loc (sc/text rally-phase))]
    (reset! game-zip-loc new-loc)
    (update-time turn (sc/text turn) attacker (sc/text attacker) phase next-phase)
    (transition-fn e next-rally-phase)))

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

(defn- create-die-info [prefix]
  (map #(hash-map :id (keyword (str prefix "-" %1)) :text (str %2) :user-data %2)
       ["one" "two" "three" "four" "five" "six"]
       (range 1 7)))

(defn create-die-radio-buttons [prefix]
  (let [the-info (create-die-info prefix)
        the-class (keyword (str prefix "-" "die-class"))
        the-button-group (sc/button-group)]
    (for [{:keys [id text user-data]} the-info]
      (vector (sc/radio :id id :class the-class :text text :group the-button-group :user-data user-data)))))

(defn- update-die-radio-buttons-enabled-state [die-panel enabled?]
  (let [the-info (create-die-info (sc/config die-panel :user-data))
        the-radio-buttons (map #(get (sc/group-by-id die-panel) %) (map :id the-info))]
    (dorun (map (fn [i] (sc/config! i :enabled? enabled?)) the-radio-buttons))))

(defn- disable-die-radio-buttons [die-panel]
  (update-die-radio-buttons-enabled-state die-panel false))

(defn- enable-die-radio-buttons [die-panel]
  (update-die-radio-buttons-enabled-state die-panel true))

(defn- find-selected-die-radio-button [die-panel]
  (first (filter identity (map #(when (sc/selection %) %) (sc/select die-panel [:JRadioButton])))))

(defn- gather-die-roll [die-panel]
  (when-let [selected (find-selected-die-radio-button die-panel)]
    (hash-map :die-roll (sc/config selected :user-data) :color (sc/config die-panel :user-data))))

(defn gather-die-rolls [die-panels]
  (filter identity (map gather-die-roll die-panels)))

(defn- clear-die-roll [die-panel]
  (dorun (map (fn [d] (sc/selection! d :false)) (sc/select die-panel [:JRadioButton]))))

(defn clear-die-rolls [die-panels]
  (dorun (map clear-die-roll die-panels)))

(defn- selected-die-radio-button? [die-panel]
  (seq (filter identity (map sc/selection (sc/select die-panel [:JRadioButton])))))

(defn- add-change-listener-to-die-radio-buttons [die-panel f]
  (dorun (map #(sc/listen % :selection f) (sc/select die-panel [:JRadioButton]))))

(defn- activate-add-rally-event-button [e]
  (let [r (sc/to-root e)
        add-rally-event-button (sc/select r [:#add-rally-event-button])
        rally-phase-text (-> r (sc/select [:#rally-phase]) sc/text)
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
                      (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally"))
                      (and description-text? white-die-selected? result-text? (or (not= action-option-text "Self Rally")
                                                                                  colored-die-selected?))
                      :else false)]
    (sc/config! add-rally-event-button :enabled? enable?)))

(defn- enable-on-description-text-and-white-die-selected [e]
  (let [r (sc/to-root e)
        description-text? (-> r (sc/select [:#description]) sc/text string/blank? not)
        white-die-selected? (-> r (sc/select [:#white-die-panel]) selected-die-radio-button?)]
    (and description-text? white-die-selected?)))

(defn- activate-final-modifier [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#rally-phase]) sc/text)
        final-modifier (sc/select r [:#final-modifier])
        enabled? (cond (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery" "ATTACKER Repair" "DEFENDER Repair"))
                       (enable-on-description-text-and-white-die-selected e)
                       (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                       true
                      :else false)]
    (sc/config! final-modifier :enabled? enabled?)))

(defn- activate-result [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#rally-phase]) sc/text)
        result (sc/select r [:#result])
        enabled? (cond (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery" "ATTACKER Repair" "DEFENDER Repair"))
                       (enable-on-description-text-and-white-die-selected e)
                       (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally" "ATTACKER Unit Rally" "DEFENDER Unit Rally"))
                       true
                       :else false)]
    (sc/config! result :enabled? enabled?)))

(defn- perform-activations [e]
  (activate-add-rally-event-button e)
  (activate-final-modifier e)
  (activate-result e))

(defn- reset-rally-phase-reinforcements [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description white-die-panel colored-die-panel final-modifier result]} (sc/group-by-id event-panel)]
    (sc/selection! action-options 0)
    (sc/config! description :enabled? true)
    (sc/text! description "")
    (clear-die-rolls [white-die-panel colored-die-panel])
    (disable-die-radio-buttons white-die-panel)
    (disable-die-radio-buttons colored-die-panel)
    (sc/selection! final-modifier 0)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- reset-rally-phase-recovery [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description white-die-panel colored-die-panel final-modifier result]}  (sc/group-by-id event-panel)]
    (sc/selection! action-options 0)
    (sc/config! description :enabled? true)
    (sc/text! description "")
    (enable-die-radio-buttons white-die-panel)
    (clear-die-roll white-die-panel)
    (disable-die-radio-buttons colored-die-panel)
    (clear-die-roll colored-die-panel)
    (sc/selection! final-modifier 0)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- reset-rally-phase-repair [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description white-die-panel colored-die-panel final-modifier result]}  (sc/group-by-id event-panel)]
    (sc/selection! action-options 0)
    (sc/config! description :enabled? true)
    (sc/text! description "")
    (enable-die-radio-buttons white-die-panel)
    (clear-die-roll white-die-panel)
    (disable-die-radio-buttons colored-die-panel)
    (clear-die-roll colored-die-panel)
    (sc/selection! final-modifier 0)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- reset-rally-phase-transfer [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description white-die-panel colored-die-panel final-modifier result]} (sc/group-by-id event-panel)]
    (sc/selection! action-options 0)
    (sc/config! description :enabled? true)
    (sc/text! description "")
    (clear-die-rolls [white-die-panel colored-die-panel])
    (disable-die-radio-buttons white-die-panel)
    (disable-die-radio-buttons colored-die-panel)
    (sc/selection! final-modifier 0)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- reset-rally-phase-self-rally [e]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options description white-die-panel colored-die-panel final-modifier result]} (sc/group-by-id event-panel)]
    (sc/selection! action-options 0)
    (sc/config! description :enabled? true)
    (sc/text! description "")
    (clear-die-rolls [white-die-panel colored-die-panel])
    (enable-die-radio-buttons white-die-panel)
    (enable-die-radio-buttons colored-die-panel)
    (sc/config! final-modifier :enabled? true)
    (sc/selection! final-modifier 0)
    (sc/config! result :enabled? true)
    (sc/text! result "")
    (perform-activations e)
    e))

(defn- update-rally-panel [e next-rally-phase advance-rally-phase-button-enabled? rewind-rally-phase-button-enabled?]
  (let [r (sc/to-root e)
        rally-panel (sc/select r [:#rally-panel])
        {:keys [rally-phase advance-rally-phase-button rewind-rally-phase-button]} (sc/group-by-id rally-panel)]
    (sc/text! rally-phase next-rally-phase)
    (sc/config! advance-rally-phase-button :enabled? advance-rally-phase-button-enabled?)
    (sc/config! rewind-rally-phase-button :enabled? rewind-rally-phase-button-enabled?)))

(defn- establish-action-options [e options enabled?]
  (let [r (sc/to-root e)
        event-panel (sc/select r [:#event-panel])
        {:keys [action-options]} (sc/group-by-id event-panel)]
    (sc/config! action-options :model options)
    (sc/config! action-options :enabled? enabled?)))

(defn- transition-to-rally-phase-reinforcements [e next-rally-phase]
  (update-rally-panel e next-rally-phase true false)
  (establish-action-options e ["Place Reinforcements"] false)
  (reset-rally-phase-reinforcements e))

(defn- transition-to-rally-phase-recovery [e next-rally-phase]
  (update-rally-panel e next-rally-phase true true)
  (establish-action-options e ["Recover SW"] false)
  (reset-rally-phase-recovery e))

(defn- transition-to-rally-phase-repair [e next-rally-phase]
  (update-rally-panel e next-rally-phase true true)
  (establish-action-options e ["Repair SW"] false)
  (reset-rally-phase-repair e))

(defn- transition-to-rally-phase-transfer [e next-rally-phase]
  (update-rally-panel e next-rally-phase true true)
  (establish-action-options e ["Transfer SW"] false)
  (reset-rally-phase-transfer e))

(defn- transition-to-rally-phase-self-rally [e next-rally-phase]
  (update-rally-panel e next-rally-phase true true)
  (establish-action-options e ["Self Rally" "Wound Resolution"] true)
  (reset-rally-phase-self-rally e))

(defn- add-rally-event [e]
  (let [r (sc/to-root e)
        rally-phase-text (-> r (sc/select [:#rally-phase]) sc/text)
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
    (swap! game-zip-loc append-event rally-phase-text action-option-text description-text die-rolls final-modifier-selection result-text)
    (cond (= "Reinforcements" rally-phase-text) (reset-rally-phase-reinforcements e)
          (some #{rally-phase-text} (list "ATTACKER Recovery" "DEFENDER Recovery")) (reset-rally-phase-recovery e)
          (some #{rally-phase-text} (list "ATTACKER Repair" "DEFENDER Repair")) (reset-rally-phase-repair e)
          (some #{rally-phase-text} (list "ATTACKER Transfer" "DEFENDER Transfer")) (reset-rally-phase-transfer e)
          (some #{rally-phase-text} (list "ATTACKER Self-Rally" "DEFENDER Self-Rally")) (reset-rally-phase-self-rally e))))

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

                      (sc/label :id :rally-phase)
                      (sm/mig-panel :id :rally-phase-line :constraints ["" "nogrid" ""] :items [["Rally Phase:"] [rally-phase "grow"]])
                      (sc/button :id :advance-rally-phase-button :text "Next Rally Phase")
                      (sc/button :id :rewind-rally-phase-button :text "Previous Rally Phrase")
                      (sm/mig-panel :id :rally-panel :constraints [] :items [[rally-phase-line "span 2, align center, wrap"]
                                                                             [advance-rally-phase-button] [rewind-rally-phase-button]])

                      (sc/combobox :id :action-options)
                      (sc/text :id :description :text "")
                      (sm/mig-panel :id :white-die-panel :constraints ["fill, insets 0"] :items (create-die-radio-buttons white) :user-data white)
                      (sm/mig-panel :id :colored-die-panel :constraints ["fill, insets 0"] :items (create-die-radio-buttons colored) :user-data colored)
                      (sc/spinner :id :final-modifier :model (sc/spinner-model 0 :from -10 :to 10 :by 1))
                      (sc/text :id :result)
                      (sc/button :id :add-rally-event-button :text "Add event")
                      (sm/mig-panel :id :event-panel :constraints ["" "[|fill, grow]" ""] :items [["Action:" "align right"] [action-options "wrap"]
                                                                                                  ["Description:" "align right"] [description "wrap"]
                                                                                                  ["White Die:" "align right"] [white-die-panel "span, wrap"]
                                                                                                  ["Colored Die:" "align right"] [colored-die-panel "span, wrap"]
                                                                                                  ["Final Modifier:" "align right"] [final-modifier "wrap"]
                                                                                                  ["Result:" "align right"] [result "wrap"]
                                                                                                  [add-rally-event-button "span, align center"]])

                      (sm/mig-panel :id :game-event-panel :constraints ["" "[|fill, grow]" ""] :items [[rally-panel] [event-panel]])

                      (sc/button :id :ok :text "OK" :enabled? false)]
                     (let [ok-fn (fn [e] (process-the-game e))]
                       (sc/listen advance-turn-button :action (advance-turn turn attacker phase))
                       (sc/listen advance-attacker-button :action (advance-attacker turn attacker phase))
                       (sc/listen advance-phase-button :action (advance-phase turn attacker phase))
                       (sc/listen advance-rally-phase-button :action advance-rally-phase)
                       (sc/listen description :document (fn [_] (perform-activations description)))
                       (add-change-listener-to-die-radio-buttons white-die-panel perform-activations)
                       (add-change-listener-to-die-radio-buttons colored-die-panel perform-activations)
                       (sc/listen final-modifier :change activate-add-rally-event-button)
                       (sc/listen result :document (fn [_] (activate-add-rally-event-button result)))
                       (sc/listen add-rally-event-button :action add-rally-event)
                       (sc/listen ok :action ok-fn)

                       (-> (sc/frame :title "ASL Recorder",
                                     :content (sm/mig-panel :constraints [] :items [[game-position-panel "wrap"]
                                                                                    [game-event-panel "growx, wrap"]
                                                                                    [ok "align center"]]),
                                     :on-close :exit)
                           (transition-to-rally-phase-reinforcements "Reinforcements")
                           sc/pack!
                           sc/show!)))))
