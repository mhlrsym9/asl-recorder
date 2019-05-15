(ns asl-recorder.new-wizard
  (:require [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd] [layout :as layout] selector [table :as t]]
            [clojure.string :as str]
            [seesaw [table :as table] [mig :as sm]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (javax.swing JSpinner)
           (javax.swing.table AbstractTableModel)))

; TODO: Choose scenario from list... pre-fills basic-parameters and map-configuration.
; TODO: Different size maps (HASL, DASL, etc.)
; TODO: Move to second page iff all fields on first page set.
; TODO: Move to second page iff first move name != second move name.
; TODO: Move to third page iff all fields on second page set.
; TODO: Make sure each id is unique.
; TODO: Make sure all positions are valid ASL format and belong to one of the maps on the board.
; TODO: UI issue moving to last page without pushing Add to OOB for last counter, then Back to that page, click Add, empty row added.

(def ^{:private true} basic-parameters-page
  (proxy [WizardPage seesaw.selector.Tag] ["Game Parameters" "Basic parameters about the scenario to be recorded."]
    (tag_name [] (.getSimpleName WizardPage))
    (updateSettings [settings]
      (proxy-super updateSettings settings)
      (.remove settings "Spinner.formattedTextField"))))

(defn- basic-parameters-panel []
  (let [p (sc/abstract-panel
            basic-parameters-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Name: " (sc/text :id :name :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["First Move: " (sc/combobox :id :first-move :model ["German" "Russian" "American"])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Other Side: " (sc/combobox :id :second-move :model ["German" "Russian" "American"])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Number Turns: " (sc/spinner :id :number-turns
                                                                                :model (sc/spinner-model 1 :from 1 :to 15 :by 1))])
                      [:fill-v 5]
                      (sc/horizontal-panel :items [(sc/checkbox :id :extra-move? :text "First Side has extra move?")])
                      :fill-v]})
        preferred-size-fn #(-> p (sc/select %) (sc/config :preferred-size))
        maximum-size-fn #(-> p (sc/select %) (sc/config! :maximum-size (preferred-size-fn %)))]
    (dorun (map #(maximum-size-fn [%]) [:#name :#first-move :#second-move :#number-turns]))
    p))

; CJWizard has a bug where it reaches into the JSpinner, extracts the low-level
; formattedTextField for the number model, and stores its value. The problem is
; there is no way to change the name of this component, therefore if you use
; a number-based JSpinner in a subsequent page, those JSpinners will be set to
; the value of this one. CJWizard's rendering system sets the value of any component
; it can find on each page if the component name matches. To avoid that, I need
; to override the updateSettings implementation so I can remove that entry. Seesaw's id
; system will allow me to extract the entry without using WizardSettings.

(def ^{:private true} map-configuration-page
  (proxy [WizardPage seesaw.selector.Tag] ["Map Configuration" "Layout of the game area."]
    (tag_name [] (.getSimpleName WizardPage))
    (updateSettings [settings]
      (proxy-super updateSettings settings)
      (.remove settings "Spinner.formattedTextField"))))

(defn- create-orientation-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [:fill-h
     "Board Orientation:"
     (sc/radio :id :vertical-orientation :class :orientation-class :text "Vertical" :group the-button-group :selected? true)
     (sc/radio :id :horizontal-orientation :class :orientation-class :text "Horizontal" :group the-button-group)
     :fill-h]))

(defn- create-direction-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [:fill-h
     (sm/mig-panel :id :direction-panel :constraints ["wrap 2" "" ""] :items [["North is:"] [(sc/radio :id :up-direction :class :direction-class :text "above the first row of boards." :group the-button-group :selected? true)]
                                                                                                [(sc/radio :id :right-direction :class :direction-class :text "to the right of the rightmost column of boards." :group the-button-group) "skip"]
                                                                                                [(sc/radio :id :down-direction :class :direction-class :text "below the last row of boards." :group the-button-group) "skip"]
                                                                                                [(sc/radio :id :left-direction :class :direction-class :text "to the left of the leftmost column of boards." :group the-button-group) "skip"]])
     :fill-h]))


(def ^{:private true} map-configuration-table-columns
  [{:key :row :text "Row" :class java.lang.Integer}
   {:key :column :text "Column" :class java.lang.Integer}
   {:key :present? :text "Present?" :class java.lang.Boolean}
   {:key :board-id :text "Board ID" :class java.lang.String}
   {:key :upper-left? :text "Upper Left?" :class java.lang.Boolean}
   {:key :upper-right? :text "Upper Right?" :class java.lang.Boolean}
   {:key :lower-left? :text "Lower Left?" :class java.lang.Boolean}
   {:key :lower-right? :text "Lower Right?" :class java.lang.Boolean}])

(defn- map-configuration-panel []
  (let [t-model-data (atom [[0 0 true "" false false false false]])
        t-model (proxy [AbstractTableModel] []
                  (getRowCount [] (count @t-model-data))
                  (getColumnCount [] (count map-configuration-table-columns))
                  (getValueAt [r c] (nth (nth @t-model-data r) c))
                  (isCellEditable [r c] (or (= c 2)
                                            (and (> c 2)
                                                 (nth (nth @t-model-data r) 2))))
                  (getColumnName [c] (:text (nth map-configuration-table-columns c)))
                  (getColumnClass [c] (:class (nth map-configuration-table-columns c)))
                  (setValueAt [o r c]
                    (let [row-data (apply assoc (nth @t-model-data r) c o
                                          (when (and (= c 2) (not o))
                                            (proxy-super fireTableRowsUpdated r r)
                                            '(3 "" 4 false 5 false 6 false)))]
                      (swap! t-model-data assoc r row-data))))
        t (sc/table :id :map-table :model t-model)
        update-table-fn (fn [e]
                          (let [r (sc/to-root e)
                                number-rows (-> r (sc/select [:#number-rows]) sc/selection)
                                number-columns (-> r (sc/select [:#number-columns]) sc/selection)
                                new-vec (vec (for [r (range number-rows)
                                                   c (range number-columns)]
                                               [r c true "" false false false false]))]
                            (reset! t-model-data new-vec)
                            (.fireTableDataChanged t-model)))
        p (sc/abstract-panel
            map-configuration-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Number of rows: "
                                                   (sc/spinner :id :number-rows
                                                               :model (sc/spinner-model 1 :from 1 :to 5 :by 1)
                                                               :listen [:change update-table-fn])
                                                   [:fill-h 10]
                                                   "Number of columns: "
                                                   (sc/spinner :id :number-columns
                                                               :model (sc/spinner-model 1 :from 1 :to 5 :by 1)
                                                               :listen [:change update-table-fn])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-orientation-radio-buttons))
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-direction-radio-buttons))
                      [:fill-v 5]
                      (sc/scrollable t)]})]
    p))

(defn- second-move-initial-setup-page [name]
  (let [title (str name " Initial Setup")
        tip (str "Initial positions for all on-board " name " units.")]
    (proxy [WizardPage seesaw.selector.Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn- add-to-oob-enabled? [e]
  (let [r (sc/to-root e)
        s1 (-> r (sc/select [:#unique-id]) sc/text)
        s2 (-> r (sc/select [:#position]) sc/text)]
    (and (-> s1 str/blank? not)
         (-> s2 str/blank? not))))

(defn- configure-add-to-oob-enabled-state [e]
  (let [r (sc/to-root e)
        add-to-oob (sc/select r [:#add-to-oob])]
    (sc/config! add-to-oob :enabled? (add-to-oob-enabled? e))))

(defn- add-to-oob-action [oob-pound-id e]
  (let [r (sc/to-root e)
        unique-id (sc/select r [:#unique-id])
        position (sc/select r [:#position])
        remove-last-from-oob (sc/select r [:#remove-last-from-oob])
        t (sc/select r [oob-pound-id])]
    (table/insert-at! t (table/row-count t) {:unique-id (sc/text unique-id) :position (sc/text position)})
    (sc/config! remove-last-from-oob :enabled? true)
    (sc/text! unique-id "")
    (sc/text! position "")))

(defn- remove-from-oob-action [oob-pound-id e]
  (let [r (sc/to-root e)
        remove-last-from-oob (sc/select r [:#remove-last-from-oob])
        t (sc/select r [oob-pound-id])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-setup-layout [oob-id oob-pound-id]
  (let [t (sc/table :id oob-id :model [:columns [:unique-id :position]])
        unique-id (sc/text :id :unique-id :listen [:document (fn [_] (configure-add-to-oob-enabled-state t))])
        position (sc/text :id :position :listen [:document (fn [_] (configure-add-to-oob-enabled-state t))])]
    {:border 5
     :items  [(sc/horizontal-panel :items ["Unique ID: " unique-id [:fill-h 10] "Position: " position])
              [:fill-v 5]
              (sc/horizontal-panel :items [(sc/button :id :add-to-oob :text "Add to OoB" :listen [:action (partial add-to-oob-action oob-pound-id)])
                                           :fill-h
                                           (sc/button :id :remove-last-from-oob :text "Remove last from OoB" :enabled? false :listen [:action (partial remove-from-oob-action oob-pound-id)])])
              [:fill-v 5]
              (sc/scrollable t)]}))

(defn- second-move-initial-setup-panel [name]
  (let [p (sc/abstract-panel
            (second-move-initial-setup-page name)
            (layout/box-layout :vertical)
            (initial-setup-layout :oob-side2 :#oob-side2))]
    p))

(defn- first-move-initial-setup-page [name]
  (let [title (str name " Initial Setup")
        tip (str "Initial positions for all on-board " name " units.")]
    (proxy [WizardPage seesaw.selector.Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage))
      (rendering [path settings]
        (proxy-super rendering path settings)
        (doto this
          (.setFinishEnabled true)
          (.setNextEnabled false))))))

(defn- first-move-initial-setup-panel [name]
  (let [p (sc/abstract-panel
            (first-move-initial-setup-page name)
            (layout/box-layout :vertical)
            (initial-setup-layout :oob-side1 :#oob-side1))]
    p))

(def new-wizard-page-factory
  (reify PageFactory
    (isTransient [_ _ _] false)
    (createPage [_ wizard-pages _]
      (let [c (.size wizard-pages)]
        (cond (= 0 c) (basic-parameters-panel)
              (= 1 c) (map-configuration-panel)
              (= 2 c) (second-move-initial-setup-panel (-> (.get wizard-pages 0) (sc/select [:#second-move]) sc/selection))
              (= 3 c) (first-move-initial-setup-panel (-> (.get wizard-pages 0) (sc/select [:#first-move]) sc/selection)))))))

(defn extract-orientation [d]
  (let [r (sc/to-root d)]
    (if (sc/selection (sc/select r [:#vertical-orientation])) "Vertical" "Horizontal")))

(defn extract-direction [d]
  (let [r (sc/to-root d)]
    (cond (sc/selection (sc/select r [:#up-direction])) "up"
          (sc/selection (sc/select r [:#right-direction])) "right"
          (sc/selection (sc/select r [:#down-direction])) "down"
          :else "left")))

(defn extract-map-rows-from-wizard [d]
  (let [r (sc/to-root d)
        t (sc/select r [:#map-table])
        number-columns (-> r (sc/select [:#number-columns]) sc/selection)
        tm (sc/config t :model)
        c (.getColumnCount tm)
        the-maps (for [i (range (.getRowCount tm))]
                   (map #(.getValueAt tm i %) (range 2 c)))]
    (partition number-columns the-maps)))

(defn- extract-initial-setup [d oob-id]
  (let [r (sc/to-root d)
        t (sc/select r [oob-id])]
    (vec (for [i (range (table/row-count t))]
           (vec (vals (table/value-at t i)))))))

(defn extract-side2-initial-setup [d]
  (extract-initial-setup d :#oob-side2))

(defn extract-side1-initial-setup [d]
  (extract-initial-setup d :#oob-side1))