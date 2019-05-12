(ns asl-recorder.new-wizard
  (:require [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd] [layout :as layout] selector [table :as t]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (javax.swing.table AbstractTableModel)))

(def ^{:private true} basic-parameters-page
  (proxy [WizardPage seesaw.selector.Tag] ["Game Parameters" "Basic parameters about the scenario to be recorded."]
    (tag_name [] (.getSimpleName WizardPage))))

(def ^{:private true} basic-parameters-panel
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
                      (sc/horizontal-panel :items ["Number Turns: " (sc/text :id :number-turns :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items [(sc/checkbox :id :extra-move? :text "First Side has extra move?")])
                      :fill-v]})
        preferred-size-fn #(-> p (sc/select %) (sc/config :preferred-size))
        maximum-size-fn #(-> p (sc/select %) (sc/config! :maximum-size (preferred-size-fn %)))]
    (dorun (map #(maximum-size-fn [%]) [:#name :#first-move :#second-move :#number-turns]))
    p))

(def ^{:private true} map-configuration-page
  (proxy [WizardPage seesaw.selector.Tag] ["Map Configuration" "Layout of the game area."]
    (tag_name [] (.getSimpleName WizardPage))))

(defn- create-orientation-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [(sc/radio :id :vertical-orientation :class :orientation-class :text "Vertical" :group the-button-group)
     (sc/radio :id :horizontal-orientation :class :orientation-class :text "Horizontal" :group the-button-group)]))

(def ^{:private true} map-configuration-table-columns
  [{:key :row :text "Row" :class java.lang.Integer}
   {:key :column :text "Column" :class java.lang.Integer}
   {:key :present? :text "Present?" :class java.lang.Boolean}
   {:key :board-id :text "Board ID" :class java.lang.String}
   {:key :upper-left? :text "Upper Left?" :class java.lang.Boolean}
   {:key :upper-right? :text "Upper Right?" :class java.lang.Boolean}
   {:key :lower-left? :text "Lower Left?" :class java.lang.Boolean}
   {:key :lower-right? :text "Lower Right?" :class java.lang.Boolean}])

(def ^{:private true} map-configuration-panel
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
                      (sc/horizontal-panel :items ["Number of rows: " (sc/spinner :id :number-rows :model (sc/spinner-model 1 :from 1 :to 5 :by 1) :listen [:change update-table-fn])
                                                   "Number of columns: " (sc/spinner :id :number-columns :model (sc/spinner-model 1 :from 1 :to 5 :by 1) :listen [:change update-table-fn])])
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-orientation-radio-buttons))
                      [:fill-v 5]
                      (sc/scrollable t)
                      :fill-v]})]
    p))

(def ^{:private true} defender-initial-setup-page
  (proxy [WizardPage seesaw.selector.Tag] ["DEFENDER Initial Setup" "Initial positions for all on-board DEFENDER units."]
    (tag_name [] (.getSimpleName WizardPage))))

(def ^{:private true} defender-initial-setup-panel
  (let [p (sc/abstract-panel
            defender-initial-setup-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["DEFENDER"])
                      :fill-v]})]
    p))

(def ^{:private true} attacker-initial-setup-page
  (proxy [WizardPage seesaw.selector.Tag] ["ATTACKER Initial Setup" "Initial positions for all on-board ATTACKER units."]
    (tag_name [] (.getSimpleName WizardPage))
    (rendering [path settings]
      (proxy-super rendering path settings)
      (doto this
        (.setFinishEnabled true)
        (.setNextEnabled false)))))

(def ^{:private true} attacker-initial-setup-panel
  (let [p (sc/abstract-panel
            attacker-initial-setup-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["ATTACKER"])
                      :fill-v]})]
    p))

(def ^{:private true} new-wizard-panels [basic-parameters-panel map-configuration-panel defender-initial-setup-panel attacker-initial-setup-panel])

(def new-wizard-page-factory
  (reify PageFactory
    (isTransient [_ _ _] false)
    (createPage [_ wizard-pages _]
      (get new-wizard-panels (.size wizard-pages)))))
