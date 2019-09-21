(ns asl-recorder.new-wizard-pages.map-configuration
  (:require [seesaw [core :as sc] [layout :as layout] [mig :as sm]])
  (:import [com.github.cjwizard WizardPage]
           (seesaw.selector Tag)
           (javax.swing.table AbstractTableModel)))

(def ^{:private true} map-configuration-page
  (proxy [WizardPage Tag] ["Map Configuration" "Layout of the game area."]
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
  [{:key :row :text "Row" :class Integer}
   {:key :column :text "Column" :class Integer}
   {:key :present? :text "Present?" :class Boolean}
   {:key :board-id :text "Board ID" :class String}
   {:key :upper-left? :text "Upper Left?" :class Boolean}
   {:key :upper-right? :text "Upper Right?" :class Boolean}
   {:key :lower-left? :text "Lower Left?" :class Boolean}
   {:key :lower-right? :text "Lower Right?" :class Boolean}])

(defn map-configuration-panel []
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

(defn- extract-orientation [p]
  (if (sc/selection (sc/select p [:#vertical-orientation])) "Vertical" "Horizontal"))

(defn- extract-direction [p]
  (cond (sc/selection (sc/select p [:#up-direction])) "up"
        (sc/selection (sc/select p [:#right-direction])) "right"
        (sc/selection (sc/select p [:#down-direction])) "down"
        :else "left"))

(defn- extract-map-rows-from-wizard [p]
  (let [t (sc/select p [:#map-table])
        number-columns (-> p (sc/select [:#number-columns]) sc/selection)
        tm (sc/config t :model)
        c (.getColumnCount tm)
        the-maps (for [i (range (.getRowCount tm))]
                   (map #(.getValueAt tm i %) (range 2 c)))]
    (partition number-columns the-maps)))

(defn extract-map-configuration [p]
  {:orientation (extract-orientation p)
   :direction (extract-direction p)
   :map-rows (extract-map-rows-from-wizard p)})


