(ns asl-recorder.new-wizard-pages.side-configuration
  (:require [asl-recorder.new-wizard-pages.utilities :as u]
            [seesaw [core :as sc] [layout :as layout] [table :as table]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (javax.swing.table AbstractTableModel DefaultTableCellRenderer)
           (seesaw.selector Tag)))

(def ^{:private true} side-data-row [false "" "" false])

(def ^{:private true} side-configuration-table-columns
  [{:key :move-order :text "Move Order" :class Integer}
   {:key :is-nationality? :text "Is Side Single Nationality?" :class Boolean}
   {:key :side-name :text "Side Name" :class String}
   {:key :number-initial-setup-groups :text "Initial Setup Groups" :class Integer}
   {:key :number-reinforcement-groups :text "Reinforcement Groups" :class Integer}
   {:key :extra-move? :text "Has Extra Move?" :class Boolean}])

(defn- create-side-name-radio-buttons []
  (let [nationalities (u/extract-nationalities)
        nationality-combobox (sc/combobox :id :nationality-side-name-cb :model nationalities)
        free-form-text (sc/text :id :free-form-side-name :columns 100)
        the-button-group (sc/button-group)]
    (sc/vertical-panel :items [:fill-v
                               (sc/horizontal-panel :items [(sc/radio :id :use-nationality-name-for-side-name :class :orientation-class :text "Use this nation as the side name:" :group the-button-group :selected? true)
                                                            [:fill-h 10]
                                                            nationality-combobox])
                               [:fill-v 5]
                               (sc/horizontal-panel :items [(sc/radio :id :use-free-form-side-name :class :orientation-class :text "Use the following name as the side name:" :group the-button-group)
                                                            [:fill-h 10]
                                                            free-form-text])
                               :fill-v])))

(defn- add-to-side-action [t]
  (let [r (sc/to-root t)
        move-order-spinner (sc/select r [:#move-order-spinner])
        move-order (sc/selection move-order-spinner)
        use-nationality-name-for-side-name (sc/select r [:#use-nationality-name-for-side-name])
        is-nationality? (sc/selection use-nationality-name-for-side-name)
        nationality-side-name-cb (sc/select r [:#nationality-side-name-cb])
        nationality-side-name (sc/selection nationality-side-name-cb)
        free-form-text (sc/select r [:#free-form-side-name])
        free-form-side-name (sc/text free-form-text)
        side-name (if is-nationality? nationality-side-name free-form-side-name)
        initial-setup-groups-spinner (sc/select r [:#initial-setup-groups-spinner])
        number-initial-setup-groups (sc/selection initial-setup-groups-spinner)
        reinforcement-groups-spinner (sc/select r [:#reinforcement-groups-spinner])
        number-reinforcement-groups (sc/selection reinforcement-groups-spinner)
        has-extra-move-checkbox (sc/select r [:#has-extra-move-checkbox])
        has-extra-move? (sc/selection has-extra-move-checkbox)
        remove-last-from-side-button (sc/select r [:#remove-last-from-side-button])]
    (table/insert-at! t (.getRowCount t) {:move-order                  move-order
                                          :is-nationality?             is-nationality?
                                          :side-name                   side-name
                                          :number-initial-setup-groups number-initial-setup-groups
                                          :number-reinforcement-groups number-reinforcement-groups
                                          :has-extra-move?             has-extra-move?})
    (sc/config! remove-last-from-side-button :enabled? true)
    (sc/selection! move-order-spinner 1)
    (sc/selection! use-nationality-name-for-side-name true)
    (sc/selection! nationality-side-name-cb 0)
    (sc/text! free-form-text "")
    (sc/selection! initial-setup-groups-spinner 1)
    (sc/selection! reinforcement-groups-spinner 0)
    (sc/selection! has-extra-move-checkbox false)))

(defn- remove-last-from-side-action [t]
  (let [r (sc/to-root t)
        remove-last-from-oob (sc/select r [:#remove-last-from-side-button])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- side-configuration-layout []
  (let [t-model-data (atom [])
        t-model (proxy [AbstractTableModel] []
                  (getRowCount [] (count @t-model-data))
                  (getColumnCount [] (count side-configuration-table-columns))
                  (getValueAt [r c] (nth (nth @t-model-data r) c))
                  (isCellEditable [_ _] false)
                  (getColumnName [c] (:text (nth side-configuration-table-columns c)))
                  (getColumnClass [c] (:class (nth side-configuration-table-columns c)))
                  (setValueAt [o r c]
                    (let [row-data (assoc (nth @t-model-data r) c o)]
                      (swap! t-model-data assoc r row-data))))
        t (sc/table :id :side-table :model t-model)
        add-to-side-button (sc/button :id :add-to-side-button :text "Add this side"
                                      :listen [:action (fn [_] (add-to-side-action t))])
        remove-last-from-side-button (sc/button :id :remove-last-from-side-button
                                                :text "Remove last side added"
                                                :enabled? false
                                                :listen [:action (fn [_] (remove-last-from-side-action t))])
        layout {:border 5
                :items  [:fill-v
                         (sc/horizontal-panel :items ["This side moves: " (sc/spinner :id :move-order-spinner
                                                                                      :model (sc/spinner-model 1 :from 1 :to 3 :by 1))])
                         [:fill-v 5]
                         (create-side-name-radio-buttons)
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Number of Initial Setup Groups: " (sc/spinner :id :initial-setup-groups-spinner
                                                                                                     :model (sc/spinner-model 1 :from 0 :to 15 :by 1))])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Number of Reinforcement Groups: "
                                                      (sc/spinner :id :reinforcement-groups-spinner
                                                                  :model (sc/spinner-model 0 :from 0 :to 15 :by 1))])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [(sc/checkbox :id :has-extra-move-checkbox :text "Has extra move?")])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-side-button :fill-h remove-last-from-side-button])
                         [:fill-v 5]
                         (sc/scrollable t)
                         :fill-v]}]
    layout))

(defn- side-configuration-page []
  (proxy [WizardPage Tag] ["Side Configuration" "Enter basic information about each side in the game."]
    (tag_name [] (.getSimpleName WizardPage))
    (updateSettings [settings]
      (proxy-super updateSettings settings)
      (.remove settings "Spinner.formattedTextField"))))

(defn side-configuration-panel []
  (let [layout (side-configuration-layout)
        p (sc/abstract-panel
            (side-configuration-page)
            (layout/box-layout :vertical)
            layout)]
    p))
