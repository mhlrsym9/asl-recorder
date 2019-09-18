(ns asl-recorder.new-wizard
  (:require [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd] [layout :as layout] selector [table :as t]]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [seesaw [table :as table] [mig :as sm]])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (javax.swing JTable DefaultCellEditor)
           (javax.swing.table TableColumnModel TableColumn)
           (javax.swing.table AbstractTableModel DefaultTableCellRenderer)
           (java.io File)))

; TODO: Choose scenario from list... pre-fills basic-parameters and map-configuration.
; TODO: Different size maps (HASL, DASL, etc.)
; TODO: Move to second page iff all fields on first page set.
; TODO: Move to second page iff first move name != second move name.
; TODO: Move to third page iff all fields on second page set.
; TODO: Make sure each id is unique.
; TODO: Make sure all positions are valid ASL format and belong to one of the maps on the board.

(def ^{:private true} basic-parameters-page
  (proxy [WizardPage seesaw.selector.Tag] ["Game Parameters" "Basic parameters about the scenario to be recorded."]
    (tag_name [] (.getSimpleName WizardPage))
    (updateSettings [settings]
      (proxy-super updateSettings settings)
      (.remove settings "Spinner.formattedTextField"))))

; The wizard has problems destroying pages if you back up in that the standard seesaw
; select method thinks there are multiple elements for each created seesaw id, and thus
; seems to return the first created (select w/ id returns only one element). Therefore,
; as each panel is created, the code adds a reference to the actual current panel into
; this stateful map (argh!). When the data is extracted, the code uses these references
; instead of the dialog root to extract the data in the last instance of each panel.
(def ^{:private true} panel-map (atom {:basic-parameters nil :optional-rules nil :map-configuration nil :oob [] :setup []}))

(defn- extract-nationalities []
  (with-open [r (-> "nationalities.xml" io/resource io/reader)]
    (let [z (-> r xml/parse zip/xml-zip)]
      (map (zip-xml/attr :name) (zip-xml/xml-> z :Nationality)))))

(defn- extract-counters [nationality unit-type]
  (let [file-path (str (str/lower-case nationality) File/separator
                       (str/lower-case unit-type) ".xml")]
    (with-open [r (-> file-path
                      io/resource
                      io/reader)]
      (let [z (-> r xml/parse zip/xml-zip)]
        (loop [loc z
               units []]
          (if (zip/end? loc)
            units
            (let [node (zip/node loc)]
              (if (= :unit-name (:tag node))
                (recur (zip/next loc) (conj units (zip-xml/text loc)))
                (recur (zip/next loc) units)))))))))

(defn- create-rule-set-radio-buttons []
  (let [the-button-group (sc/button-group)]
    [:fill-h
     "Rule Set:"
     (sc/radio :id :asl :class :orientation-class :text "ASL" :group the-button-group :selected? true)
     (sc/radio :id :asl-sk :class :orientation-class :text "ASL Starter Kit" :group the-button-group)
     :fill-h]))

(def ^{:private true} side-configuration-table-columns
  [{:key :move-order :text "Move Order" :class java.lang.Integer}
   {:key :is-nationality? :text "Is Side Single Nationality?" :class java.lang.Boolean}
   {:key :nationality :text "Nationality" :class java.lang.String}
   {:key :coalition :text "Coalition Name" :class java.lang.String}
   {:key :extra-move? :text "Has Extra Move?" :class java.lang.Boolean}])

(def side-data-row [false "" "" false])

(defn- basic-parameters-panel []
  (let [nationalities (extract-nationalities)
        nationality-combobox (sc/combobox :id :nationality :model nationalities)
        create-side-rows (fn [idx]
                           (into [] (concat (vector idx) side-data-row)))
        t-model-data (atom (vec (map create-side-rows (range 1 3))))
        t-model (proxy [AbstractTableModel] []
                  (getRowCount [] (count @t-model-data))
                  (getColumnCount [] (count side-configuration-table-columns))
                  (getValueAt [r c] (nth (nth @t-model-data r) c))
                  (isCellEditable [r c] (let [is-nationality? (nth (nth @t-model-data r) 1)]
                                          (cond (= c 0) false
                                                (= c 1) true
                                                (= c 2) is-nationality?
                                                (= c 3) (not is-nationality?)
                                                (= c 4) true)))
                  (getColumnName [c] (:text (nth side-configuration-table-columns c)))
                  (getColumnClass [c] (:class (nth side-configuration-table-columns c)))
                  (setValueAt [o r c]
                    (let [row-data (assoc (nth @t-model-data r) c o)]
                      (swap! t-model-data assoc r row-data))))
        t (sc/table :id :side-table :model t-model)
        update-table-fn (fn [e]
                          (let [r (sc/to-root e)
                                number-sides (-> r (sc/select [:#number-sides]) sc/selection)
                                new-vec (vec (map create-side-rows (range 1 (inc number-sides))))]
                            (reset! t-model-data new-vec)
                            (.fireTableDataChanged t-model)))
        p (sc/abstract-panel
            basic-parameters-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items ["Name: " (sc/text :id :name :columns 100)])
                      [:fill-v 5]
                      (sc/horizontal-panel :items (create-rule-set-radio-buttons))
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Number Turns: " (sc/spinner :id :number-turns
                                                                                :model (sc/spinner-model 1 :from 1 :to 15 :by 1))])
                      [:fill-v 5]
                      (sc/horizontal-panel :items ["Number of Sides: "
                                                   (sc/spinner :id :number-sides
                                                               :model (sc/spinner-model 2 :from 2 :to 4 :by 1)
                                                               :listen [:change update-table-fn])])
                      [:fill-v 5]
                      (sc/scrollable t)
                      :fill-v]})
        preferred-size-fn #(-> p (sc/select %) (sc/config :preferred-size))
        maximum-size-fn #(-> p (sc/select %) (sc/config! :maximum-size (preferred-size-fn %)))]
    (dorun (map #(maximum-size-fn [%]) [:#name :#number-turns :#side-table]))
    (swap! panel-map assoc :basic-parameters p)
    (let [column-model (.getColumnModel t)
          column (.getColumn column-model 2)
          default-cell-renderer (DefaultTableCellRenderer.)
          default-cell-editor (DefaultCellEditor. nationality-combobox)]
      (.setToolTipText default-cell-renderer "Click for combo box!")
      (.setCellEditor column default-cell-editor)
      (.setCellRenderer column default-cell-renderer))
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
    (swap! panel-map assoc :map-configuration p)
    p))

(defn- build-id [key side]
  (keyword (str key side)))

(defn- build-pound-id [key side]
  (keyword (str "#" key side)))

(defn- nth-string [n]
  (str n (cond (= 11 (mod n 100)) "th"
               (= 12 (mod n 100)) "th"
               (= 1 (mod n 10)) "st"
               (= 2 (mod n 10)) "nd"
               :else "th")))

(defn- build-oob-id [side-number]
  (build-id "oob-id" side-number))

(defn- build-oob-pound-id [side-number]
  (build-pound-id "oob-id" side-number))

(defn- build-nationality-id [side-number]
  (build-id "nationality-id" side-number))

(defn- build-nationality-pound-id [side-number]
  (build-pound-id "nationality-id" side-number))

(defn- build-counter-type-id [side-number]
  (build-id "counter-type-id" side-number))

(defn- build-counter-type-pound-id [side-number]
  (build-pound-id "counter-type-id" side-number))

(defn- build-counter-id [side-number]
  (build-id "counter-id" side-number))

(defn- build-counter-pound-id [side-number]
  (build-pound-id "counter-id" side-number))

(defn- build-number-counters-id [side-number]
  (build-id "number-counters-id" side-number))

(defn- build-number-counters-pound-id [side-number]
  (build-pound-id "number-counters-id" side-number))

(defn- build-add-to-oob-id [side-number]
  (build-id "add-to-oob" side-number))

(defn- build-remove-last-from-oob-id [side-number]
  (build-id "remove-last-from-oob" side-number))

(defn- build-remove-last-from-oob-pound-id [side-number]
  (build-pound-id "remove-last-from-oob" side-number))

(defn- update-counter-model [side-number t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id side-number)]))
        counter-type (sc/selection (sc/select r [(build-counter-type-pound-id side-number)]))
        counter (sc/select r [(build-counter-pound-id side-number)])]
    (sc/config! counter :model (extract-counters nationality counter-type))))

(defn- add-to-oob-action [side-number t]
  (let [r (sc/to-root t)
        nationality (sc/selection (sc/select r [(build-nationality-pound-id side-number)]))
        counter (sc/selection (sc/select r [(build-counter-pound-id side-number)]))
        number-counters-spinner (sc/select r [(build-number-counters-pound-id side-number)])
        number-counters (sc/selection number-counters-spinner)
        counters (apply concat (repeat number-counters (list (table/row-count t)
                                                       {:nationality nationality
                                                        :counter     counter
                                                        :unique-id   ""})))
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id side-number)])]
    (apply table/insert-at! t counters)
    (sc/selection! number-counters-spinner 1)
    (sc/config! remove-last-from-oob :enabled? true)))

(defn- remove-last-from-oob-action [side-number t]
  (let [r (sc/to-root t)
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id side-number)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-oob-layout [side-number]
  (let [t (sc/table :id (build-oob-id side-number) :model [:columns [:nationality :counter :unique-id]])
        nationalities (extract-nationalities)
        nationality (sc/combobox :id (build-nationality-id side-number)
                                 :model nationalities
                                 :listen [:action (fn [_] (update-counter-model side-number t))])
        counter-type (sc/combobox :id (build-counter-type-id side-number)
                                  :model ["Infantry" "Support Weapon" "Gun" "Vehicle" "Fortification" "Concealment"]
                                  :listen [:action (fn [_] (update-counter-model side-number t))])
        counter (sc/combobox :id (build-counter-id side-number)
                             :model (extract-counters (sc/selection nationality) (sc/selection counter-type)))
        number-counters (sc/spinner :id (build-number-counters-id side-number)
                                    :model (sc/spinner-model 1 :from 1 :to 26 :by 1))
        add-to-oob-button (sc/button :id (build-add-to-oob-id side-number)
                                     :text "Add to OoB"
                                     :listen [:action (fn [_] (add-to-oob-action side-number t))])
        remove-last-from-oob-button (sc/button :id (build-remove-last-from-oob-id side-number)
                                               :text "Remove last from OoB"
                                               :listen [:action (fn [_] (remove-last-from-oob-action side-number t))]
                                               :enabled? false)
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Nationality: " nationality [:fill-h 10]
                                                      "Counter Type: " counter-type])
                         [:fill-v 5]
                         (sc/horizontal-panel :items ["Counter: " counter [:fill-h 10]
                                                      "Number of Counters: " number-counters])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-oob-button :fill-h remove-last-from-oob-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    layout))

(defn- initial-oob-page [side-number]
  (let [title (str "Order of Battle (OOB) Panel " (inc side-number))
        tip (str "Initial OOB for all units on side " (nth-string (inc side-number)) ".")]
    (proxy [WizardPage seesaw.selector.Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn- initial-oob-panel [side-number]
  (let [layout (initial-oob-layout side-number)
        p (sc/abstract-panel
            (initial-oob-page side-number)
            (layout/box-layout :vertical)
            layout)]
    (swap! panel-map assoc-in [:oob side-number] p)
    p))

(defn- build-unique-id [side]
  (build-id "unique-id" side))

(defn- build-unique-pound-id [side]
  (build-pound-id "unique-id" side))

(defn- build-position-id [side]
  (build-id "position-id" side))

(defn- build-position-pound-id [side]
  (build-pound-id "position-id" side))

(defn- build-covered-arc-id [side]
  (build-id "covered-arc-id" side))

(defn- build-covered-arc-pound-id [side]
  (build-pound-id "covered-arc-id" side))

(defn- build-setup-id [side]
  (build-id "setup-id" side))

(defn- build-setup-pound-id [side]
  (build-pound-id "setup-id" side))

(defn- build-add-to-setup-id [side]
  (build-id "add-to-setup" side))

(defn- build-add-to-setup-pound-id [side]
  (build-pound-id "add-to-setup" side))

(defn- build-remove-last-from-setup-id [side]
  (build-id "remove-last-from-setup" side))

(defn- build-remove-last-from-setup-pound-id [side]
  (build-pound-id "remove-last-from-setup" side))

(defn- add-to-setup-enabled? [unique-pound-id position-pound-id e]
  (let [r (sc/to-root e)
        s1 (-> r (sc/select [unique-pound-id]) sc/text)
        s2 (-> r (sc/select [position-pound-id]) sc/text)]
    (and (-> s1 str/blank? not)
         (-> s2 str/blank? not))))

(defn- configure-add-to-setup-enabled-state [side t]
  (let [r (sc/to-root t)
        add-to-oob (sc/select r [(build-add-to-setup-pound-id side)])]
    (sc/config! add-to-oob :enabled? (add-to-setup-enabled? (build-unique-pound-id side) (build-position-pound-id side) t))))

(defn- add-to-setup-action [side p _]
  (let [unique-id (sc/select p [(build-unique-pound-id side)])
        position (sc/select p [(build-position-pound-id side)])
        covered-arc (sc/select p [(build-covered-arc-pound-id side)])
        remove-last-from-oob (sc/select p [(build-remove-last-from-setup-pound-id side)])
        t (sc/select p [(build-setup-pound-id side)])]
    (table/insert-at! t (table/row-count t) {:unique-id (sc/text unique-id)
                                             :position (sc/text position)
                                             :covered-arc (sc/text covered-arc)})
    (sc/config! remove-last-from-oob :enabled? true)
    (sc/text! unique-id "")
    (sc/text! position "")
    (sc/text! covered-arc "")))

(defn- remove-last-from-setup-action [side p _]
  (let [remove-last-from-oob (sc/select p [(build-remove-last-from-setup-pound-id side)])
        t (sc/select p [(build-setup-pound-id side)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-setup-layout [setup-panel-index]
  (let [t (sc/table :id (build-setup-id setup-panel-index) :model [:columns [:unique-id :position :covered-arc]])
        unique-id-text (sc/text :id (build-unique-id setup-panel-index)
                                :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        position-text (sc/text :id (build-position-id setup-panel-index)
                               :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        covered-arc-text (sc/text :id (build-covered-arc-id setup-panel-index)
                                  :listen [:document (fn [_] (configure-add-to-setup-enabled-state setup-panel-index t))])
        add-to-setup-button (sc/button :id (build-add-to-setup-id setup-panel-index) :text "Add to OoB")
        remove-last-from-setup-button (sc/button :id (build-remove-last-from-setup-id setup-panel-index)
                                               :text "Remove last from OoB" :enabled? false)
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Unique ID: " unique-id-text [:fill-h 10]
                                                      "Position: " position-text [:fill-h 10]
                                                      "Covered Arc (CA): " covered-arc-text])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-setup-button :fill-h remove-last-from-setup-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    {:add-to-setup-button add-to-setup-button :remove-last-from-setup-button remove-last-from-setup-button :layout layout}))

(defn- initial-setup-page [setup-panel-index]
  (let [title (str "Setup Panel " (inc setup-panel-index))
        tip (str "Initial positions for all units setup " (nth-string (inc setup-panel-index)) ".")]
    (proxy [WizardPage seesaw.selector.Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn- initial-setup-panel [setup-panel-index]
  (let [{:keys [add-to-setup-button remove-last-from-setup-button layout]} (initial-setup-layout setup-panel-index)
        p (sc/abstract-panel
            (initial-setup-page setup-panel-index)
            (layout/box-layout :vertical)
            layout)]
    (sc/listen add-to-setup-button :action (partial add-to-setup-action setup-panel-index p))
    (sc/listen remove-last-from-setup-button :action (partial remove-last-from-setup-action setup-panel-index p))
    (swap! panel-map assoc-in [:setup setup-panel-index] p)
    p))

(def ^{:private true} optional-rules-page
  (proxy [WizardPage seesaw.selector.Tag] ["Optional Rules" "Choose the ASL optional rules in effect for this game."]
    (tag_name [] (.getSimpleName WizardPage))
    (rendering [path settings]
      (proxy-super rendering path settings)
      (doto this
        (.setFinishEnabled true)
        (.setNextEnabled false)))))

(defn- optional-rules-panel []
  (let [p (sc/abstract-panel
            optional-rules-page
            (layout/box-layout :vertical)
            {:border 5
             :items  [:fill-v
                      (sc/horizontal-panel :items [(sc/checkbox :id :use-iift? :text "Use IIFT?")])
                      [:fill-v 5]
                      (sc/horizontal-panel :items [(sc/checkbox :id :use-battlefield-integrity? :text "Use Battlefield Integrity?")])
                      :fill-v]})]
    (swap! panel-map assoc :optional-rules p)
    p))

(defn- is-asl-selected? []
  (let [p (:basic-parameters @panel-map)]
    (sc/selection (sc/select p [:#asl]))))

(defn- extract-rule-set []
  (if (is-asl-selected?) "asl" "asl-sk"))

(def new-wizard-page-factory
  (reify PageFactory
    (isTransient [_ _ _] false)
    (createPage [_ wizard-pages _]
      (let [c (.size wizard-pages)]
        (cond (= 0 c) (basic-parameters-panel)
              (= 1 c) (map-configuration-panel)
              :else (let [p (:basic-parameters @panel-map)
                          number-sides (sc/selection (sc/select p [:#number-sides]))]
                      (cond (< c (+ 2 number-sides)) (initial-oob-panel (- c 2))
                            (= c (+ 2 number-sides)) (initial-setup-panel (- c 2 number-sides))
                            :else (optional-rules-panel))))))))

(defn extract-basic-parameters [_]
  (let [p (:basic-parameters @panel-map)
        name (sc/text (sc/select p [:#name]))
        rule-set (extract-rule-set)
        fm (sc/text (sc/select p [:#first-move]))
        sm (sc/text (sc/select p [:#second-move]))
        nt (sc/selection (sc/select p [:#number-turns]))
        em? (sc/selection (sc/select p [:#extra-move?]))]
    {:name name :rule-set rule-set :fm fm :sm sm :nt nt :em? em?}))

(defn extract-optional-rules [_]
  (let [p (:optional-rules @panel-map)
        iift? (sc/selection (sc/select p [:#use-iift?]))
        battlefield-integrity? (sc/selection (sc/select p [:#use-battlefield-integrity?]))]
    {:iift? iift? :battlefield-integrity? battlefield-integrity?}))

(defn extract-orientation [_]
  (let [p (:map-configuration @panel-map)]
    (if (sc/selection (sc/select p [:#vertical-orientation])) "Vertical" "Horizontal")))

(defn extract-direction [_]
  (let [p (:map-configuration @panel-map)]
    (cond (sc/selection (sc/select p [:#up-direction])) "up"
          (sc/selection (sc/select p [:#right-direction])) "right"
          (sc/selection (sc/select p [:#down-direction])) "down"
          :else "left")))

(defn extract-map-rows-from-wizard [_]
  (let [p (:map-configuration @panel-map)
        t (sc/select p [:#map-table])
        number-columns (-> p (sc/select [:#number-columns]) sc/selection)
        tm (sc/config t :model)
        c (.getColumnCount tm)
        the-maps (for [i (range (.getRowCount tm))]
                   (map #(.getValueAt tm i %) (range 2 c)))]
    (partition number-columns the-maps)))

(defn- extract-initial-setup [p side]
  (let [t (sc/select p [(build-setup-pound-id side)])]
    (vec (for [i (range (table/row-count t))]
           (vec (vals (table/value-at t i)))))))

(defn extract-side2-initial-setup [_]
  (extract-initial-setup (:side-2 @panel-map) "2"))

(defn extract-side1-initial-setup [_]
  (extract-initial-setup (:side-1 @panel-map) "1"))