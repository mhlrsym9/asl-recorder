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
(def ^{:private true} panel-map (atom {:basic-parameters nil :map-configuration nil :side-2 nil :side-1 nil}))

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
    (swap! panel-map assoc :basic-parameters p)
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

(defn- second-move-initial-setup-page [name]
  (let [title (str name " Initial Setup")
        tip (str "Initial positions for all on-board " name " units.")]
    (proxy [WizardPage seesaw.selector.Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage)))))

(defn- build-id [key side]
  (keyword (str key side)))

(defn- build-pound-id [key side]
  (keyword (str "#" key side)))

(defn- build-unique-id [side]
  (build-id "unique-id" side))

(defn- build-unique-pound-id [side]
  (build-pound-id "unique-id" side))

(defn- build-position-id [side]
  (build-id "position-id" side))

(defn- build-position-pound-id [side]
  (build-pound-id "position-id" side))

(defn- build-oob-side-id [side]
  (build-id "oob-side-id" side))

(defn- build-oob-side-pound-id [side]
  (build-pound-id "oob-side-id" side))

(defn- build-add-to-oob-id [side]
  (build-id "add-to-oob" side))

(defn- build-add-to-oob-pound-id [side]
  (build-pound-id "add-to-oob" side))

(defn- build-remove-last-from-oob-id [side]
  (build-id "remove-last-from-oob" side))

(defn- build-remove-last-from-oob-pound-id [side]
  (build-pound-id "remove-last-from-oob" side))

(defn- add-to-oob-enabled? [unique-pound-id position-pound-id e]
  (let [r (sc/to-root e)
        s1 (-> r (sc/select [unique-pound-id]) sc/text)
        s2 (-> r (sc/select [position-pound-id]) sc/text)]
    (and (-> s1 str/blank? not)
         (-> s2 str/blank? not))))

(defn- configure-add-to-oob-enabled-state [side t]
  (let [r (sc/to-root t)
        add-to-oob (sc/select r [(build-add-to-oob-pound-id side)])]
    (sc/config! add-to-oob :enabled? (add-to-oob-enabled? (build-unique-pound-id side) (build-position-pound-id side) t))))

(defn- add-to-oob-action [side p _]
  (let [unique-id (sc/select p [(build-unique-pound-id side)])
        position (sc/select p [(build-position-pound-id side)])
        remove-last-from-oob (sc/select p [(build-remove-last-from-oob-pound-id side)])
        t (sc/select p [(build-oob-side-pound-id side)])]
    (table/insert-at! t (table/row-count t) {:unique-id (sc/text unique-id) :position (sc/text position)})
    (sc/config! remove-last-from-oob :enabled? true)
    (sc/text! unique-id "")
    (sc/text! position "")))

(defn- remove-from-oob-action [side p _]
  (let [remove-last-from-oob (sc/select p [(build-remove-last-from-oob-pound-id side)])
        t (sc/select p [(build-oob-side-pound-id side)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-setup-layout [side]
  (let [t (sc/table :id (build-oob-side-id side) :model [:columns [:unique-id :position]])
        unique-id-text (sc/text :id (build-unique-id side) :listen [:document (fn [_] (configure-add-to-oob-enabled-state side t))])
        position-text (sc/text :id (build-position-id side) :listen [:document (fn [_] (configure-add-to-oob-enabled-state side t))])
        add-to-oob-button (sc/button :id (build-add-to-oob-id side) :text "Add to OoB")
        remove-last-from-oob-button (sc/button :id (build-remove-last-from-oob-id side) :text "Remove last from OoB" :enabled? false)
        layout {:border 5
                :items  [(sc/horizontal-panel :items ["Unique ID: " unique-id-text [:fill-h 10] "Position: " position-text])
                         [:fill-v 5]
                         (sc/horizontal-panel :items [add-to-oob-button :fill-h remove-last-from-oob-button])
                         [:fill-v 5]
                         (sc/scrollable t)]}]
    {:add-to-oob-button add-to-oob-button :remove-last-from-oob-button remove-last-from-oob-button :layout layout}))

(defn- second-move-initial-setup-panel [name]
  (let [{:keys [add-to-oob-button remove-last-from-oob-button layout]} (initial-setup-layout "2")
        p (sc/abstract-panel
            (second-move-initial-setup-page name)
            (layout/box-layout :vertical)
            layout)]
    (sc/listen add-to-oob-button :action (partial add-to-oob-action "2" p))
    (sc/listen remove-last-from-oob-button :action (partial remove-from-oob-action "2" p))
    (swap! panel-map assoc :side-2 p)
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
  (let [{:keys [add-to-oob-button remove-last-from-oob-button layout]} (initial-setup-layout "1")
        p (sc/abstract-panel
            (first-move-initial-setup-page name)
            (layout/box-layout :vertical)
            layout)]
    (sc/listen add-to-oob-button :action (partial add-to-oob-action "1" p))
    (sc/listen remove-last-from-oob-button :action (partial remove-from-oob-action "1" p))
    (swap! panel-map assoc :side-1 p)
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

(defn extract-basic-parameters [_]
  (let [p (:basic-parameters @panel-map)
        name (sc/text (sc/select p [:#name]))
        fm (sc/text (sc/select p [:#first-move]))
        sm (sc/text (sc/select p [:#second-move]))
        nt (sc/selection (sc/select p [:#number-turns]))
        em? (sc/selection (sc/select p [:#extra-move?]))]
    {:name name :fm fm :sm sm :nt nt :em? em?}))

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
  (let [t (sc/select p [(build-oob-side-pound-id side)])]
    (vec (for [i (range (table/row-count t))]
           (vec (vals (table/value-at t i)))))))

(defn extract-side2-initial-setup [_]
  (extract-initial-setup (:side-2 @panel-map) "2"))

(defn extract-side1-initial-setup [_]
  (extract-initial-setup (:side-1 @panel-map) "1"))