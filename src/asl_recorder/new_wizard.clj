(ns asl-recorder.new-wizard
  (:require [seesaw [core :as sc] [mig :as sm] [chooser :as sch] [dnd :as dnd] [layout :as layout] selector [table :as t]]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [seesaw [table :as table] [mig :as sm]]
            [asl-recorder.new-wizard-pages.basic-configuration :as basic]
            [asl-recorder.new-wizard-pages.map-configuration :as map]
            [asl-recorder.new-wizard-pages.optional-rules :as optional]
            [asl-recorder.new-wizard-pages.side-configuration :as side]
            [asl-recorder.new-wizard-pages.utilities :as u])
  (:import [com.github.cjwizard WizardContainer PageFactory WizardPage WizardListener WizardSettings]
           (javax.swing JTable DefaultCellEditor)
           (javax.swing.table TableColumnModel TableColumn)
           (javax.swing.table AbstractTableModel DefaultTableCellRenderer)
           (java.io File)
           (seesaw.selector Tag)))

; TODO: Choose scenario from list... pre-fills basic-configuration and map-configuration.
; TODO: Different size maps (HASL, DASL, etc.)
; TODO: Move to second page iff all fields on first page set.
; TODO: Move to second page iff first move name != second move name.
; TODO: Move to third page iff all fields on second page set.
; TODO: Make sure each id is unique.
; TODO: Make sure all positions are valid ASL format and belong to one of the maps on the board.

; The wizard has problems destroying pages if you back up in that the standard seesaw
; select method thinks there are multiple elements for each created seesaw id, and thus
; seems to return the first created (select w/ id returns only one element). Therefore,
; as each panel is created, the code adds a reference to the actual current panel into
; this stateful map (argh!). When the data is extracted, the code uses these references
; instead of the dialog root to extract the data in the last instance of each panel.
(def ^{:private true} panel-map (atom {:basic-configuration nil :optional-rules nil :map-configuration nil :oob [] :setup []}))

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
        entry {:nationality     nationality
               :counter         counter
               :number-counters number-counters}
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id side-number)])]
    (apply table/insert-at! t entry)
    (sc/selection! number-counters-spinner 1)
    (sc/config! remove-last-from-oob :enabled? true)))

(defn- remove-last-from-oob-action [side-number t]
  (let [r (sc/to-root t)
        remove-last-from-oob (sc/select r [(build-remove-last-from-oob-pound-id side-number)])]
    (table/remove-at! t (- (table/row-count t) 1))
    (sc/config! remove-last-from-oob :enabled? (< 0 (table/row-count t)))))

(defn- initial-oob-layout [side-number]
  (let [t (sc/table :id (build-oob-id side-number) :model [:columns [:nationality :counter :number-counters]])
        nationalities (u/extract-nationalities)
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
  (let [{:keys [side-name]} (nth (side/extract-side-configuration panel-map) side-number)
        title (str side-name " Order of Battle (OOB) Panel")
        tip (str "Initial OOB for all units on the " side-name "side.")]
    (proxy [WizardPage Tag] [title tip]
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
    (proxy [WizardPage Tag] [title tip]
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

(defn- final-layout []
  (let [layout {:border 5
                :items ["Congratulations! You're done!"]}]
    layout))

(defn- final-page []
  (let [title "Final Panel "
        tip "This is the final panel!"]
    (proxy [WizardPage Tag] [title tip]
      (tag_name [] (.getSimpleName WizardPage))
      (rendering [path settings]
        (proxy-super rendering path settings)
        (doto this
          (.setFinishEnabled true)
          (.setNextEnabled false))))))

(defn- final-panel []
  (let [layout (final-layout)
        p (sc/abstract-panel
            (final-page)
            (layout/box-layout :vertical)
            layout)]
    p))

(def new-wizard-page-factory
  (reify PageFactory
    (isTransient [_ _ _] false)
    (createPage [_ wizard-pages _]
      (let [c (.size wizard-pages)]
        (cond (= 0 c) (let [bpp (basic/basic-configuration-panel)]
                        (swap! panel-map assoc :basic-configuration bpp)
                        bpp)
              (= 1 c) (let [orp (optional/optional-rules-panel)]
                        (swap! panel-map assoc :optional-rules orp)
                        orp)
              (= 2 c) (let [mcp (map/map-configuration-panel)]
                        (swap! panel-map assoc :map-configuration mcp)
                        mcp)
              (= 3 c) (let [scp (side/side-configuration-panel)]
                        (swap! panel-map assoc :side-configuration scp)
                        scp)
              :else (let [p (:basic-configuration @panel-map)
                          number-sides (sc/selection (sc/select p [:#number-sides]))]
                      (cond (< c (+ 2 number-sides)) (initial-oob-panel (- c 2))
                            (= c (+ 2 number-sides)) (initial-setup-panel (- c 2 number-sides))
                            :else (final-panel))))))))

(defn extract-wizard-data []
  {:basic-configuration (basic/extract-basic-configuration panel-map)
   :optional-rules (optional/extract-optional-rules panel-map)
   :map-configuration (map/extract-map-configuration panel-map)
   :side-configuration (side/extract-side-configuration panel-map)})

(defn- extract-initial-setup [p side]
  (let [t (sc/select p [(build-setup-pound-id side)])]
    (vec (for [i (range (table/row-count t))]
           (vec (vals (table/value-at t i)))))))

(defn extract-side2-initial-setup [_]
  (extract-initial-setup (:side-2 @panel-map) "2"))

(defn extract-side1-initial-setup [_]
  (extract-initial-setup (:side-1 @panel-map) "1"))