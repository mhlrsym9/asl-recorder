(ns asl-recorder.new-wizard
  (:require [asl-recorder.new-wizard-pages.basic-configuration :as basic]
            [asl-recorder.new-wizard-pages.final :as final]
            [asl-recorder.new-wizard-pages.map-configuration :as map]
            [asl-recorder.new-wizard-pages.order-of-battle :as oob]
            [asl-recorder.new-wizard-pages.optional-rules :as optional]
            [asl-recorder.new-wizard-pages.initial-setup :as is]
            [asl-recorder.new-wizard-pages.side-configuration :as side])
  (:import [com.github.cjwizard PageFactory]))

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
(def ^{:private true} panel-map (atom {:basic-configuration nil :optional-rules nil :map-configuration nil :initial-setup-oob [] :initial-setup [] :reinforcements-oob []}))

(defn- extract-initial-setup-oobs []
  (map-indexed (fn [idx itm] (oob/extract-initial-setup-oob itm idx)) (:initial-setup-oob panel-map)))

(defn- extract-reinforcements-oobs []
  (map-indexed (fn [idx itm] (oob/extract-reinforcement-oob itm idx)) (:reinforcements-oob panel-map)))

(defn- extract-side-names-remaining [sc k extract-fn]
  (let [total-side-name-frequencies (apply hash-map (mapcat (fn [side] [(:side-name side) (k side)]) sc))
        side-names-used-in-oobs-frequencies (frequencies (map :side-name (extract-fn)))]
    (keys (filter (fn [[k v]] (not= v (get side-names-used-in-oobs-frequencies k 0))) total-side-name-frequencies))))

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
              :else (let [side-configuration (side/extract-side-configuration panel-map)
                          side-names-remaining-for-initial-setup-oobs (extract-side-names-remaining side-configuration :number-initial-setup-groups extract-initial-setup-oobs)
                          total-initial-setup-groups (apply + (map :number-initial-setup-groups side-configuration))
                          total-reinforcement-groups (apply + (map :number-reinforcement-groups side-configuration))]
                      (cond (< c (+ 4 (* 2 total-initial-setup-groups)))
                            (let [group-number (quot (- c 4) 2)]
                              (if (even? c)
                                (let [isop (oob/initial-setup-oob-panel group-number side-configuration side-names-remaining-for-initial-setup-oobs)]
                                  (swap! panel-map assoc-in [:initial-setup-oob group-number] isop)
                                  isop)
                                (let [isp (is/initial-setup-panel group-number side-configuration)]
                                  (swap! panel-map assoc-in [:initial-setup group-number] isp)
                                  isp)))
                            (< c (+ 4 (* 2 total-initial-setup-groups) total-reinforcement-groups))
                            (let [group-number (- c 4 (* 2 total-initial-setup-groups))
                                  side-names-remaining-for-reinforcement-oobs (extract-side-names-remaining side-configuration :number-reinforcement-groups extract-reinforcements-oobs)
                                  rop (oob/reinforcement-oob-panel group-number side-configuration side-names-remaining-for-reinforcement-oobs)]
                              (swap! panel-map assoc-in [:reinforcements-oob group-number] rop)
                              rop)
                            :else (final/final-panel))))))))

(defn extract-wizard-data []
  {:basic-configuration (basic/extract-basic-configuration panel-map)
   :optional-rules      (optional/extract-optional-rules panel-map)
   :map-configuration   (map/extract-map-configuration panel-map)
   :side-configuration  (let [initial-setup-oobs (extract-initial-setup-oobs)
                              initial-setups (map-indexed (fn [idx itm] (is/extract-initial-setup itm idx)) (:initial-setup panel-map))
                              side-setups (map #({:side-name (:side-name %1) :initial-setup %2}) initial-setup-oobs initial-setups)]
                          (map (fn [{:keys [side-name] :as sc}] (assoc sc :initial-setup
                                                                          (apply concat (map :initial-setup
                                                                                             (filter #(= side-name (:side-name %))
                                                                                                     side-setups)))))
                               (side/extract-side-configuration panel-map)))})