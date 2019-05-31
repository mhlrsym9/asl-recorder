(ns asl-recorder.tree-edit
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.string :as str]))

(defn- tree-edit
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   apply the editor."
  ([zipper matcher editor]
   (:xml (tree-edit zipper matcher editor false)))
  ([zipper matcher editor is-modified?]
   (loop [loc zipper
          modified? is-modified?]
     (if (zip/end? loc)
       {:xml (zip/root loc) :modified? modified?}
       (if-let [matcher-result (matcher loc)]
         (let [new-loc (zip/edit loc editor)]
           (if (not (= (zip/node new-loc) (zip/node loc)))
             (recur (zip/next new-loc) true)
             (recur (zip/next new-loc) modified?)))
         (recur (zip/next loc) modified?))))))

(defn- found-scenario-element? [loc]
  (let [tag (:tag (zip/node loc))]
    (= tag :scenario)))

(defn- found-turn-element? [loc]
  (let [tag (:tag (zip/node loc))]
    (= tag :turn)))

(defn- found-die-roll-element? [loc]
  (let [tag (:tag (zip/node loc))]
    (= tag :die-roll)))

(defn- update-the-scenario-element [node]
  (let [number-full-turns (get-in node [:attrs :number-full-turns])
        additional-half-turn (get-in node [:attrs :additional-half-turn])]
    (-> node
        (assoc-in [:attrs :number-full-turns] (if (string? number-full-turns)
                                                (Integer/parseInt number-full-turns)
                                                number-full-turns))
        (assoc-in [:attrs :additional-half-turn] (if (string? additional-half-turn)
                                                   (= "true" (str/lower-case additional-half-turn))
                                                   additional-half-turn)))))

(defn- convert-turn-number-to-int [node]
  (let [turn-number (get-in node [:attrs :number])]
    (if (string? turn-number)
      (assoc-in node [:attrs :number] (Integer/parseInt turn-number))
      node)))

(defn- convert-die-roll-to-int [node]
  (let [c (:content node)]
    (cond (string? c) (assoc node :content (Integer/parseInt c))
          (seq? c) (assoc node :content (map (fn [e] (Integer/parseInt e)) c))
          :else node)))

(defn update-scenario-element [the-xml]
  (-> the-xml
      zip/xml-zip
      (tree-edit found-scenario-element? update-the-scenario-element)))

(defn turn-number-to-int [the-xml]
  (-> the-xml
      zip/xml-zip
      (tree-edit found-turn-element? convert-turn-number-to-int)))

(defn die-roll-to-int [the-xml]
  (-> the-xml
      zip/xml-zip
      (tree-edit found-die-roll-element? convert-die-roll-to-int)))

