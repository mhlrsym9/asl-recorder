(ns asl-recorder.game-attributes
  (:require [clojure.data.zip.xml :as zip-xml]
            [clojure.zip :as zip]))

(defn- get-scenario-zipper [loc]
  (-> loc
      zip/up
      zip/up
      zip/up
      zip/up
      zip/down
      zip/node
      zip/xml-zip))

(defn- get-side-from-loc [loc side-number]
  (let [z (get-scenario-zipper loc)
        l (zip-xml/xml1-> z :sides :side (zip-xml/attr= :move-order side-number))]
    (-> l
        zip/node
        :attrs
        :side-name)))

(defn get-side1-from-loc [loc]
  (get-side-from-loc loc 1))

(defn get-side2-from-loc [loc]
  (get-side-from-loc loc 2))

(defn- get-game-attributes [loc]
  (-> loc
      zip/up
      zip/up
      zip/up
      zip/up
      zip/down
      zip/node
      :attrs))

(defn get-number-turns-from-loc [loc]
  (:number-full-turns (get-game-attributes loc)))

