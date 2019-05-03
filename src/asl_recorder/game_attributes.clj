(ns asl-recorder.game-attributes
  (:require [clojure.zip :as zip]))

(defn- get-game-attributes [loc]
  (-> loc
      zip/up
      zip/up
      zip/up
      zip/node
      :attrs))

(defn get-side1-from-loc [loc]
  (:side1 (get-game-attributes loc)))

(defn get-side2-from-loc [loc]
  (:side2 (get-game-attributes loc)))

(defn get-number-turns-from-loc [loc]
  (:number-full-turns (get-game-attributes loc)))

