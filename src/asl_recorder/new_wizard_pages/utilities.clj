(ns asl-recorder.new-wizard-pages.utilities
  (:require [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import (java.io File)))

(defn extract-nationalities []
  (with-open [r (-> "nationalities.xml" io/resource io/reader)]
    (let [z (-> r xml/parse zip/xml-zip)]
      (map (zip-xml/attr :name) (zip-xml/xml-> z :Nationality)))))

(defn extract-counters [nationality unit-type]
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

(defn build-id
  ([key side]
   (keyword (str key side)))
  ([key side suffix]
   (keyword (str key side "-" suffix))))

(defn build-pound-id
  ([key side]
   (keyword (str "#" key side)))
  ([key side suffix]
   (keyword (str "#" key side "-" suffix))))

(defn nth-string [n]
  (cond (= 1 n) "first"
        (= 2 n) "second"
        (= 3 n) "third"
        (= 11 (mod n 100)) (str n "th")
        (= 12 (mod n 100)) (str n "th")
        (= 1 (mod n 10)) (str n "st")
        (= 2 (mod n 10)) (str n "nd")
        :else "th"))

(defn capital-nth-string [n]
  (cond (= 1 n) "First"
        (= 2 n) "Second"
        (= 3 n) "Third"
        :else (nth-string n)))





