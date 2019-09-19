(ns asl-recorder.new-wizard-pages.utilities
  (:require [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.java.io :as io]
            [clojure.zip :as zip]))

(defn extract-nationalities []
  (with-open [r (-> "nationalities.xml" io/resource io/reader)]
    (let [z (-> r xml/parse zip/xml-zip)]
      (map (zip-xml/attr :name) (zip-xml/xml-> z :Nationality)))))

