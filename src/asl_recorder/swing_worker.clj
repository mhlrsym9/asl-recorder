(ns asl-recorder.swing-worker
  (:gen-class
    :main false
    :extends javax.swing.SwingWorker
    :exposes-methods {publish publishFromClojure}))

(defn -publishFromClojure [this chunks]
  (.publish this chunks))

