(ns passel.config
  (:require
   [raven.util :as u]))

(defn read-config [config-path]
  (u/ignore-errors
   (u/slurp-edn config-path)))

(defonce current (atom nil))

(defn lookup
  ([] @current)
  ([key] (get @current key)))

(defn init! [config-path]
  (->> (read-config config-path)
       (reset! current)))
