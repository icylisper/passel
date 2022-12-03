(ns passel.config
  (:require
   [clojure.edn :as edn]
   [passel.error :as error]))

(defn slurp-edn [path]
  (-> path
      slurp
      edn/read-string))

(defn read-config [config-path]
  (ignore-errors
   (slurp-edn config-path)))

(defonce current (atom nil))

(defn lookup
  ([] @current)
  ([key] (get @current key)))

(defn init! [config-path]
  (->> (read-config config-path)
       (reset! current)))
