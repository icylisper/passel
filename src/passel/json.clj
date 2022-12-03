(ns passel.json
  (:require
   [jsonista.core :as json]
   [camel-snake-kebab.core :as csk]
   [passel.error :as error]))

(defn read-json [str]
  (->> (json/object-mapper
        {:decode-key-fn csk/->kebab-case-keyword})
       (json/read-value str)))

(defn write-json [thing]
  (json/write-value-as-string thing))

(defn read-stream [is]
  (let [m (error/ignore-errors
           (-> (slurp is)
               (read-json)))]
    (when (map? m) m)))
