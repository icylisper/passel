(ns passel.json
  (:require
   [jsonista.core :as json]
   [camel-snake-kebab.core :as csk]
   [passel.error :as error]))

(defn read-str [str]
  (->> (json/object-mapper
        {:decode-key-fn csk/->kebab-case-keyword})
       (json/read-value str)))

(defn write-str [thing]
  (json/write-value-as-string thing))

(defn read-stream [is]
  (let [m (error/ignore-errors
           (-> (slurp is)
               (read-str)))]
    (when (map? m) m)))
