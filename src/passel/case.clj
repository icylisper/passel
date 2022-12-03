(ns passel.case
  (:require
   [clojure.walk :as walk]))

(defn- transform-keys [t coll]
  (let [f (fn [[k v]] [(t k) v])]
    (walk/postwalk (fn [x]
                     (if (map? x)
                       (into {} (map f x))
                       x))
                   coll)))

(defn kebab-map [m]
  (transform-keys csk/->kebab-case-keyword m))

(defn snake-map [m]
  (transform-keys csk/->snake_case_keyword m))
