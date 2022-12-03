(ns passel.walk
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]))

(defn- hyphen->underscore [k]
  (-> (name k)
      (str/replace  #"-" "_")))

(defn- underscore->hyphen [k]
  (-> (name k)
      (str/replace #"_" "-")))

(defn- transform-map [key-fn val-fn coll]
  (let [f (fn [[k v]] [(key-fn k) (val-fn v)])]
    (walk/postwalk (fn [x]
                     (if (map? x)
                       (into {} (map f x))
                       x))
                   coll)))

(defn- transform-keys [t coll]
  (let [f (fn [[k v]] [(t k) v])]
    (walk/postwalk (fn [x]
                     (if (map? x)
                       (into {} (map f x))
                       x))
                   coll)))

(defn hyphenize-keys [coll]
  (transform-keys ->kebab-case-keyword coll))

(defn- as-key [k]
  (-> (hyphen->underscore k)
      (str/lower-case)
      (keyword)))

(defn- flatten-shallow [coll]
  (transform-map as-key identity coll))

(defn flatten-keys
  ([form]
     (into {} (flatten-keys form nil)))
  ([form pre]
   (mapcat (fn [[k v]]
             (let [prefix (if pre (str pre "." (name k)) (name k))]
               (cond
                 (map? v)
                 (flatten-keys v prefix)

                 (vector? v)
                 (do
                   [[(as-key prefix) (map flatten-shallow v)]])

                 :else
                 [[(as-key prefix) v]])))
           form)))
