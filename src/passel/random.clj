(ns passel.random
  (:import
   [java.util UUID]))

(defn uuid-str [] (str (UUID/randomUUID)))
