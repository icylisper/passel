(ns passel.cache
  (:require
   [clojure.core.cache :as cache]))

(def C (atom (cache/fifo-cache-factory {})))

(defn invalidate! [k]
  (swap! C dissoc k)
  :ok)

(defn cached
  ([k] (get @C k))
  ([k f]
   (swap! C cache/through-cache k f)
   (get @C k)))

(defn clear! []
  (reset! C (cache/fifo-cache-factory {})))
