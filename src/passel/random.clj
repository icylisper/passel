(ns passel.random
  (:import
   [java.util UUID]))

(defn uuid-str [] (str (UUID/randomUUID)))

(defn rand-str [len]
  (transduce
   (map (fn [_]
          (let [rnd (rand-int 52)]
            (char (+ (mod rnd 26)
                     (int (if (< rnd 26) \a \A)))))))
   str
   ""
   (range len)))
