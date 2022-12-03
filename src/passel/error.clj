(ns passel.error
  (:import
   [java.io StringWriter PrintWriter]))

(defmacro ignore-errors
  [& body]
  `(try
     ~@body
     (catch Throwable e# nil)))

(defn stringify-error [error]
  (let [result (StringWriter.)
        printer (PrintWriter. result)]
    (.printStackTrace error printer)
    (.toString result)))

(defmacro error-as-value [& body]
  `(try
    ~@body
    (catch Exception e#
      (or (ex-data e#)
          {:error-id :unidentified
           :msg (.getMessage e#)}))))
