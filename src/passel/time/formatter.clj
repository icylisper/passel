(ns passel.time.formatter
  (:import
   [java.util TimeZone]
   [java.text DateFormat SimpleDateFormat]))

(defn timezone
  "Looks up a java.util.TimeZone instance by name"
  [tz]
  (if (string? tz)
    (TimeZone/getTimeZone ^String tz)
    tz))

(defn ^SimpleDateFormat simple-date-formatter
  "Create a java.text.SimpleDateFormat with `format-string` and
  optional `tz` timezone."
  ([format-string]
   (simple-date-formatter format-string "UTC"))
  ([format-string tz]
   (doto (SimpleDateFormat. format-string)
     (.setTimeZone (timezone tz)))))

(def ^:constant iso-8601-formatter
  (simple-date-formatter "yyyy-MM-dd'T'HH:mm:ss.SSSX"))

(defn ^SimpleDateFormat as-formatter [formatter-args]
  (let [[formatter & _] formatter-args]
    (if (instance? SimpleDateFormat formatter)
      formatter
      (apply simple-date-formatter formatter-args))))

(defn format-timestamp [timestamp & formatter-args]
  (.format ^DateFormat (as-formatter formatter-args) timestamp))
