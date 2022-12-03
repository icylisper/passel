(ns passel.time
  (:require
   [clojure.string :as str]
   [passel.time.zone :as zone]
   [passel.time.parser :as parser]
   [passel.time.formatter :as f])
  (:import
   [java.sql Timestamp]))

;; java.sql.Timsestamp and java.util.Date have no zone but UTC

(defn current-ms []
  (System/currentTimeMillis))

(defn format-timestamp
  "Format a timestamp using a SimpleDateFormat, by defaults this
  function formats as ISO 8601 timestamp string, e.g.,

  (format-timestamp)
  => \"2017-04-12T17:18:37.363+0000\"

  (format-timestamp (System/currentTimeMillis))
  => \"2017-04-12T17:18:37.363+0000\"

  (format-timestamp (System/currentTimeMillis) \"yyyy/MM\")
  => \"2017/04\"
  "
  ([]
   (format-timestamp (current-ms)))
  ([timestamp]
   (f/format-timestamp timestamp f/iso-8601-formatter)))

;; zone specific
(defn format-ms [ms zone-id]
  (let [{:keys [month day hour min]} (zone/parse-utc-ms ms zone-id)]
    (format "%s/%s %s:%s" month day hour min)))

(defn timestamp->ms
  "Takes a string as datetime with format YYYY-MM-dd HH:mm:ss
  Converts it to a UTC Tiemstamp in millis given an arbitrary Tiemzone"
  [time tz]
  (let [parts   (parser/custom-datetime time)
        zone-id (zone/zone-of (or tz "UTC"))]
    (zone/to-millis parts zone-id)))

(defn sql-timestamp
  ([]
   (sql-timestamp (System/currentTimeMillis)))
  ([tstamp-ms]
   (Timestamp. tstamp-ms)))
