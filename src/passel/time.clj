(ns passel.time
  (:require
   [clojure.string :as str])
  (:import
   [java.time ZoneId Instant ZonedDateTime Period Duration]
   [java.sql Timestamp]))

(defn parse-utc-ms
  "(parse-utc-ms 1670032954472 UTC)"
  [ms zone-id]
  (let [zdt (-> (Instant/ofEpochMilli ms)
                (.atZone (ZoneId/of zone-id)))]
    {:year  (.getYear zdt)
     :month (.getMonthValue zdt)
     :day   (.getDayOfMonth zdt)
     :hour  (.getHour zdt)
     :min   (.getMinute zdt)}))

(defn format-ms [ms zone-id]
  (let [{:keys [month day hour min]} (parse-utc-ms ms zone-id)]
    (format "%s/%s %s:%s" month day hour min)))

(defn parse-custom-datetime [dt]
  (let [mx (re-matches #"^([0-9]{4})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1]) (2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])$" dt)]
    (->> (rest mx) (map #(Integer/parseInt %)))))

(defn- as-zone-id [zone-id]
  (ZoneId/of zone-id))

(defn- zoned-date-time [parts zone-id]
  (let [[y m d h min s] parts]
    (ZonedDateTime/of y m d h min s 0 zone-id)))

(defn to-millis-from-epoch [o]
  (.toEpochMilli (.toInstant o)))

(defn timestamp-utc-ms
  "Takes a string as datetime with format YYYY-MM-dd HH:mm:ss
  Converts it to a UTC Tiemstamp in millis given an arbitrary Tiemzone"
  [time tz]
  (let [parts   (parse-custom-datetime time)
        zone-id (as-zone-id (or tz "UTC"))]
    (->> (zoned-date-time parts zone-id)
         to-millis-from-epoch)))

(defn sql-timestamp
  ([]
   (sql-timestamp (System/currentTimeMillis)))
  ([tstamp-ms]
   (Timestamp. tstamp-ms)))
