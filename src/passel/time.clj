(ns passel.time
  (:require
   [clojure.string :as str])
  (:import
   [java.sql Timestamp]
   [java.time ZoneId Instant ZonedDateTime Duration]
   [java.time.format DateTimeFormatter]))

(def formatters
  {:iso-time            DateTimeFormatter/ISO_TIME
   :iso-date-time       DateTimeFormatter/ISO_DATE_TIME
   :iso-instant         DateTimeFormatter/ISO_INSTANT
   :rfc-1123            DateTimeFormatter/RFC_1123_DATE_TIME
   :iso-local-date-time DateTimeFormatter/ISO_LOCAL_DATE_TIME})

(defn current-ms []
  (System/currentTimeMillis))

(defn- zoned-ms [ms zone-id]
  (-> (Instant/ofEpochMilli ms)
      (.atZone (ZoneId/of zone-id))))

(defn- zoned-ts [^Timestamp ts zone-id]
  (let [zone-region    (ZoneId/of zone-id)
        local-datetime (.toLocalDateTime ts)]
    (.atZone local-datetime zone-region)))

(defn- custom-datetime [dt-str]
  (let [mx (re-matches #"^([0-9]{4})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1]) (2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])$" dt-str)]
    (->> (rest mx) (map #(Integer/parseInt %)))))

(defn- zoned-custom
  "Takes a string as datetime with format YYYY-MM-dd HH:mm:ss
  Converts it to a UTC Tiemstamp in millis given an arbitrary Tiemzone"
  [^String dt-str zone-id]
  (let [parts           (custom-datetime dt-str)
        [y m d h min s] parts]
    (ZonedDateTime/of y m d h min s 0 zone-id)))

(defn sql-timestamp
  ([]
   (sql-timestamp (System/currentTimeMillis)))
  ([tstamp-ms]
   (Timestamp. tstamp-ms)))

(defn- to-millis [o]
  (.toEpochMilli (.toInstant o)))

(defn convert-ts-zone [ts from-zone to-zone]
  (let [zdt         (zoned-ts ts from-zone)
        zone-region (ZoneId/of to-zone)]
    (.withZoneSameInstant zdt zone-region)))

(defn convert-ms-zone [ms from-zone to-zone]
  (let [zdt         (zoned-ms ms from-zone)
        zone-region (ZoneId/of to-zone)]
    (->> (.withZoneSameInstant zdt zone-region)
         (to-millis))))


;; formatting

(defn- make-formatter [fmt-str]
  (DateTimeFormatter/ofPattern fmt-str))

(defn- format-zoned
  ([zdt]
   (format-zoned zdt (:iso-instant formatters)))
  ([zdt formatter]
   (.format zdt formatter)))

(defn format-ms [ms formatter]
  (let [fmt (make-formatter formatter)
        zdt (zoned-ms ms "UTC")]
    (format-zoned zdt fmt)))

;; moments

(defn time-ago [^Timestamp past-time zone-id]
  (let [current  (zoned-ts (sql-timestamp) zone-id)
        past     (zoned-ts past-time zone-id)
        duration (Duration/between past current)
        secs     (.getSeconds duration)
        mins     (.toMinutes duration)
        hrs      (.toHours duration)
        days     (.toDays duration)]

    (cond
      (and (not (pos? days))
           (not (pos? hrs)))
      (format "%sm ago" mins)

      (pos? days)
      (format "%sd ago" days)

      :else
      (format "%sh ago" hrs))))
