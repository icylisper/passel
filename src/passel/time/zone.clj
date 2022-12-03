(ns passel.time.zone
  (:import
   [java.time ZoneId ZonedDateTime Instant]))

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

(defn- zoned-date-time [parts zone-id]
  (let [[y m d h min s] parts]
    (ZonedDateTime/of y m d h min s 0 zone-id)))

(defn- to-millis-from-epoch [o]
  (.toEpochMilli (.toInstant o)))

(defn zone-of [zone-id]
  (ZoneId/of zone-id))

(defn to-millis [datetime-parts zone-id]
  (->> (zoned-date-time datetime-parts zone-id)
       to-millis-from-epoch))
