(ns passel.log
  (:require
   [clojure.string :as str])
  (:import
   [java.util TimeZone]
   [java.text DateFormat SimpleDateFormat]
   [clojure.lang ExceptionInfo]))

(defn timezone
  "Looks up a java.util.TimeZone instance by name"
  [tz]
  (if (string? tz)
    (java.util.TimeZone/getTimeZone ^String tz)
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
   (format-timestamp (System/currentTimeMillis)))
  ([timestamp]
   (format-timestamp timestamp iso-8601-formatter))
  ([timestamp & formatter-args]
   (.format ^DateFormat (as-formatter formatter-args) timestamp)))

(defn safe-pr-str [o]
  (binding [*print-readably* true
            *print-meta* false
            *print-dup* false]
    (pr-str o)))


(defonce logger (atom (agent nil)))

(def default-config {:serializer #'safe-pr-str})
(defn set-default-config [config]
  (def default-config config))

(def ^:dynamic *config* nil)
(defmacro with-config [config & body]
  `(binding [*config* (merge (or *config* default-config) ~config)]
     ~@body))

(defn config [& ks]
  (get-in *config* ks))

(defmacro with-preamble
  "Specify a PREAMBLE (string or structure) to emit at the beginning of
  every log entry within BODY"
  [preamble & body]
  `(with-config {:preamble ~preamble}
     ~@body))

(defn serialize [x]
  (if (string? x)
    x
    ((or (config :serializer)
         (default-config :serializer)
         #'safe-pr-str) x)))

(defn printer [{:keys [preamble line column]} level ns & args]
  (apply println
         (format-timestamp (System/currentTimeMillis)
                           "yyyy-MM-dd HH:mm:ss")
         (str (-> level name str/upper-case)
              (when preamble
                (str " " (serialize preamble))))
         (str ns ":" line ":" column)
         (map serialize args)))

(def ^:dynamic *level* (atom :info))
(defn set-level!
  "Force the current binding of log level to new level."
  [level]
  (reset! *level* level))
(defmacro with-level [level & body]
  `(binding [*level* (atom ~level)]
     ~@body))

(def level-map
  {:fatal #{:fatal}
   :error #{:error :fatal}
   :warn  #{:warn :error :fatal}
   :info  #{:info :warn :error :fatal}
   :debug #{:debug :info :warn :error :fatal}
   :trace #{:trace :debug :info :warn :error :fatal}})

(defmacro log* [config level & rest]
  `(when (contains? (get level-map @*level*) ~level)
     (printer ~config ~level ~(str *ns*) ~@rest)))

(defmacro log [level & rest]
  `(log* ~(meta &form) ~level ~@rest))

;; fixme: this is not propagating the &form metadata of the correct
;; macro
(defmacro deflogfn [level & [log-fn]]
  `(defmacro ~(symbol (name level)) [& args#]
     (let [level# ~level]
       `(log* (meta '~&form) ~level# ~@args#))))

(defmacro fatal [& args] `(log* ~(meta &form) :fatal ~@args))
(defmacro error [& args] `(log* ~(meta &form) :error ~@args))
(defmacro warn [& args] `(log* ~(meta &form) :warn ~@args))
(defmacro info [& args] `(log* ~(meta &form) :info ~@args))
(defmacro debug [& args] `(log* ~(meta &form) :debug ~@args))
(defmacro trace [& args] `(log* ~(meta &form) :trace ~@args))

(defmacro log-errors [& body]
  `(try
     ~@body
     (catch ExceptionInfo ex#
       (error ex# (ex-data ex#)))
     (catch Throwable ex#
       (error ex#))))

(defmacro log-and-rethrow-errors [& body]
  `(try
     ~@body
     (catch ExceptionInfo ex#
       (error ex# (ex-data ex#))
       (throw ex#))
     (catch Throwable ex#
       (error ex#)
       (throw ex#))))

(defmacro spy
  ([v]
   `(spy :debug ~v))
  ([level v]
   `(let [v# ~v]
      (log* ~(meta &form) ~level '~v v#)
      v#)))


(defn init! []
  (System/setProperties
  (doto (java.util.Properties. (System/getProperties))
    (.put "com.mchange.v2.log.MLog" "com.mchange.v2.log.FallbackMLog")
    (.put "com.mchange.v2.log.FallbackMLog.DEFAULT_CUTOFF_LEVEL" "OFF"))))
