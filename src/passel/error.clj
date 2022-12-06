(ns passel.error
  (:require
   [passel.log :as log])
  (:import
   [java.io StringWriter PrintWriter]
   [clojure.lang ExceptionInfo]))

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

;; error registry
(def ^:private registry (atom nil))

(defn lookup-registry [] @registry)

(defn register!
  "Register new assertion/error identifers and associated metadata,
  e.g.,

  (register-errors!
    {:id-not-found {:tag :precondition-failed
                       :message \"The specified id was not found\"}})

  which may later be asserted as:

  (assert! (find-id id) :id-not-found)"
  [errors]
  (swap! registry merge
         (if (var? errors)
           (var-get errors)
           errors)))

(defmacro assert!
  "Assert the truthiness of an expression, and raise an ExceptionInfo
  exception if that fails. Supports multiple call scenarios:

  (assert! (pos? -10) :not-positive-number)
  (assert! (pos? -10) :not-positive-number {:detail \"bad number\"})
  (assert! (pos? -10) \"Not a positive number\" :not-positive-number)
  (assert! (pos? -10) \"Not a positive number\" :not-positive-number {:detail \"bad number\"})
  "
  {:arglists '([expr id]
               [expr id ex-info-map]
               [expr message ex-tag]
               [expr message ex-tag ex-info-map])}
  ([expr id]
   `(assert! ~expr ~id {}))
  ([expr id-or-message ex-tag-or-info-map]
   (let [id                (when (keyword? id-or-message)
                             id-or-message)
         ex-info-overrides (when (map? ex-tag-or-info-map)
                             ex-tag-or-info-map)]
     `(or ~expr
          (let [ex-info-map# ~(if id
                                `(-> (lookup-registry)
                                     (get ~id)
                                     (merge ~ex-info-overrides)
                                     (merge {:id ~id}))
                                `{:message ~id-or-message
                                  :tag     ~ex-tag-or-info-map})]
            (throw
             (ex-info (or (:message ex-info-map#)
                          (format "Unknown error identifier %s" ~id-or-message))
                      ex-info-map#))))))
  ([expr message ex-tag ex-info-map]
   `(or ~expr
        (do
          (when-let [id# (:id ~ex-info-map)]
            (log/debug ~(merge {:log-type :error} ex-info-map)))
          (throw
           (ex-info ~message
                    ~(merge {:message message
                             :type    :assertion-failed
                             :tag     ex-tag}
                            ex-info-map)))))))

(defmacro assert-empty!
  "Assert that `thing` is empty, additional `args` are passed to
  `assert!`"
  [thing & args]
  `(assert! (empty? ~thing) ~@args))

(defmacro assert-not-empty!
  "Assert that `thing` is not-empty, additional `args` are passed to
  `assert!`"
  [thing & args]
  `(assert! (not (empty? ~thing)) ~@args))

(defn catch-assertion-value
  "Evaluate function `f` catching any ExceptionInfo conditions, and
  apply `k-fn` on the exception's `ex-data`."
  [f k-fn]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo ex
      (k-fn (-> ex ex-data)))))

(defmacro assertion-get
  "Evaluate `body`, and apply `ifn` to any assertions that are thrown,
  e.g.,

  (do
    (register-errors! {:bad-nil {:tag :bad-request}})
    (= :bad-request (assertion-get :tag (assert! nil :bad-nil))))"
  [ifn & body]
  `(catch-assertion-value (fn [] ~@body) ~ifn))

(defmacro assertion-message
  "Evaluate `body`, and return assertion message of any assertions that may
  be thrown."
  [& body]
  `(assertion-get :message ~@body))
