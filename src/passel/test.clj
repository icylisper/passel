(ns passel.test
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :as test])
  (:import
   [java.io PushbackReader File
    BufferedReader FileReader]))

(defn remove-nil-entries
  "remove keys from the map if their values are nil"
  [m]
  (reduce-kv #(if (and %3 (not (empty? %3)))
                (assoc %1 %2 %3) %1)
             {}
             m))

;; ns

(defn clojure-source-file? [^File file]
  (and (.isFile file)
       (or
         (.endsWith (.getName file) ".clj")
         (.endsWith (.getName file) ".cljc"))))

(defn find-clojure-sources-in-dir [^File dir]
  (sort-by #(.getAbsolutePath ^File %)
           (filter clojure-source-file? (file-seq dir))))

(defn comment? [form]
  (and (list? form) (= 'comment (first form))))


(defn ns-decl? [form]
  (and (list? form) (= 'ns (first form))))

(defn read-ns-decl [^PushbackReader rdr]
  (try
   (loop [] (let [form (doto (read rdr) str)]
              (cond
               (ns-decl? form) form
               (comment? form) (recur)
               :else nil)))
       (catch Exception e nil)))

(defn read-file-ns-decl [^File file]
  (with-open [rdr (PushbackReader. (BufferedReader. (FileReader. file)))]
    (read-ns-decl rdr)))

(defn find-ns-decls-in-dir [^File dir]
  (filter identity (map read-file-ns-decl (find-clojure-sources-in-dir dir))))

(defn find-namespaces-in-dir [^File dir]
  (map second (find-ns-decls-in-dir dir)))

(defn fail? [{:keys [test fail error]}]
  (or (nil? test)
      (zero? test)
      (pos? fail)
      (pos? error)))

(defn ns-filter [{:keys [namespace]}]
  (if namespace
    #(= namespace %)
    (constantly true)))

(defn var-filter
  [{:keys [var include exclude]}]
  (let [test-specific (if var
                        (set (map #(or (resolve %)
                                       (throw (ex-info
                                               (str "Could not resolve var: " %)
                                               {:symbol %})))))
                        (constantly true))
        test-inclusion (if include
                         #((apply some-fn include) (meta %))
                        (constantly true))
        test-exclusion (if exclude
                         #((complement (apply some-fn exclude)) (meta %))
                         (constantly true))]
    #(and (test-specific %)
          (test-inclusion %)
          (test-exclusion %))))

(defn filter-vars!
  [nses filter-fn]
  (doseq [ns nses]
    (doseq [[name var] (ns-publics ns)]
      (when (:test (meta var))
        (when (not (filter-fn var))
          (alter-meta! var #(-> %
                                (assoc ::test (:test %))
                                (dissoc :test))))))))

(defn restore-vars!
  [nses]
  (doseq [ns nses]
    (doseq [[name var] (ns-publics ns)]
      (when (::test (meta var))
        (alter-meta! var #(-> %
                              (assoc :test (::test %))
                              (dissoc ::test)))))))
(defn do-test
  [options]
  (let [dirs (or (:dir options)
                 #{"test"})
        nses (->> dirs
                  (map io/file)
                  (mapcat find-namespaces-in-dir))
        nses (filter (ns-filter options) nses)]
    (println (format "\nRunning tests in %s" dirs))
    (dorun (map require nses))
    (try
      (filter-vars! nses (var-filter options))
      (apply test/run-tests nses)
      (finally
        (restore-vars! nses)))))

(def selectors
  #{:api :scenario :fixme :integration :unit})

(defn run-test [selector]
  (try
    (let [opts {:include (when selector #{selector})
                :exclude (disj selectors selector)}
          result (do-test (remove-nil-entries opts))]
      (println result)
      (if (fail? result)
        (System/exit 1)
        (System/exit 0)))
    (catch Exception e
      (println e)
      (System/exit 1))
    (finally
      (shutdown-agents))))

(defn run-selector [{:keys [selector]
                     :or {selector :unit}}]
  (run-test selector))
