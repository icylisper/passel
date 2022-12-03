(ns passel.db
  (:require
   [clojure.java.jdbc :as jdbc]
   [passel.error :as error]
   [passel.case :as case]
   [passel.sql :as sql])
  (:import
   [com.mchange.v2.c3p0 ComboPooledDataSource]))

(def current (atom nil))
(defn pool [] @current)

(defn- make-pool [{:keys [classname subname
                          subprotocol user password]}]
  (let [cpds (doto (ComboPooledDataSource.)
               (.setDriverClass classname)
               (.setJdbcUrl (str "jdbc:" subprotocol ":" subname))
               (.setUser user)
               (.setPassword password)
               (.setMaxIdleTimeExcessConnections (* 30 60))
               (.setMaxIdleTime (* 3 60 60)))]
    {:datasource cpds}))

(defn- query* [qvec]
  (jdbc/with-db-connection [conn (pool)]
    (jdbc/query conn qvec)))

(defn query [qmap]
  (query* [(sql/sql qmap)]))

(defn insert! [tbl qmap]
  (jdbc/with-db-connection [conn (pool)]
    (->> (case/snake-map qmap)
         (jdbc/insert! conn tbl)
         (first))))

(defn drop-table! [tbl]
  (jdbc/db-do-commands (pool)
                       (jdbc/drop-table-ddl tbl)))

(defn drop-all-tables []
  (jdbc/with-db-connection [conn (pool)]
    (jdbc/query
     conn
     [(str "SELECT * FROM information_schema.tables "
           "WHERE table_schema = 'PUBLIC'")]
     :row-fn (fn [{:keys [table_name]}]
               (println "DROP TABLE" table_name)
               (jdbc/db-do-commands
                conn
                (str "DROP TABLE IF EXISTS " table_name " CASCADE"))))))

(defn list-tables []
  (->> (jdbc/query (pool)
                   ["select * from pg_catalog.pg_tables"])
       (filter #(= (:schemaname %) "public"))
       (map :tablename)))

(defn- schema-migrated? [tbl]
  (error/ignore-errors
   (-> (jdbc/query (pool)
                   [(str "select count(*) from information_schema.tables "
                         (format "where table_name='%s'" (name tbl)))])
       first :count pos?)))

(defn create! [tbl form]
  (when-not (schema-migrated? tbl)
    (jdbc/db-do-commands (pool)
                         (jdbc/create-table-ddl tbl form))))

(defn find1 [tbl where-clause]
  (first
   (query (merge {:select :*
                  :from   tbl
                  :where  where-clause}))))

(defn update! [tbl id form]
  (jdbc/with-db-connection [conn (pool)]
    (jdbc/update! conn tbl form ["id = ?" id])))

(defn delete! [tbl id]
  (jdbc/with-db-connection [conn (pool)]
    (jdbc/delete! conn tbl ["id = ?" id])))

(defn init! [db-spec]
  (->> (make-pool db-spec)
       (reset! current)))
